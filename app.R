library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(ggtime)

#reading the data
raw <- read.csv("data/residential_electricity.csv") |>
  mutate(month = yearmonth(as.Date(date))) |>
  select(month, kwh = residential_million_kwh) |>
  as_tsibble(index = month)

DATA_START <- as.Date(min(raw$month))
DATA_END <- as.Date(max(raw$month))
DEFAULT_START <- as.Date("2010-01-01")
# ARIMA(1,0,0)(0,1,1)[12] cannot be estimated on fewer than 36 months
MIN_TRAIN <- 36

# Latest allowed start so that at least MIN_TRAIN months remain after the holdout
latest_start <- function(holdout) {
  DATA_END %m-% months(MIN_TRAIN + holdout - 1)
}
#fitting the models
fit_models <- function(x) {
  x |>
    model(
      #redefining the ARIMA model to reflect what Auto-ARIMA chose in the EDA assignment
      ARIMA = ARIMA(kwh ~ 1 + pdq(1, 0, 0) + PDQ(0, 1, 1)),
      #Using an auto-selected TSLM model
      TSLM  = TSLM(log(kwh) ~ trend() + season()),
      #Using the manual ETS model
      ETS   = ETS(log(kwh) ~ error("M") + trend("N") + season("M"))
    )
}

ui <- page_navbar(
  title = "U.S. Residential Electricity: ARIMA vs TSLM vs ETS",
  theme = bs_theme(version = 5),
 sidebar = sidebar(
    sliderInput("start", "Data start (month)",
                min = DATA_START + 14, max = latest_start(12) + 14,
                value = DEFAULT_START + 14, timeFormat = "%b %Y",
                step = 365.25 / 12),
    sliderInput("holdout", "Validation holdout (months)", min = 6, max = 36, value = 12, step = 6),
    sliderInput("horizon", "Future forecast horizon (months)", 6, 36, 12, step = 6),
    helpText("Default holdout (12 months): Training Jan 2010 - Jul 2025, Test Aug 2025 - Jul 2026.",
             "ARIMA is the auto-selected ARIMA(1,0,0)(0,1,1)[12] with drift on the raw series;",
             "TSLM is the auto-selected Time Series Linear Model with trend and seasonality on the log-transformed series and back-transformed automatically;",
             "ETS is the Exponential Smoothing model fit on the log-transformed series, back-transformed automatically, and given multiplicative seasonality and error, with no trend component.")
  ),
  nav_panel("1. The series",
    layout_columns(col_widths = c(12, 12),
      card(card_header("Monthly residential sales (million kWh)"),
           plotOutput("p_series", height = 320)),
      card(uiOutput("txt_series")))),
  nav_panel("2. Seasonality & trend",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Seasonal subseries"), plotOutput("p_subseries")),
      card(card_header("STL decomposition"), plotOutput("p_stl"))),
    card(uiOutput("txt_season"))),
  nav_panel("3. The models",
    card(card_header("Fitted model specifications"), tableOutput("t_specs")),
    card(uiOutput("txt_models"))),
  nav_panel("4. Test-set performance",
    layout_columns(col_widths = c(12, 7, 5),
      card(card_header("Forecasts vs. held-out actuals"),
           plotOutput("p_test", height = 380)),
      card(card_header("Training vs. validation accuracy"), tableOutput("t_acc")),
      #Using Gemini 4 Neon to modify this panel
      card(uiOutput("txt_acc")))),
  nav_panel("5. Residual diagnostics",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Residuals over time (training fit)"), plotOutput("p_res")),
      card(card_header("Residual ACF"), plotOutput("p_acf"))),
    card(card_header("Ljung-Box test (24 lags)"), tableOutput("t_lb")),
    card(uiOutput("txt_res"))),
  nav_panel("6. Forecast",
    layout_columns(col_widths = c(12, 12),
      card(card_header("Refit on all data: forecast with 80% intervals"),
           plotOutput("p_fc", height = 400)),
      card(uiOutput("txt_fc"))))
)

server <- function(input, output, session) {
  # Keep the start slider's upper limit tied to the holdout
  observeEvent(input$holdout, {
    mx <- latest_start(input$holdout) + 14
    updateSliderInput(session, "start", max = mx,
                      value = min(as.Date(input$start), mx))
  })

  dat <- reactive({
    req(input$start)
    filter(raw, month >= yearmonth(input$start))
  })

split <- reactive({
    x <- dat(); n <- nrow(x); h <- input$holdout
    list(train = slice_head(x, n = n - h), test = slice_tail(x, n = h))
  })

  fit_train <- reactive({
    withProgress(message = "Fitting models", fit_models(split()$train))
  })

  fc_test <- reactive(forecast(fit_train(), h = input$holdout))

acc <- reactive({
    bind_rows(
      accuracy(fit_train()),
      accuracy(fc_test(), dat())
    ) |> 
      #Using Gemini 4 Neon to combine the test and validation accuracies into one window
      mutate(Window = ifelse(.type == "Test", "Validation", .type)) |>
      select(Model = .model, Window, RMSE, MAE, MAPE) |>
      arrange(desc(Window), RMSE)
  })

  fit_full <- reactive({
    withProgress(message = "Refitting on all data", fit_models(dat()))
  })

  fc_full <- reactive(forecast(fit_full(), h = input$horizon))

  output$p_series <- renderPlot({
    ggplot(dat(), aes(month, kwh)) + geom_line() +
      labs(x = NULL, y = "Million kWh")
  })

  output$txt_series <- renderUI({
    x <- dat()
    yr <- x |> as_tibble() |> group_by(y = year(month)) |>
      summarise(t = sum(kwh), n = n()) |> filter(n == 12)
    g <- (last(yr$t) / first(yr$t))^(1 / (nrow(yr) - 1)) - 1
    pk <- x |> as_tibble() |> slice_max(kwh, n = 1)
    HTML(sprintf(
      "<p>The series runs from <b>%s</b> to <b>%s</b> (%d months, no missing values).
      Annual total sales grew from %s (%d) to %s (%d), roughly <b>%.1f%% per year</b> on average. The all-time monthly peak is
      <b>%s</b> (%s million kWh). Seasonal swings are large relative to the trend, suggesting that the data is weather-driven.
      All models include a seasonal component. </p>",
      format(min(x$month)), format(max(x$month)), nrow(x),
      format(round(first(yr$t)), big.mark = ","), first(yr$y),
      format(round(last(yr$t)), big.mark = ","), last(yr$y), 100 * g,
      format(pk$month), format(round(pk$kwh), big.mark = ",")))
  })

  output$p_subseries <- renderPlot({
    gg_subseries(dat(), kwh) + labs(x = NULL, y = "Million kWh")
  })

  output$p_stl <- renderPlot({
    dat() |> model(STL(log(kwh) ~ season(window = "periodic"))) |>
      components() |> autoplot()
  })

  output$txt_season <- renderUI({
    comp <- dat() |> model(STL(log(kwh) ~ season(window = "periodic"))) |> components()
    s <- comp |> as_tibble() |> group_by(m = month(month, label = TRUE)) |>
      summarise(s = mean(season_year))
    HTML(sprintf(
      "<p>Demand has two peaks: summer (cooling) and winter (heating), with the
      highest average seasonal effect in <b>%s</b> and the lowest in <b>%s</b>.
      The seasonal amplitude is multiplicative, which is
      why the models use a log transform. 12-month seasonality from the STL
      decomposition is the main source of predictability here.</p>",
      s$m[which.max(s$s)], s$m[which.min(s$s)]))
  })

  output$t_specs <- renderTable({
    f <- fit_full()
    tibble(Model = c("ARIMA", "TSLM", "ETS"),
           Specification = c(format(f$ARIMA[[1]]),
                             "Linear trend + monthly seasonal dummies (log scale)",
                             format(f$ETS[[1]])))
  })

  output$txt_models <- renderUI({
    HTML("<p><b>ARIMA</b> is the team's auto-selected ARIMA(1,0,0)(0,1,1)[12] with drift.
    <b>TSLM</b> is a linear regression that assumes a constant trend and consistent 12-month seasonality.
    <b>ETS</b> uses exponentially smoothed leveling, trend and seasonal states that adapt
    to recent data. Accuracy across all three models is measured with RMSE, MAE and MAPE.</p>")
  })

  output$p_test <- renderPlot({
    autoplot(fc_test(), filter(dat(), year(month) >= 2024), level = NULL) +
      labs(x = NULL, y = "Million kWh", colour = "Model")
  })

  output$t_acc <- renderTable(acc(), digits = 2)

output$txt_acc <- renderUI({
    a <- acc() |> filter(Window == "Validation")
    HTML(sprintf(
      "<p>On the last %d months, <b>%s</b> had the lowest validation RMSE (%s million kWh,
      MAPE %.1f%%), and <b>%s</b> the highest RMSE (%s).</p>",
      input$holdout, a$Model[1], format(round(a$RMSE[1]), big.mark = ","), a$MAPE[1],
      a$Model[nrow(a)], format(round(a$RMSE[nrow(a)]), big.mark = ",")))
  }) #Using Gemini 4 Neon to modify the output for model accuracy

output$p_res <- renderPlot({
  augment(fit_train()) |>
    ggplot(aes(month, .innov)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~.model, ncol = 1, scales = "free_y") +
    labs(x = NULL, y = "Innovation residual")
})
  #Gemini 4 Neon replaces the original residual plot in order to keep the y axis free

  output$p_acf <- renderPlot({
    augment(fit_train()) |> ACF(.innov) |> autoplot()
  })

  lb <- reactive({
    augment(fit_train()) |>
      features(.innov, ljung_box, lag = 24) |>
      transmute(Model = .model, `Q statistic` = lb_stat, `p-value` = lb_pvalue)
  })
  output$t_lb <- renderTable(lb(), digits = 4)

  output$txt_res <- renderUI({
    l <- lb()
    bad <- l$Model[l$`p-value` < 0.05]
    HTML(sprintf(
     "<p>Residuals represent leftover variance that the model could not capture. If the 
  p-value of the residuals is less than 0.05, we reject the null hypothesis that the 
  residuals are white noise. At the 5%% level, the models: <b>%s</b> fail the 
  Ljung-Box test due to remaining autocorrelation.</p>

  <p>Under the default holdout, the Residual ACF plots confirm this: TSLM shows 
  strong autocorrelation with significant spikes outside the confidence bounds 
  at lags 1 and 2, and ETS has a slight spike outside the bounds at 
  lag 2. In contrast, the auto-selected ARIMA model keeps all lags within the 
  confidence boundaries, indicating no significant remaining autocorrelation.</p> ",
      if (length(bad)) paste(bad, collapse = ", ") else "none"))
  })

  output$p_fc <- renderPlot({
    autoplot(fc_full(), filter(dat(), year(month) >= max(year(month)) - 6), level = 80) +
      facet_wrap(~.model, ncol = 1) + labs(x = NULL, y = "Million kWh") +
      theme(legend.position = "none")
  })

  output$txt_fc <- renderUI({
    f <- fc_full() |> as_tibble() |>
      group_by(.model) |> slice_max(.mean, n = 1) |> ungroup()
    items <- paste0("<li><b>", f$.model, "</b>: peak ", format(round(f$.mean), big.mark = ","),
                    " in ", format(f$month), "</li>", collapse = "")
    HTML(paste0("<p>Peak forecast month per model over the next ", input$horizon,
                " months:</p><ul>", items, "</ul>"))
  })
}

shinyApp(ui, server)
