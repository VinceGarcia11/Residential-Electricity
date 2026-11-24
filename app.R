library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(ggtime)

# Fixed split shared with the team's ARIMA notebook:
# train Jan 2010 - Jul 2025 (187 months), test Aug 2025 - Jul 2026 (12 months)
HOLDOUT <- 12

raw <- read.csv("data/residential_electricity.csv") |>
  mutate(month = yearmonth(as.Date(date))) |>
  select(month, kwh = residential_million_kwh) |>
  as_tsibble(index = month) |>
  filter_index("2010 Jan" ~ "2026 Jul")

fit_models <- function(x) {
  x |>
    model(
      ARIMA = ARIMA(kwh ~ 1 + pdq(0, 0, 1) + PDQ(0, 1, 1)),
      TSLM  = TSLM(log(kwh) ~ trend() + season()),
      ETS   = ETS(log(kwh))
    )
}

ui <- page_navbar(
  title = "U.S. Residential Electricity: ARIMA vs TSLM vs ETS",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    sliderInput("horizon", "Future forecast horizon (months)", 6, 36, 12, step = 6),
    helpText("Training: Jan 2010 - Jul 2025. Test: Aug 2025 - Jul 2026 (12 months).",
             "ARIMA is ARIMA(0,0,1)(0,1,1)[12] with drift on the raw series;",
             "TSLM and ETS are fit on log(kWh) and back-transformed.")
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
      card(card_header("Accuracy on test window"), tableOutput("t_acc")),
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
  dat <- reactive(raw)

  split <- reactive({
    x <- dat(); n <- nrow(x); h <- HOLDOUT
    list(train = slice_head(x, n = n - h), test = slice_tail(x, n = h))
  })

  fit_train <- reactive({
    withProgress(message = "Fitting models", fit_models(split()$train))
  })

  fc_test <- reactive(forecast(fit_train(), h = HOLDOUT))

  acc <- reactive({
    accuracy(fc_test(), dat()) |>
      select(Model = .model, RMSE, MAE, MAPE) |>
      arrange(RMSE)
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
      Annual totals grew from %s (%d) to %s (%d), roughly <b>%.1f%% per year</b> on average,
      but growth has clearly slowed in recent decades. The all-time monthly peak is
      <b>%s</b> (%s million kWh). Seasonal swings are large relative to the trend,
      so any model must capture seasonality first.</p>",
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
      The seasonal amplitude is multiplicative (it grows with the level), which is
      why the models use a log transform. Seasonal strength from the STL
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
    HTML("<p><b>ARIMA</b> is the team's manual ARIMA(0,0,1)(0,1,1)[12] with drift, chosen
    from 12-month forecasts inside the training sample (not the test year).
    <b>TSLM</b> is a regression on a deterministic trend and 11 seasonal dummies: simple
    and interpretable, but it assumes the trend and seasonal pattern are fixed over time.
    <b>ETS</b> uses exponentially smoothed level, trend and seasonal states that adapt
    to recent data. Information criteria such as AICc are not comparable across these model
    classes (different likelihoods and differencing), so test-set accuracy is used instead.</p>")
  })

  output$p_test <- renderPlot({
    s <- split()
    autoplot(fc_test(), filter(dat(), month >= min(s$test$month) - 48), level = NULL) +
      labs(x = NULL, y = "Million kWh", colour = "Model")
  })

  output$t_acc <- renderTable(acc(), digits = 2)

  output$txt_acc <- renderUI({
    a <- acc()
    HTML(sprintf(
      "<p>On the last %d months, <b>%s</b> had the lowest RMSE (%s million kWh,
      MAPE %.1f%%), and <b>%s</b> the highest RMSE (%s). A single holdout window is a noisy
      basis for ranking: differences of a few tenths of a percent in MAPE may not
      persist in other windows. The test window is fixed to match the team notebook.</p>",
      HOLDOUT, a$Model[1], format(round(a$RMSE[1]), big.mark = ","), a$MAPE[1],
      a$Model[nrow(a)], format(round(a$RMSE[nrow(a)]), big.mark = ",")))
  })

  output$p_res <- renderPlot({
    augment(fit_train()) |>
      ggplot(aes(month, .innov)) + geom_line() +
      facet_wrap(~.model, ncol = 1) + labs(x = NULL, y = "Innovation residual (log)")
  })

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
      "<p>Residuals that still show autocorrelation mean a model leaves structure
      unexplained. At the 5%% level, the Ljung-Box test flags: <b>%s</b>. Even when
      residuals are autocorrelated, point forecasts can be adequate, but prediction
      intervals will be too narrow. Note the test uses in-sample residuals and the
      p-values are not adjusted for model degrees of freedom.</p>",
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
                " months:</p><ul>", items, "</ul>
      <p>Where the models agree, confidence is higher; where they diverge or intervals widen,
      treat the forecast as uncertain. TSLM extrapolates a fixed trend, so it is the most
      sensitive to the training window.</p>"))
  })
}

shinyApp(ui, server)
