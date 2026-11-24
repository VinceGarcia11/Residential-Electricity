# ADS-506 Final Project: U.S. Residential Electricity Demand

Team 9: Mason Delan, Michael Rosalia, Vince Garcia

This project forecasts monthly U.S. residential electricity sales. Home electricity use peaks in the summer and again in the winter, and the goal is a 12-month forecast of that demand.

## Dataset

Residential sales to ultimate customers, from the EIA Monthly Energy Review, Table 7.6 (series ESRCPUS). Values are million kilowatthours. The series is monthly, January 1973 through July 2026.

Source: [EIA Monthly Energy Review, Table 7.6](https://www.eia.gov/totalenergy/data/browser/?tbl=T07.06)

A copy of the monthly residential series is in `data/residential_electricity.csv`.

| Column | Description |
| --- | --- |
| `date` | First day of the month |
| `residential_million_kwh` | Residential electricity sales, million kWh |

## Repository

```
data/residential_electricity.csv
.gitignore
.README.md
app.R
renv.lock
.posit/publish/deployments/deployment-LP72.toml
.posit/publish/deployments/deployment-Residential Electricity Foreca-7SSC.toml

```

## Acknowledgments & AI Use Disclosure

Project Dataset & Team Code: Uses the U.S. Residential Electricity dataset (residential_electricity.csv) and the ARIMA(0,0,1)(0,1,1)[12] with drift specification developed in our team's project notebook.

Posit Assistant (Claude Sonnet 5.5 in Positron): Generated the initial bslib page_navbar UI boilerplate, reactive pipeline scaffolding (split, fit_train, fc_test, fit_full), and assisted with Posit Connect Cloud deployment.

Gemini: Assisted with refactoring app.R to add the interactive holdout window slider (input$holdout), combining training and validation metrics in the accuracy table (bind_rows()), and refining phrasing and spell-checking. Also assisted with phrasing of AI contributions. 

Author Contributions: I integrated my own TSLM(log(kwh) ~ trend() + season()) model, customized the code structure to match my coding conventions, verified all reactive logic and diagnostic outputs, and authored the Part II data story.
