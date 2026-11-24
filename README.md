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
data/.gitignore
data/.README.md
data/app.R
data/renv.lock
.posit/publish/deployments/deployment-LP72.toml
.posit/publish/deployments/deployment-Residential Electricity Foreca-7SSC.toml

Note about AI Use:
Deployments were handled within Positron, using Claude Sonnet 5.5 within Posit Assistant. Additionally, the code within app.R, which handled the front-end and back end logic, was also largely assisted by Claude Sonnet 5.5 and Gemini Neon 4. Some phrasing and spell-checking was also assisted with Gemini Neon 4.  
```
