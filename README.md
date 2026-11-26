# ADS 506 Week 5: Australian Wine Forecasting Explorer

A Shiny app for comparing TSLM, ETS and ARIMA forecasts of monthly
Australian wine sales by varietal, 1980 to 1994.

## Running it

Keep `app.R` and `AustralianWines.csv` in the same folder, then:

```r
shiny::runApp()
```

Requires: shiny, bslib, tidyverse, fpp3.

## Deploying

**Posit Connect Cloud.** Push this folder to a public GitHub repo. In
the project, run `rsconnect::writeManifest()` and commit the resulting
`manifest.json`. Then at connect.posit.cloud, choose Publish, pick a
Shiny app, select the repo and the main branch.

**Posit Cloud.** Create a new RStudio project, upload `app.R` and the
CSV, install the packages, run the app, then share the project with
instructor access.

## Acknowledgment

Claude was used to draft portions of this application's boilerplate.
All modeling choices, the data treatment and the narrative are my own.
