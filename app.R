# ADS-506 Week 5: Australian Wine Forecasting Explorer
#
# Shiny app for comparing TSLM, ETS and ARIMA forecasts of monthly
# Australian wine sales by varietal. Data is read with a project
# relative path so the app runs unchanged wherever it is deployed.

library(shiny)
library(bslib)
library(tidyverse)
library(fpp3)

# ---------------------------------------------------------------- data
# Rose carries "*" for two months in 1994; those are read as missing and
# carried forward, matching the treatment used earlier in the course.
wines <- read_csv("AustralianWines.csv", na = "*",
                  col_types = cols(.default = col_number(),
                                   Month = col_character()),
                  show_col_types = FALSE) |>
  rename_with(str_trim) |>
  fill(everything(), .direction = "down") |>
  mutate(Month = yearmonth(mdy(str_replace(Month, "-", "-01-")))) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

VARIETALS <- sort(unique(wines$Varietal))
DATE_MIN  <- as.Date(min(wines$Month))
DATE_MAX  <- as.Date(max(wines$Month))

# ------------------------------------------------------------------ ui
ui <- page_sidebar(
  title = "Australian Wine Forecasting Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,
    selectInput("varietals", "Varietals",
                choices = VARIETALS,
                selected = c("sparkling", "Dry white"),
                multiple = TRUE),
    helpText("Pick one or more. Two are selected by default."),

    sliderInput("range", "Date range",
                min = DATE_MIN, max = DATE_MAX,
                value = c(DATE_MIN, DATE_MAX),
                timeFormat = "%b %Y"),

    sliderInput("split", "Training cutoff",
                min = DATE_MIN, max = DATE_MAX,
                value = as.Date("1992-12-01"),
                timeFormat = "%b %Y"),
    helpText("Everything up to this date trains the models; what",
             "follows is held back to score them."),

    sliderInput("h", "Forecast horizon (months)",
                min = 3, max = 36, value = 24, step = 3),

    checkboxInput("logtf", "Fit on the log scale", value = FALSE),
    helpText("Use when the seasonal swing grows or shrinks with the",
             "level. This stabilizes the variance."),

    actionButton("fit", "Fit models", class = "btn-primary",
                 icon = icon("play")),
    helpText("Models refit only when this is pressed, so the other",
             "controls stay responsive.")
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      card(card_header("Sales over time"), plotOutput("plot_series",
                                                      height = "330px")),
      card(card_header("Seasonal pattern by month"),
           plotOutput("plot_season", height = "300px"))
    ),
    nav_panel(
      "Models",
      card(card_header("What each model chose"),
           tableOutput("tbl_specs")),
      card(card_header("Accuracy by window"),
           tableOutput("tbl_accuracy"))
    ),
    nav_panel(
      "Forecasts",
      card(card_header("Comparative forecasts with 80% and 95% intervals"),
           plotOutput("plot_forecast", height = "420px")),
      card(card_header("Forecast values"), tableOutput("tbl_forecast"))
    ),
    nav_panel(
      "About",
      card(
        card_header("About this app"),
        htmlOutput("about")
      )
    )
  )
)

# -------------------------------------------------------------- server
server <- function(input, output, session) {

  selected <- reactive({
    req(input$varietals)
    wines |>
      filter(Varietal %in% input$varietals,
             Month >= yearmonth(input$range[1]),
             Month <= yearmonth(input$range[2]))
  })

  training <- reactive({
    selected() |> filter(Month <= yearmonth(input$split))
  })

  # Fitting is the slow step, so it runs only on the button press.
  fitted_models <- eventReactive(input$fit, {
    train <- training()
    validate(need(nrow(train) >= 36,
                  "Need at least three years of training data."))

    withProgress(message = "Fitting models", value = 0, {
      incProgress(0.2, detail = "TSLM and ETS")
      out <- if (isTRUE(input$logtf)) {
        train |> model(
          TSLM  = TSLM(log(Sales) ~ trend() + season()),
          ETS   = ETS(log(Sales)),
          ARIMA = ARIMA(log(Sales), stepwise = TRUE,
                        approximation = TRUE,
                        order_constraint = p + q + P + Q <= 5)
        )
      } else {
        train |> model(
          TSLM  = TSLM(Sales ~ trend() + season()),
          ETS   = ETS(Sales),
          ARIMA = ARIMA(Sales, stepwise = TRUE, approximation = TRUE,
                        order_constraint = p + q + P + Q <= 5)
        )
      }
      incProgress(0.8, detail = "ARIMA search")
      out
    })
  }, ignoreNULL = FALSE)

  forecasts <- reactive({
    fitted_models() |> forecast(h = input$h)
  })

  # ---------------------------------------------------------- overview
  output$plot_series <- renderPlot({
    cutoff <- as.Date(input$split)
    selected() |>
      autoplot(Sales) +
      geom_vline(xintercept = cutoff, linetype = "dashed",
                 colour = "firebrick") +
      labs(x = NULL, y = "Sales (thousands of litres)",
           colour = "Varietal",
           caption = "Dashed line marks the training cutoff") +
      theme_minimal(base_size = 13)
  })

  output$plot_season <- renderPlot({
    selected() |>
      gg_season(Sales) +
      labs(x = NULL, y = "Sales (thousands of litres)") +
      theme_minimal(base_size = 13)
  })

  # ------------------------------------------------------------ models
  output$tbl_specs <- renderTable({
    fitted_models() |>
      as_tibble() |>
      mutate(across(-Varietal, ~ as.character(format(.x)))) |>
      pivot_longer(-Varietal, names_to = "Model",
                   values_to = "Specification") |>
      pivot_wider(names_from = Model, values_from = Specification)
  }, striped = TRUE, width = "100%")

  output$tbl_accuracy <- renderTable({
    fits <- fitted_models()

    train_acc <- fits |>
      accuracy() |>
      mutate(Window = "Training")

    valid <- selected() |> filter(Month > yearmonth(input$split))
    h_valid <- valid |> as_tibble() |> distinct(Month) |> nrow()

    valid_acc <- if (h_valid > 0) {
      fits |>
        forecast(h = h_valid) |>
        accuracy(selected()) |>
        mutate(Window = "Validation")
    } else {
      NULL
    }

    bind_rows(train_acc, valid_acc) |>
      select(Window, Varietal, Model = .model, RMSE, MAE, MAPE) |>
      arrange(Window, Varietal, RMSE)
  }, striped = TRUE, digits = 1, width = "100%")

  # --------------------------------------------------------- forecasts
  output$plot_forecast <- renderPlot({
    forecasts() |>
      autoplot(selected(), level = c(80, 95)) +
      facet_grid(Varietal ~ .model, scales = "free_y") +
      labs(x = NULL, y = "Sales (thousands of litres)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$tbl_forecast <- renderTable({
    forecasts() |>
      as_tibble() |>
      select(Varietal, Model = .model, Month, .mean) |>
      mutate(Month = format(as.Date(Month), "%b %Y")) |>
      pivot_wider(names_from = Model, values_from = .mean) |>
      head(36)
  }, striped = TRUE, digits = 0, width = "100%")

  # ------------------------------------------------------------- about
  output$about <- renderUI({
    HTML(paste0(
      "<p><b>Data.</b> Monthly Australian wine sales by varietal, ",
      "January 1980 to December 1994, in thousands of litres. Two ",
      "missing Rose observations in 1994 are carried forward from the ",
      "preceding month.</p>",
      "<p><b>Models.</b> Three families are fitted per selected ",
      "varietal, all auto-specified: a time series linear model with ",
      "trend and seasonal terms, exponential smoothing via ETS(), and ",
      "ARIMA via ARIMA(). The ARIMA search is capped at a combined ",
      "order of five so the app stays responsive.</p>",
      "<p><b>Reproducing a figure.</b> Set the varietals, date range ",
      "and training cutoff in the sidebar, press Fit models, then open ",
      "the tab you want. Every table and plot responds to the same ",
      "controls.</p>",
      "<p><b>The log scale option.</b> Several varietals have a ",
      "seasonal swing that changes with the level. Fitting on the log ",
      "scale turns that proportional pattern into an additive one, ",
      "which usually improves validation accuracy for those series. ",
      "Forecasts are back-transformed before scoring, so the metrics ",
      "stay comparable either way.</p>",
      "<p><b>Acknowledgment.</b> Claude was used to draft portions of ",
      "this application's boilerplate. All modeling choices, the data ",
      "treatment and the narrative are my own, and I have reviewed and ",
      "understand the code.</p>"
    ))
  })
}

shinyApp(ui, server)
