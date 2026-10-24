
seasonal_factors_fit <- function(d, N, h = N) {
  n <- length(d)
  pos <- (seq_len(n) - 1) %% N + 1
  m <- mean(d)
  ratio <- d / m
  cf <- tapply(ratio, pos, mean)
  cf <- cf * N / sum(cf)
  fut_pos <- (n + seq_len(h) - 1) %% N + 1
  list(mean = m, factors = as.numeric(cf), ratio = ratio, pos = pos,
       fit = m * cf[pos], fut = m * cf[fut_pos])
}



seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_season_input(ns),
      fc_horizon_input(ns),
      helpText("Season length found from the ACF = the lag (after removing",
               "a linear trend) with the largest autocorrelation.")
    ),
    fc_main_panel(
      ns,
      h4("Seasonal factors"),
      tableOutput(ns("factor_table")),
      plotOutput(ns("acf_plot"), height = "230px")
    )
  )
}



seasonal_factors_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Seasonal")
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      N <- fc_season_value(input, d)
      s <- seasonal_factors_fit(d, N, input$h)
      list(d = d, fit = as.numeric(s$fit), fut = as.numeric(s$fut), from = 1,
           s = s, N = N,
           title = paste0("Seasonal factors (quick and dirty), N = ", N))
    })
    
    fc_render_outputs(output, result)
    
    output$factor_table <- renderTable({
      r <- result()
      data.frame("Season position" = seq_len(r$N),
                 "Seasonal factor" = round(r$s$factors, 4),
                 "Forecast (mean x factor)" = round(r$s$mean * r$s$factors, 2),
                 check.names = FALSE)
    }, caption = "Sample mean of all data is used as the base level.")
    
    output$acf_plot <- renderPlot({
      stats::acf(demand(), lag.max = floor(length(demand()) / 2),
                 main = "ACF of the series")
    })
  })
}