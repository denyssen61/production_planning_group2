
regression_fit <- function(d, degree = 1, h = 4) {
  n <- length(d)
  t <- seq_len(n)
  f <- if (degree == 1) stats::lm(d ~ t) else stats::lm(d ~ t + I(t^2))
  new <- data.frame(t = n + seq_len(h))
  pr <- stats::predict(f, newdata = new, interval = "prediction", level = 0.95)
  list(model = f, fit = as.numeric(stats::fitted(f)),
       fut = as.numeric(pr[, "fit"]),
       lower = as.numeric(pr[, "lwr"]), upper = as.numeric(pr[, "upr"]))
}



regression_forecast_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("degree"), "Trend",
                   choices = c("Linear: a + b t" = 1,
                               "Quadratic: a + b t + c t^2" = 2)),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      h4("Regression results"),
      tableOutput(ns("coef_table")),
      verbatimTextOutput(ns("fit_text"))
    )
  )
}



regression_forecast_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Trend")
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      deg <- as.integer(input$degree)
      r <- regression_fit(d, deg, input$h)
      cf <- stats::coef(r$model)
      eq <- if (deg == 1)
        sprintf("D = %.3f + %.3f t", cf[1], cf[2])
      else sprintf("D = %.3f + %.3f t + %.4f t^2", cf[1], cf[2], cf[3])
      c(list(d = d, from = 1, title = paste0("Trend regression:  ", eq)), r)
    })
    
    fc_render_outputs(output, result)
    
    output$coef_table <- renderTable({
      s <- summary(result()$model)$coefficients
      data.frame("Term" = c("a (intercept)", "b (t)", "c (t^2)")[seq_len(nrow(s))],
                 "Estimate" = round(s[, 1], 4), "Std. error" = round(s[, 2], 4),
                 "t value" = round(s[, 3], 3), "p value" = round(s[, 4], 4),
                 check.names = FALSE)
    })
    
    output$fit_text <- renderText({
      s <- summary(result()$model)
      paste0("R-squared = ", round(s$r.squared, 4),
             "    Adjusted R-squared = ", round(s$adj.r.squared, 4),
             "    Residual std. error = ", round(s$sigma, 3))
    })
  })
}