
centered_moving_average <- function(d, N) {
  n <- length(d)
  cma <- rep(NA_real_, n)
  if (N %% 2 == 1) {
    k <- (N - 1) / 2
    for (t in (k + 1):(n - k)) cma[t] <- mean(d[(t - k):(t + k)])
  } else {
    k <- N / 2
    for (t in (k + 1):(n - k))
      cma[t] <- (0.5 * d[t - k] + sum(d[(t - k + 1):(t + k - 1)]) +
                   0.5 * d[t + k]) / N
  }
  cma
}

decomposition_fit <- function(d, N, trend = "regression", h = N) {
  n <- length(d)
  t <- seq_len(n)
  pos <- (t - 1) %% N + 1
  cma <- centered_moving_average(d, N)
  ratio <- d / cma
  cf <- as.numeric(tapply(ratio, pos, mean, na.rm = TRUE))
  cf <- cf * N / sum(cf)
  deseason <- d / cf[pos]
  if (trend == "regression") {
    fit_lm <- stats::lm(deseason ~ t)
    a <- unname(coef(fit_lm)[1]); b <- unname(coef(fit_lm)[2])
  } else {
    a <- mean(deseason); b <- 0
  }
  t_fut <- n + seq_len(h)
  pos_fut <- (t_fut - 1) %% N + 1
  list(cma = cma, ratio = ratio, factors = cf, pos = pos, deseason = deseason,
       a = a, b = b,
       fit = (a + b * t) * cf[pos],
       fut = (a + b * t_fut) * cf[pos_fut])
}



seasonal_decomposition_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_season_input(ns),
      radioButtons(ns("trend"), "Trend of the deseasonalized series",
                   choices = c("Regression line (a + b t)" = "regression",
                               "No trend (mean)" = "none")),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      h4("Seasonal factors"),
      tableOutput(ns("factor_table")),
      verbatimTextOutput(ns("trend_text")),
      plotOutput(ns("decomp_plot"), height = "300px"),
      h4("Decomposition table"),
      tableOutput(ns("decomp_table"))
    )
  )
}



seasonal_decomposition_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Seasonal")
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      N <- fc_season_value(input, d)
      s <- decomposition_fit(d, N, input$trend, input$h)
      list(d = d, fit = s$fit, fut = s$fut, from = 1, s = s, N = N,
           title = paste0("Seasonal decomposition, N = ", N))
    })
    
    fc_render_outputs(output, result)
    
    output$factor_table <- renderTable({
      r <- result()
      data.frame("Season position" = seq_len(r$N),
                 "Seasonal factor" = round(r$s$factors, 4), check.names = FALSE)
    })
    
    output$trend_text <- renderText({
      s <- result()$s
      paste0("Deseasonalized series:  ", round(s$a, 3), " + ",
             round(s$b, 3), " t")
    })
    
    output$decomp_plot <- renderPlot({
      r <- result(); s <- r$s; n <- length(r$d)
      plot(seq_len(n), r$d, type = "o", pch = 19, col = "grey60",
           ylim = range(c(r$d, s$deseason), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = "Series, centered MA, deseasonalized series and trend")
      lines(seq_len(n), s$cma, lwd = 2, col = "#4E8F4A")
      lines(seq_len(n), s$deseason, type = "o", pch = 1, col = FC_COLORS["fit"])
      abline(a = s$a, b = s$b, lty = 2, lwd = 2, col = FC_COLORS["future"])
      legend("topleft", bty = "n",
             legend = c("Actual", "Centered MA", "Deseasonalized", "Trend"),
             col = c("grey60", "#4E8F4A", FC_COLORS["fit"], FC_COLORS["future"]),
             lty = c(1, 1, 1, 2), pch = c(19, NA, 1, NA))
    })
    
    output$decomp_table <- renderTable({
      r <- result(); s <- r$s
      data.frame("Period" = seq_along(r$d), "Season" = s$pos,
                 "Demand" = r$d, "Centered MA" = round(s$cma, 3),
                 "Ratio D/CMA" = round(s$ratio, 4),
                 "Factor" = round(s$factors[s$pos], 4),
                 "Deseasonalized" = round(s$deseason, 3),
                 check.names = FALSE)
    }, na = "")
  })
}