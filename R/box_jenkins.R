
bj_fit <- function(d, p, dd, q, include_mean = TRUE) {
  tryCatch(
    suppressWarnings(stats::arima(d, order = c(p, dd, q),
                                  include.mean = include_mean && dd == 0,
                                  method = "CSS-ML")),
    error = function(e) NULL)
}

bj_orders <- function(type, pmax, dmax, qmax) {
  g <- switch(type,
              AR    = expand.grid(p = 1:pmax, d = 0, q = 0),
              MA    = expand.grid(p = 0, d = 0, q = 1:qmax),
              ARMA  = expand.grid(p = 1:pmax, d = 0, q = 1:qmax),
              ARIMA = expand.grid(p = 0:pmax, d = 1:dmax, q = 0:qmax))
  g
}

bj_search <- function(d, type, pmax, dmax, qmax, include_mean, criterion) {
  g <- bj_orders(type, pmax, dmax, qmax)
  rows <- lapply(seq_len(nrow(g)), function(i) {
    f <- bj_fit(d, g$p[i], g$d[i], g$q[i], include_mean)
    if (is.null(f)) return(NULL)
    e <- fc_errors(d, as.numeric(d - stats::residuals(f)), 1)
    data.frame(p = g$p[i], d = g$d[i], q = g$q[i],
               AIC = stats::AIC(f), BIC = stats::BIC(f),
               MAD = e[["MAD"]], MSE = e[["MSE"]], MAPE = e[["MAPE"]])
  })
  out <- do.call(rbind, rows)
  validate(need(!is.null(out), "No model could be estimated."))
  out <- out[order(out[[criterion]]), ]
  rownames(out) <- NULL
  out
}

bj_coef_table <- function(f) {
  est <- stats::coef(f)
  se <- sqrt(diag(f$var.coef))
  se <- se[names(est)]
  tval <- est / se
  data.frame("Parameter" = names(est),
             "Estimate" = round(est, 4),
             "Std. error" = round(se, 4),
             "t value" = round(tval, 3),
             "p value" = round(2 * stats::pnorm(-abs(tval)), 4),
             check.names = FALSE)
}

bj_model_name <- function(p, dd, q) {
  if (dd > 0) return(paste0("ARIMA(", p, ", ", dd, ", ", q, ")"))
  if (q == 0) return(paste0("AR(", p, ")"))
  if (p == 0) return(paste0("MA(", q, ")"))
  paste0("ARMA(", p, ", ", q, ")")
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
box_jenkins_ui <- function(id, type = c("AR", "MA", "ARMA", "ARIMA")) {
  type <- match.arg(type)
  ns <- NS(id)
  has_p <- type %in% c("AR", "ARMA", "ARIMA")
  has_q <- type %in% c("MA", "ARMA", "ARIMA")
  has_d <- type == "ARIMA"
  p_min <- if (type == "ARIMA") 0 else 1
  q_min <- if (type == "ARIMA") 0 else 1
  
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("mode"), "Model order",
                   choices = c("I will enter the order" = "manual",
                               "Find the best order" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        if (has_p) numericInput(ns("p"), "p (AR order)", 1, min = p_min, step = 1),
        if (has_d) numericInput(ns("d"), "d (differencing)", 1, min = 1, max = 2, step = 1),
        if (has_q) numericInput(ns("q"), "q (MA order)", 1, min = q_min, step = 1)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        if (has_p) numericInput(ns("pmax"), "Largest p to try", 3, min = 1, step = 1),
        if (has_d) numericInput(ns("dmax"), "Largest d to try", 2, min = 1, max = 2, step = 1),
        if (has_q) numericInput(ns("qmax"), "Largest q to try", 3, min = 1, step = 1),
        selectInput(ns("criterion"), "Choose the best order by",
                    choices = c("AIC", "BIC", "MAD", "MSE", "MAPE"))
      ),
      if (!has_d) checkboxInput(ns("mean"), "Include a constant (mean)", TRUE),
      fc_horizon_input(ns, value = 6),
      numericInput(ns("lags"), "Lags in the ACF / PACF plots", 15, min = 5, step = 1)
    ),
    mainPanel(
      h4("Autocorrelation (ACF) and partial autocorrelation (PACF)"),
      plotOutput(ns("acf_plot"), height = if (has_d) "460px" else "260px"),
      h4(textOutput(ns("title"))),
      plotOutput(ns("forecast_plot"), height = "360px"),
      h4("Parameter estimates"),
      tableOutput(ns("coef_table")),
      verbatimTextOutput(ns("fit_text")),
      h4("Error measures (in-sample one-step forecasts)"),
      tableOutput(ns("error_table")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Comparison of the orders tried (best 10)"),
        tableOutput(ns("search_table"))
      ),
      h4("ACF of the residuals"),
      plotOutput(ns("resid_plot"), height = "230px"),
      h4("Forecasts by period"),
      tableOutput(ns("period_table"))
    )
  )
}


box_jenkins_server <- function(id, data, type = c("AR", "MA", "ARMA", "ARIMA")) {
  type <- match.arg(type)
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session,
                                 prefer = if (type == "ARIMA") "Trend" else "Stationary")
    include_mean <- reactive(if (type == "ARIMA") FALSE else isTRUE(input$mean))
    
    search <- reactive({
      req(input$mode == "auto")
      pmax <- if (type %in% c("AR", "ARMA", "ARIMA")) input$pmax else 0
      qmax <- if (type %in% c("MA", "ARMA", "ARIMA")) input$qmax else 0
      dmax <- if (type == "ARIMA") input$dmax else 0
      req(!is.null(pmax), !is.null(qmax), !is.null(dmax))
      withProgress(message = "Fitting models...",
                   bj_search(demand(), type, pmax, dmax, qmax,
                             include_mean(), input$criterion))
    })
    
    model_order <- reactive({
      if (input$mode == "auto") {
        s <- search(); return(c(s$p[1], s$d[1], s$q[1]))
      }
      p  <- if (type %in% c("AR", "ARMA", "ARIMA")) input$p else 0
      dd <- if (type == "ARIMA") input$d else 0
      q  <- if (type %in% c("MA", "ARMA", "ARIMA")) input$q else 0
      req(!is.null(p), !is.null(dd), !is.null(q))
      validate(need(!is.na(p) && !is.na(dd) && !is.na(q) && p >= 0 && q >= 0 && dd >= 0,
                    "Enter non-negative orders."))
      c(p, dd, q)
    })
    
    model <- reactive({
      d <- demand(); o <- model_order()
      validate(need(length(d) > sum(o) + 5,
                    "Too few observations for this model order."))
      f <- bj_fit(d, o[1], o[2], o[3], include_mean())
      validate(need(!is.null(f), "The model could not be estimated. Try another order."))
      f
    })
    
    result <- reactive({
      d <- demand(); f <- model(); o <- model_order()
      req(input$h >= 1)
      pr <- stats::predict(f, n.ahead = input$h)
      fut <- as.numeric(pr$pred); se <- as.numeric(pr$se)
      list(d = d, fit = as.numeric(d - stats::residuals(f)), fut = fut,
           lower = fut - 1.96 * se, upper = fut + 1.96 * se, from = 1,
           title = bj_model_name(o[1], o[2], o[3]))
    })
    
    fc_render_outputs(output, result)
    
    output$acf_plot <- renderPlot({
      d <- demand(); L <- input$lags
      dd <- if (type == "ARIMA") model_order()[2] else 0
      if (dd > 0) {
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
      } else {
        par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
      }
      stats::acf(d, lag.max = L, main = "ACF of the series")
      stats::pacf(d, lag.max = L, main = "PACF of the series")
      if (dd > 0) {
        x <- diff(d, differences = dd)
        stats::acf(x, lag.max = L, main = paste0("ACF after differencing (d = ", dd, ")"))
        stats::pacf(x, lag.max = L, main = paste0("PACF after differencing (d = ", dd, ")"))
      }
    })
    
    output$coef_table <- renderTable(bj_coef_table(model()))
    
    output$fit_text <- renderText({
      f <- model()
      fitdf <- sum(model_order()[c(1, 3)])
      lb <- stats::Box.test(stats::residuals(f),
                            lag = min(max(10, fitdf + 5), length(demand()) - 1),
                            type = "Ljung-Box", fitdf = fitdf)
      paste0("sigma^2 = ", round(f$sigma2, 4),
             "    log-likelihood = ", round(f$loglik, 3),
             "    AIC = ", round(stats::AIC(f), 3),
             "    BIC = ", round(stats::BIC(f), 3),
             "\nLjung-Box test on residuals: Q = ", round(lb$statistic, 3),
             ", p-value = ", round(lb$p.value, 4),
             if (!is.finite(lb$p.value)) ""
             else if (lb$p.value > 0.05) "  (residuals look like white noise)"
             else "  (residuals are still autocorrelated)")
    })
    
    output$search_table <- renderTable(fc_search_table(search()))
    
    output$resid_plot <- renderPlot({
      stats::acf(stats::residuals(model()), lag.max = input$lags,
                 main = "ACF of the residuals")
    })
  })
}