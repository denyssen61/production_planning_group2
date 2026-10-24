
wma_forecast <- function(d, w) {
  n <- length(d); N <- length(w)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) F[t] <- sum(w * d[(t - 1):(t - N)])
  F
}

# Weights through a softmax so that they stay positive and sum to 1.
wma_softmax <- function(z) { e <- exp(z - max(z)); e / sum(e) }

wma_optimise <- function(d, N, criterion = "MSE", from = N + 1) {
  if (N == 1) return(1)
  obj <- function(z) fc_errors(d, wma_forecast(d, wma_softmax(z)), from)[[criterion]]
  # start from equal weights and from "more weight on recent periods"
  starts <- list(rep(0, N), seq(1, 0, length.out = N))
  fits <- lapply(starts, function(s)
    stats::optim(s, obj, method = "Nelder-Mead", control = list(maxit = 2000)))
  best <- fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
  wma_softmax(best$par)
}

wma_search <- function(d, Nmax, criterion = "MSE") {
  from <- Nmax + 1
  ws <- lapply(1:Nmax, function(N) wma_optimise(d, N, criterion, from))
  rows <- lapply(1:Nmax, function(N) {
    e <- fc_errors(d, wma_forecast(d, ws[[N]]), from)
    data.frame(N = N, Weights = paste(round(ws[[N]], 3), collapse = ", "),
               MAD = e[["MAD"]], MSE = e[["MSE"]], MAPE = e[["MAPE"]],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "weights") <- ws
  out
}



weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("mode"), "Weights",
                   choices = c("I will enter the weights" = "manual",
                               "Find the best N and weights" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        textInput(ns("weights"), "Weights, most recent period first",
                  value = "0.5, 0.3, 0.2"),
        helpText("N is the number of weights. If the weights do not add",
                 "up to 1 they are rescaled.")
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try", value = 6, min = 1, step = 1),
        fc_criterion_input(ns)
      ),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      h4("Weights used"),
      tableOutput(ns("weight_table")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Best weights for each N"),
        tableOutput(ns("n_table"))
      )
    )
  )
}


# ---------------------------------------------------------------------
# 3) SERVER
# ---------------------------------------------------------------------
weighted_moving_average_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Stationary")
    
    search <- reactive({
      req(input$mode == "auto", input$Nmax)
      d <- demand()
      validate(need(input$Nmax < length(d),
                    "The largest N must be smaller than the number of observations."))
      withProgress(message = "Optimising the weights...",
                   wma_search(d, input$Nmax, input$criterion))
    })
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      if (input$mode == "manual") {
        w <- fc_parse_numbers(input$weights)
        validate(need(length(w) >= 1 && !anyNA(w) && all(w >= 0) && sum(w) > 0,
                      "Enter non-negative numeric weights separated by commas."),
                 need(length(w) < length(d),
                      "There are more weights than observations."))
        w <- w / sum(w); from <- length(w) + 1
      } else {
        tab <- search()
        best <- which.min(tab[[input$criterion]])
        w <- attr(tab, "weights")[[best]]; from <- input$Nmax + 1
      }
      F <- wma_forecast(d, w)
      n <- length(d)
      list(d = d, fit = F[1:n], fut = rep(F[n + 1], input$h), from = from,
           w = w, title = paste0("Weighted moving average, N = ", length(w)))
    })
    
    fc_render_outputs(output, result)
    
    output$weight_table <- renderTable({
      w <- result()$w
      data.frame("Lag" = paste0("D(t-", seq_along(w), ")"),
                 "Weight" = round(w, 4), check.names = FALSE)
    })
    
    output$n_table <- renderTable({
      tab <- search()
      tab$N <- as.integer(tab$N)
      tab[c("MAD", "MSE", "MAPE")] <- lapply(tab[c("MAD", "MSE", "MAPE")], round, 3)
      tab
    })
  })
}