
winters_init_factors <- function(d, N, S0, G0, k = length(d)) {
  k <- max(N, min(k, length(d)))
  t <- seq_len(k)
  pos <- (t - 1) %% N + 1
  cf <- as.numeric(tapply(d[t] / (S0 + G0 * t), pos, mean))
  cf * N / sum(cf)
}

winters_run <- function(d, N, alpha, beta, gamma, S0, G0, c0, h = 1) {
  n <- length(d)
  F <- S <- G <- C <- numeric(n)
  cf <- c0                      # latest factor for each season position
  S_prev <- S0; G_prev <- G0
  for (t in 1:n) {
    j <- (t - 1) %% N + 1
    F[t] <- (S_prev + G_prev) * cf[j]
    S[t] <- alpha * d[t] / cf[j] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    cf[j] <- gamma * d[t] / S[t] + (1 - gamma) * cf[j]
    C[t] <- cf[j]
    S_prev <- S[t]; G_prev <- G[t]
  }
  tau <- seq_len(h)
  j_fut <- (n + tau - 1) %% N + 1
  list(fit = F, S = S, G = G, C = C, last_factors = cf,
       fut = (S[n] + tau * G[n]) * cf[j_fut])
}

winters_search <- function(d, N, S0, G0, c0, step = 0.1, criterion = "MSE") {
  a <- seq(step, 1, by = step)
  grid <- expand.grid(alpha = a, beta = a, gamma = a)
  fc_grid_search(grid, function(p)
    winters_run(d, N, p$alpha, p$beta, p$gamma, S0, G0, c0)$fit,
    d, criterion)
}



winters_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      fc_season_input(ns),
      radioButtons(ns("mode"), "Smoothing constants",
                   choices = c("I will enter alpha, beta, gamma" = "manual",
                               "Find the best values" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "alpha (level)", 0.01, 1, 0.2, step = 0.01),
        sliderInput(ns("beta"), "beta (slope)", 0.01, 1, 0.1, step = 0.01),
        sliderInput(ns("gamma"), "gamma (seasonal)", 0.01, 1, 0.1, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        selectInput(ns("step"), "Search step",
                    choices = c("0.1 (fast)" = 0.1, "0.05 (slower)" = 0.05)),
        fc_criterion_input(ns)
      ),
      hr(),
      fc_init_input(ns, slope = TRUE,
                    k_label = "Periods used for the regression (0 = all; e.g. 2 seasons)"),
      conditionalPanel(
        condition = "input.init == 'manual'", ns = ns,
        textInput(ns("c0"), "Initial seasonal factors (optional, comma-separated)", ""),
        helpText("Leave empty to estimate them from the data and the",
                 "line S0 + G0 t.")
      ),
      hr(),
      fc_horizon_input(ns, value = 8)
    ),
    fc_main_panel(
      ns,
      h4("Initial values"),
      verbatimTextOutput(ns("init_text")),
      tableOutput(ns("factor_table")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Best 10 (alpha, beta, gamma) combinations"),
        tableOutput(ns("search_table"))
      ),
      h4("Level, slope and seasonal factor by period"),
      tableOutput(ns("state_table"))
    )
  )
}



winters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Seasonal")
    season <- reactive(fc_season_value(input, demand()))
    
    init <- reactive({
      d <- demand(); N <- season()
      i <- fc_init_values(input, d, slope = TRUE)
      c0 <- if (input$init == "manual") fc_parse_numbers(input$c0) else numeric(0)
      if (length(c0) > 0) {
        validate(need(length(c0) == N && !anyNA(c0) && all(c0 > 0),
                      paste0("Enter exactly ", N, " positive seasonal factors.")))
        i$factor_source <- "Seasonal factors entered manually"
      } else {
        validate(need(all(i$level + i$slope * seq_len(i$k) > 0),
                      "The initial trend line must be positive to compute seasonal factors."))
        c0 <- winters_init_factors(d, N, i$level, i$slope, i$k)
        i$factor_source <- paste0("Seasonal factors = average of D_t / (S0 + G0 t), periods 1–",
                                  max(N, i$k))
      }
      i$c0 <- c0
      i
    })
    
    search <- reactive({
      req(input$mode == "auto")
      i <- init()
      withProgress(message = "Searching alpha, beta and gamma...",
                   winters_search(demand(), season(), i$level, i$slope, i$c0,
                                  as.numeric(input$step), input$criterion))
    })
    
    result <- reactive({
      d <- demand(); N <- season(); i <- init()
      req(input$h >= 1)
      if (input$mode == "manual") {
        p <- c(input$alpha, input$beta, input$gamma)
      } else {
        s <- search(); p <- c(s$alpha[1], s$beta[1], s$gamma[1])
      }
      p <- round(p, 3)
      w <- winters_run(d, N, p[1], p[2], p[3], i$level, i$slope, i$c0, input$h)
      list(d = d, fit = w$fit, fut = w$fut, from = 1, w = w, N = N,
           title = paste0("Winters' method, N = ", N, ", alpha = ", p[1],
                          ", beta = ", p[2], ", gamma = ", p[3]))
    })
    
    fc_render_outputs(output, result)
    
    output$init_text <- renderText({
      i <- init()
      paste0(i$source, "\nS0 = ", round(i$level, 3), "    G0 = ",
             round(i$slope, 3), "\n", i$factor_source)
    })
    
    output$factor_table <- renderTable({
      i <- init()
      data.frame("Season position" = seq_along(i$c0),
                 "Initial factor" = round(i$c0, 4),
                 "Final factor" = round(result()$w$last_factors, 4),
                 check.names = FALSE)
    })
    
    output$search_table <- renderTable(fc_search_table(search()))
    
    output$state_table <- renderTable({
      r <- result()
      data.frame("Period" = seq_along(r$d), "Level S" = round(r$w$S, 3),
                 "Slope G" = round(r$w$G, 3),
                 "Seasonal factor c" = round(r$w$C, 4), check.names = FALSE)
    })
  })
}