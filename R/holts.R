
holt_run <- function(d, alpha, beta, S0, G0, h = 1) {
  n <- length(d)
  F <- numeric(n); S <- numeric(n); G <- numeric(n)
  S_prev <- S0; G_prev <- G0
  for (t in 1:n) {
    F[t] <- S_prev + G_prev
    S[t] <- alpha * d[t] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    S_prev <- S[t]; G_prev <- G[t]
  }
  list(fit = F, S = S, G = G, fut = S[n] + seq_len(h) * G[n])
}

holt_search <- function(d, S0, G0, step = 0.05, criterion = "MSE") {
  a <- seq(step, 1, by = step)
  grid <- expand.grid(alpha = a, beta = a)
  fc_grid_search(grid, function(p) holt_run(d, p$alpha, p$beta, S0, G0)$fit,
                 d, criterion)
}



holt_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("mode"), "Smoothing constants",
                   choices = c("I will enter alpha and beta" = "manual",
                               "Find the best alpha and beta" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "alpha (level)", 0.01, 1, 0.2, step = 0.01),
        sliderInput(ns("beta"), "beta (slope)", 0.01, 1, 0.1, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        selectInput(ns("step"), "Search step",
                    choices = c("0.05" = 0.05, "0.02" = 0.02, "0.1" = 0.1)),
        fc_criterion_input(ns)
      ),
      hr(),
      fc_init_input(ns, slope = TRUE),
      hr(),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      h4("Initial values"),
      verbatimTextOutput(ns("init_text")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Best 10 (alpha, beta) pairs"),
        tableOutput(ns("search_table"))
      ),
      h4("Level and slope by period"),
      tableOutput(ns("state_table"))
    )
  )
}



holt_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Trend")
    
    init <- reactive(fc_init_values(input, demand(), slope = TRUE))
    
    search <- reactive({
      req(input$mode == "auto")
      withProgress(message = "Searching alpha and beta...",
                   holt_search(demand(), init()$level, init()$slope,
                               as.numeric(input$step), input$criterion))
    })
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      if (input$mode == "manual") {
        alpha <- input$alpha; beta <- input$beta
      } else {
        alpha <- search()$alpha[1]; beta <- search()$beta[1]
      }
      h <- holt_run(d, alpha, beta, init()$level, init()$slope, input$h)
      list(d = d, fit = h$fit, fut = h$fut, from = 1, S = h$S, G = h$G,
           alpha = alpha, beta = beta,
           title = paste0("Holt's method, alpha = ", round(alpha, 3),
                          ", beta = ", round(beta, 3)))
    })
    
    fc_render_outputs(output, result)
    
    output$init_text <- renderText({
      i <- init()
      paste0(i$source, "\nS0 = ", round(i$level, 3), "    G0 = ", round(i$slope, 3))
    })
    
    output$search_table <- renderTable(fc_search_table(search()))
    
    output$state_table <- renderTable({
      r <- result()
      data.frame("Period" = c(0, seq_along(r$d)),
                 "Level S" = round(c(init()$level, r$S), 3),
                 "Slope G" = round(c(init()$slope, r$G), 3),
                 check.names = FALSE)
    })
  })
}