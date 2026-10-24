
moving_average_forecast <- function(d, N) {
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) F[t] <- mean(d[(t - N):(t - 1)])
  F
}

# Tries every N = 1 ... Nmax. For a fair comparison all N are evaluated
# over the SAME periods (Nmax+1 ... n).
moving_average_search <- function(d, Nmax, criterion = "MSE") {
  fc_grid_search(data.frame(N = 1:Nmax),
                 function(p) moving_average_forecast(d, p$N),
                 d, criterion, from = Nmax + 1)
}



moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("mode"), "How should N be chosen?",
                   choices = c("I will choose N" = "manual",
                               "Find the best N" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "N (number of past periods)", value = 3,
                     min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try", value = 12, min = 2, step = 1),
        fc_criterion_input(ns)
      ),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Comparison of all N values"),
        plotOutput(ns("n_plot"), height = "250px"),
        tableOutput(ns("n_table"))
      )
    )
  )
}



moving_average_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Stationary")
    
    search <- reactive({
      req(input$mode == "auto", input$Nmax)
      d <- demand()
      validate(need(input$Nmax < length(d),
                    "The largest N must be smaller than the number of observations."))
      moving_average_search(d, input$Nmax, input$criterion)
    })
    
    result <- reactive({
      d <- demand()
      req(input$h >= 1)
      if (input$mode == "manual") {
        req(input$N)
        validate(need(input$N >= 1 && input$N < length(d),
                      "N must be between 1 and the number of observations - 1."))
        N <- input$N; from <- N + 1
      } else {
        N <- search()$N[1]; from <- input$Nmax + 1
      }
      F <- moving_average_forecast(d, N)
      n <- length(d)
      list(d = d, fit = F[1:n], fut = rep(F[n + 1], input$h), from = from,
           N = N, title = paste0("MA(", N, ")"))
    })
    
    fc_render_outputs(output, result)
    
    output$n_plot <- renderPlot({
      tab <- search(); tab <- tab[order(tab$N), ]
      k <- input$criterion; best <- result()$N
      barplot(tab[[k]], names.arg = tab$N, border = NA,
              col = ifelse(tab$N == best, FC_COLORS["future"], "#9DB4CF"),
              xlab = "N", ylab = k, main = paste0("Best N by ", k, " = ", best))
    })
    
    output$n_table <- renderTable({
      tab <- search(); tab <- tab[order(tab$N), ]
      tab$N <- as.integer(tab$N)
      tab[c("MAD", "MSE", "MAPE")] <- lapply(tab[c("MAD", "MSE", "MAPE")], round, 3)
      tab
    })
  })
}