
ses_forecast <- function(d, alpha, S0) {
  n <- length(d)
  F <- numeric(n + 1)
  S <- S0
  for (t in 1:n) {
    F[t] <- S
    S <- alpha * d[t] + (1 - alpha) * S
  }
  F[n + 1] <- S
  F
}

ses_search <- function(d, S0, step = 0.01, criterion = "MSE") {
  fc_grid_search(data.frame(alpha = seq(step, 1, by = step)),
                 function(p) ses_forecast(d, p$alpha, S0), d, criterion)
}



exponential_smoothing_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_series_input(ns),
      radioButtons(ns("mode"), "Smoothing constant alpha",
                   choices = c("I will enter alpha" = "manual",
                               "Find the best alpha" = "auto")),
      conditionalPanel(
        condition = "input.mode == 'manual'", ns = ns,
        sliderInput(ns("alpha"), "alpha", min = 0.01, max = 1, value = 0.2, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        selectInput(ns("step"), "Search step for alpha",
                    choices = c("0.01" = 0.01, "0.05" = 0.05, "0.1" = 0.1)),
        fc_criterion_input(ns)
      ),
      hr(),
      fc_init_input(ns, slope = FALSE),
      hr(),
      fc_horizon_input(ns)
    ),
    fc_main_panel(
      ns,
      h4("Initial value"),
      verbatimTextOutput(ns("init_text")),
      conditionalPanel(
        condition = "input.mode == 'auto'", ns = ns,
        h4("Error measure for every alpha"),
        plotOutput(ns("alpha_plot"), height = "250px")
      )
    )
  )
}



exponential_smoothing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_series_reactive(data, input, session, prefer = "Stationary")
    
    init <- reactive(fc_init_values(input, demand(), slope = FALSE))
    
    search <- reactive({
      req(input$mode == "auto")
      ses_search(demand(), init()$level, as.numeric(input$step), input$criterion)
    })
    
    result <- reactive({
      d <- demand(); n <- length(d)
      req(input$h >= 1)
      alpha <- if (input$mode == "manual") input$alpha else search()$alpha[1]
      F <- ses_forecast(d, alpha, init()$level)
      list(d = d, fit = F[1:n], fut = rep(F[n + 1], input$h), from = 1,
           alpha = alpha,
           title = paste0("Simple exponential smoothing, alpha = ", round(alpha, 3)))
    })
    
    fc_render_outputs(output, result)
    
    output$init_text <- renderText({
      paste0(init()$source, "\nS0 = F1 = ", round(init()$level, 3))
    })
    
    output$alpha_plot <- renderPlot({
      tab <- search(); tab <- tab[order(tab$alpha), ]
      k <- input$criterion
      plot(tab$alpha, tab[[k]], type = "l", lwd = 2, col = FC_COLORS["fit"],
           xlab = "alpha", ylab = k,
           main = paste0("Best alpha by ", k, " = ", result()$alpha))
      abline(v = result()$alpha, lty = 2, col = FC_COLORS["future"])
    })
  })
}