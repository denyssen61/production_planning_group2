library(shiny)
source("planningproduction1.R")
source("moving_average.R")
source("learning_curve.R")
ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("planningproduction1", planningproduction1_ui("planningproduction1")),
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  planningproduction1_server("planningproduction1")
}

shinyApp(ui, server)