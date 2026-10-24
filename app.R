library(shiny)



ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 
                 navbarMenu("Forecasting",
                            tabPanel("Data", forecast_data_ui("fc_data")),
                            
                            "----", "Stationary series",

                            tabPanel("Moving Average", moving_average_ui("moving_average")),
                            tabPanel("Weighted Moving Average", weighted_moving_average_ui("wma")),
                            tabPanel("Simple Exponential Smoothing", exponential_smoothing_ui("ses")),
                            tabPanel("Seasonal Factors (quick and dirty)", seasonal_factors_ui("seasonal_factors")),
                            
                            "----", "Trend series",
                            tabPanel("Holt's Method", holt_ui("holt")),
                            
                            "----", "Seasonal series",
                            tabPanel("Seasonal Decomposition", seasonal_decomposition_ui("decomposition")),
                            tabPanel("Winters' Method", winters_ui("winters")),
                            
                            "----", "Box???Jenkins",
                            tabPanel("AR", box_jenkins_ui("bj_ar", "AR")),
                            tabPanel("MA", box_jenkins_ui("bj_ma", "MA")),
                            tabPanel("ARMA", box_jenkins_ui("bj_arma", "ARMA")),
                            tabPanel("ARIMA", box_jenkins_ui("bj_arima", "ARIMA")),
                            
                            "----", "Regression",
                            tabPanel("Trend Regression", regression_forecast_ui("regression"))
                 )
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  

  fc_data <- forecast_data_server("fc_data")
  
  moving_average_server("moving_average", fc_data)
  weighted_moving_average_server("wma", fc_data)
  exponential_smoothing_server("ses", fc_data)
  seasonal_factors_server("seasonal_factors", fc_data)
  holt_server("holt", fc_data)
  seasonal_decomposition_server("decomposition", fc_data)
  winters_server("winters", fc_data)
  box_jenkins_server("bj_ar", fc_data, "AR")
  box_jenkins_server("bj_ma", fc_data, "MA")
  box_jenkins_server("bj_arma", fc_data, "ARMA")
  box_jenkins_server("bj_arima", fc_data, "ARIMA")
  regression_forecast_server("regression", fc_data)
}

shinyApp(ui, server)
