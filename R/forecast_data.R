

forecast_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"), "Upload data (.csv, .xlsx or .xls)",
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      selectInput(ns("sep"), "CSV separator",
                  choices = c("Detect automatically" = "auto",
                              "Comma (,)" = ",", "Semicolon (;)" = ";",
                              "Tab" = "\t")),
      uiOutput(ns("sheet_ui")),
      actionButton(ns("example"), "Use the example data"),
      hr(),
      helpText("Put one series in each column and one period in each row.",
               "The first row must contain the column names.",
               "Until a file is uploaded, an example data set with a",
               "stationary, a trend and a seasonal series is used.")
    ),
    mainPanel(
      h4(textOutput(ns("info"))),
      plotOutput(ns("plot"), height = "320px"),
      h4("Data"),
      tableOutput(ns("preview"))
    )
  )
}


forecast_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    source_type <- reactiveVal("example")
    observeEvent(input$file, source_type("file"))
    observeEvent(input$example, source_type("example"))
    
    is_excel <- reactive({
      req(input$file)
      tolower(tools::file_ext(input$file$name)) %in% c("xlsx", "xls")
    })
    
    output$sheet_ui <- renderUI({
      req(input$file, is_excel())
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput(session$ns("sheet"), "Excel sheet", choices = sheets)
    })
    
    data <- reactive({
      if (source_type() == "example" || is.null(input$file))
        return(fc_example_data())
      df <- tryCatch(
        fc_read_data(input$file$datapath, input$file$name,
                     sep = input$sep,
                     sheet = if (is_excel()) input$sheet else NULL),
        error = function(e) {
          validate(need(FALSE, paste("The file could not be read:",
                                     conditionMessage(e))))
        })
      validate(need(any(vapply(df, is.numeric, logical(1))),
                    "The file has no numeric column."))
      df
    })
    
    output$info <- renderText({
      df <- data()
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      paste0(if (source_type() == "example") "Example data" else input$file$name,
             ":  ", nrow(df), " periods,  numeric series: ",
             paste(num, collapse = ", "))
    })
    
    output$plot <- renderPlot({
      df <- data()
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      num <- setdiff(num, c("Period", "period", "t", "Time", "time"))
      req(length(num) > 0)
      m <- as.matrix(df[num])
      cols <- rep_len(c("#2F6FB0", "#B4501E", "#4E8F4A", "#7A4E9C", "grey40"),
                      length(num))
      matplot(seq_len(nrow(m)), m, type = "o", pch = 19, cex = 0.6,
              lty = 1, col = cols, xlab = "Period", ylab = "Value",
              main = "Uploaded series")
      legend("topleft", bty = "n", legend = num, col = cols, lty = 1, pch = 19)
    })
    
    output$preview <- renderTable(data(), na = "")
    
    data
  })
}