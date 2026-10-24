

FC_COLORS <- c(actual = "grey30", fit = "#2F6FB0",
               future = "#B4501E", band = "#F3DCCF")



fc_guess_sep <- function(path) {
  first <- readLines(path, n = 10, warn = FALSE)
  if (any(grepl(";", first, fixed = TRUE))) return(";")
  if (any(grepl("\t", first, fixed = TRUE))) return("\t")
  ","
}

# Text columns that actually hold numbers (e.g. "12,5") become numeric.
fc_fix_numbers <- function(df) {
  for (k in names(df)) {
    x <- df[[k]]
    if (is.character(x) || is.factor(x)) {
      x <- trimws(as.character(x))
      y <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
      filled <- !is.na(x) & x != ""
      if (any(filled) && sum(!is.na(y)) >= 0.8 * sum(filled)) df[[k]] <- y
    }
  }
  df
}

fc_read_data <- function(path, filename, sep = "auto", sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = if (is.null(sheet)) 1 else sheet)
    df <- as.data.frame(df, check.names = FALSE)
  } else if (ext %in% c("csv", "txt")) {
    if (sep == "auto") sep <- fc_guess_sep(path)
    df <- utils::read.csv(path, sep = sep, check.names = FALSE,
                          stringsAsFactors = FALSE, strip.white = TRUE)
  } else {
    stop("Unsupported file type. Please upload a .csv, .xlsx or .xls file.")
  }
  fc_fix_numbers(df)
}


fc_example_data <- function() {
  data.frame(
    Period     = 1:24,
    Stationary = c(205, 192, 210, 198, 187, 214, 201, 195, 208, 190, 203, 211,
                   196, 199, 207, 185, 202, 213, 194, 200, 209, 191, 204, 198),
    Trend      = c(120, 126, 129, 137, 140, 146, 151, 153, 160, 166, 168, 175,
                   179, 183, 190, 192, 198, 205, 207, 213, 219, 222, 228, 233),
    Seasonal   = c( 82, 118, 139,  91,  94, 127, 160,  97, 104, 141, 175, 107,
                    110, 159, 186, 121, 119, 171, 201, 130, 133, 180, 222, 136)
  )
}




fc_series_input <- function(ns) {
  selectInput(ns("series"), "Series", choices = NULL)
}


fc_series_reactive <- function(data, input, session, prefer = NULL) {
  observeEvent(data(), {
    df <- data()
    cols <- names(df)[vapply(df, is.numeric, logical(1))]
    cols <- setdiff(cols, c("Period", "period", "t", "Time", "time"))
    if (length(cols) == 0) cols <- names(df)[vapply(df, is.numeric, logical(1))]
    sel <- if (!is.null(prefer) && prefer %in% cols) prefer else tail(cols, 1)
    updateSelectInput(session, "series", choices = cols, selected = sel)
  })
  reactive({
    df <- data()
    req(input$series, input$series %in% names(df))
    d <- as.numeric(df[[input$series]])
    d <- d[!is.na(d)]
    validate(need(length(d) >= 4,
                  "The selected series needs at least 4 observations."))
    d
  })
}



fc_errors <- function(d, F, from = 1) {
  n <- length(d)
  t <- from:n
  t <- t[!is.na(F[t])]
  e <- F[t] - d[t]
  nz <- d[t] != 0
  c(MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = if (any(nz)) mean(abs(e[nz] / d[t][nz])) * 100 else NA_real_)
}

fc_criterion_input <- function(ns, choices = c("MAD", "MSE", "MAPE")) {
  selectInput(ns("criterion"), "Choose the best value by", choices = choices)
}

fc_horizon_input <- function(ns, value = 4) {
  numericInput(ns("h"), "Periods to forecast ahead", value = value,
               min = 1, step = 1)
}


fc_grid_search <- function(grid, run, d, criterion = "MSE", from = 1) {
  err <- t(vapply(seq_len(nrow(grid)), function(i) {
    F <- tryCatch(run(as.list(grid[i, , drop = FALSE])),
                  error = function(e) rep(NA_real_, length(d)))
    if (all(is.na(F[from:length(d)]))) return(c(MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_))
    fc_errors(d, F, from)
  }, c(MAD = 0, MSE = 0, MAPE = 0)))
  out <- cbind(grid, err)
  out <- out[order(out[[criterion]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}



fc_regression_init <- function(d, k = 0) {
  if (is.null(k) || is.na(k) || k <= 1 || k > length(d)) k <- length(d)
  t <- seq_len(k)
  fit <- stats::lm(d[t] ~ t)
  list(level = unname(coef(fit)[1]), slope = unname(coef(fit)[2]), k = k)
}


fc_init_input <- function(ns, slope = TRUE, k_default = 0, k_label = NULL) {
  if (is.null(k_label))
    k_label <- "Periods used for the regression (0 = all)"
  tagList(
    radioButtons(ns("init"), "Initial values",
                 choices = c("Fit a regression line (D = a + b t)" = "reg",
                             "Enter manually" = "manual")),
    conditionalPanel(
      condition = "input.init == 'reg'", ns = ns,
      numericInput(ns("init_k"), k_label, value = k_default, min = 0, step = 1)
    ),
    conditionalPanel(
      condition = "input.init == 'manual'", ns = ns,
      numericInput(ns("S0"), "Initial level S0", value = NA),
      if (slope) numericInput(ns("G0"), "Initial slope G0", value = NA)
    )
  )
}


fc_init_values <- function(input, d, slope = TRUE) {
  if (input$init == "reg") {
    r <- fc_regression_init(d, input$init_k)
    list(level = r$level, slope = if (slope) r$slope else 0, k = r$k,
         source = paste0("Regression on periods 1–", r$k,
                         ":  D = ", round(r$level, 3), " + ",
                         round(r$slope, 3), " t"))
  } else {
    validate(need(!is.na(input$S0), "Please enter the initial level S0."))
    if (slope) validate(need(!is.na(input$G0), "Please enter the initial slope G0."))
    list(level = input$S0, slope = if (slope) input$G0 else 0, k = length(d),
         source = "Entered manually")
  }
}


fc_parse_numbers <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
  parts <- strsplit(trimws(txt), "[,;[:space:]]+")[[1]]
  suppressWarnings(as.numeric(parts[nzchar(parts)]))
}



fc_detect_season <- function(d, max_lag = NULL) {
  n <- length(d)
  if (is.null(max_lag) || is.na(max_lag)) max_lag <- floor(n / 2)
  max_lag <- max(2, min(max_lag, floor(n / 2)))
  t <- seq_len(n)
  r <- stats::residuals(stats::lm(d ~ t))
  a <- stats::acf(r, lag.max = max_lag, plot = FALSE)$acf[-1]
  lags <- 2:max_lag
  lags[which.max(a[lags])]
}

fc_season_input <- function(ns, value = 4) {
  tagList(
    radioButtons(ns("season_mode"), "Season length N",
                 choices = c("I will enter N" = "manual",
                             "Detect from the ACF" = "auto"), inline = TRUE),
    conditionalPanel(
      condition = "input.season_mode == 'manual'", ns = ns,
      numericInput(ns("season"), "Periods per season (N)", value = value,
                   min = 2, step = 1)
    )
  )
}

fc_season_value <- function(input, d) {
  N <- if (input$season_mode == "auto") fc_detect_season(d) else input$season
  validate(need(!is.na(N) && N >= 2, "The season length must be at least 2."),
           need(length(d) >= 2 * N,
                "At least two full seasons of data are needed."))
  as.integer(N)
}




fc_main_panel <- function(ns, ..., after = NULL) {
  mainPanel(
    h4(textOutput(ns("title"))),
    plotOutput(ns("forecast_plot"), height = "360px"),
    h4("Error measures"),
    tableOutput(ns("error_table")),
    ...,
    h4("Forecasts by period"),
    tableOutput(ns("period_table")),
    after
  )
}

fc_plot <- function(r, ylab = "Demand") {
  d <- r$d; n <- length(d); h <- length(r$fut)
  y_rng <- range(c(d, r$fit, r$fut, r$lower, r$upper), na.rm = TRUE)
  plot(1:n, d, type = "o", pch = 19, col = FC_COLORS["actual"],
       xlim = c(1, n + h), ylim = y_rng, xlab = "Period", ylab = ylab,
       main = r$title)
  if (!is.null(r$lower)) {
    polygon(c(n + 1:h, rev(n + 1:h)), c(r$lower, rev(r$upper)),
            col = FC_COLORS["band"], border = NA)
    points(1:n, d, type = "o", pch = 19, col = FC_COLORS["actual"])
  }
  lines(1:n, r$fit, type = "o", pch = 17, cex = 0.8, lty = 2,
        col = FC_COLORS["fit"])
  lines(c(n, n + 1:h), c(d[n], r$fut), lty = 1, lwd = 2,
        col = FC_COLORS["future"])
  points(n + 1:h, r$fut, pch = 17, cex = 1.4, col = FC_COLORS["future"])
  abline(v = n + 0.5, col = "grey80", lty = 3)
  leg <- c("Actual", "One-step forecast", "Future forecast")
  col <- FC_COLORS[c("actual", "fit", "future")]
  pch <- c(19, 17, 17); lty <- c(1, 2, 1)
  if (!is.null(r$lower)) {
    leg <- c(leg, "95% interval"); col <- c(col, FC_COLORS["band"])
    pch <- c(pch, 15); lty <- c(lty, NA)
  }
  legend("topleft", bty = "n", legend = leg, col = col, pch = pch, lty = lty)
}

fc_error_df <- function(r) {
  e <- fc_errors(r$d, r$fit, r$from)
  data.frame("Measure" = c("MAD", "MSE", "MAPE (%)"),
             "Value" = round(unname(e), 3),
             "Periods evaluated" = paste0(r$from, "–", length(r$d)),
             check.names = FALSE)
}

fc_period_df <- function(r) {
  n <- length(r$d); h <- length(r$fut)
  out <- data.frame(
    "Period" = 1:(n + h),
    "Demand (D)" = c(r$d, rep(NA, h)),
    "Forecast (F)" = round(c(r$fit, r$fut), 2),
    "Error (e = F - D)" = round(c(r$fit - r$d, rep(NA, h)), 2),
    check.names = FALSE
  )
  if (!is.null(r$lower)) {
    out[["Lower 95%"]] <- round(c(rep(NA, n), r$lower), 2)
    out[["Upper 95%"]] <- round(c(rep(NA, n), r$upper), 2)
  }
  out
}


fc_render_outputs <- function(output, result) {
  output$title <- renderText({
    r <- result()
    paste0(r$title, "  —  next-period forecast: ", round(r$fut[1], 2))
  })
  output$forecast_plot <- renderPlot(fc_plot(result()))
  output$error_table   <- renderTable(fc_error_df(result()))
  output$period_table  <- renderTable(fc_period_df(result()), na = "")
}


fc_search_table <- function(tab, top = 10) {
  tab <- head(tab, top)
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], round, 3)
  cbind(Rank = seq_len(nrow(tab)), tab)
}