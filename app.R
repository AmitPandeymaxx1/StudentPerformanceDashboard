# ==============================================================
#  🎓 Interactive Student Performance Analysis System
# ==============================================================
#  Put this file and student.csv in the SAME folder, then click
#  "Run App" in RStudio (or run shiny::runApp() in that folder).
#
#  Install the packages once:
#  install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "DT"))
# ==============================================================

# ---------------- Startup checks ----------------
# (these turn cryptic errors into a message that tells you exactly what to do)

pkgs <- c("shiny", "ggplot2", "dplyr", "plotly", "DT")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing package(s): ", paste(missing_pkgs, collapse = ", "),
       "\nRun this ONCE in the R console, then run the app again:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))",
       call. = FALSE)
}

if (!file.exists("student.csv")) {
  stop("student.csv was not found. R is currently looking in:\n  ", getwd(),
       "\nPut student.csv in the SAME folder as app.R, then either click 'Run App' ",
       "or use: Session > Set Working Directory > To Source File Location.",
       call. = FALSE)
}

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# ---------------- Data helpers ----------------

required_cols <- c("Student_ID", "Study_Hours", "Attendance", "Marks")

# Detect comma-, tab-, or semicolon-separated files. The supplied student.csv
# is tab-separated despite its .csv extension, so read.csv() misreads it.
read_student_file <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
separator <- if (grepl("\t", first_line, fixed = TRUE)) "\t" else if (grepl(";", first_line, fixed = TRUE)) ";" else ","
  read.table(path, header = TRUE, sep = separator, stringsAsFactors = FALSE,
             check.names = FALSE, fileEncoding = "UTF-8-BOM",
             strip.white = TRUE, comment.char = "")
}

prep_data <- function(d) {
  missing_cols <- setdiff(required_cols, names(d))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  for (nm in c("Student_ID", "Study_Hours", "Attendance", "Marks")) {
    d[[nm]] <- suppressWarnings(as.numeric(trimws(as.character(d[[nm]]))))
  }

  if (!"Department" %in% names(d)) d$Department <- "All"
  if (!"Gender" %in% names(d)) d$Gender <- "All"
  d$Department <- ifelse(is.na(d$Department) | trimws(as.character(d$Department)) == "",
                         "Unknown", trimws(as.character(d$Department)))
  d$Gender <- ifelse(is.na(d$Gender) | trimws(as.character(d$Gender)) == "",
                     "Unknown", trimws(as.character(d$Gender)))

  d <- d[complete.cases(d[, c("Student_ID", "Study_Hours", "Attendance", "Marks")]), ]
  d <- d[d$Study_Hours >= 0 & d$Attendance >= 0 & d$Attendance <= 100 &
           d$Marks >= 0 & d$Marks <= 100, ]
  if (nrow(d) == 0) {
    stop("No valid student rows found. Check the numeric columns and value ranges.", call. = FALSE)
  }
  d
}

# "UTF-8-BOM" also works if the CSV was re-saved from Excel
base_data <- prep_data(read_student_file("student.csv"))

num_vars <- c("Study Hours" = "Study_Hours",
              "Attendance"  = "Attendance",
              "Marks"       = "Marks")

no_data_msg <- "No students match the current filters 😅 Try widening the sliders."
need_rows <- function(d) validate(need(nrow(d) > 0, no_data_msg))

# Colours: red/green for Pass/Fail, a nice palette for everything else
palette_for <- function(var, n) {
  if (var == "Result") return(c(Pass = "#2ecc71", Fail = "#e74c3c"))
  rep_len(c("#3498db", "#e67e22", "#9b59b6", "#1abc9c", "#f1c40f", "#34495e"),
          max(n, 1))
}

kpi_card <- function(label, value, color = "#2c3e50") {
  div(class = "kpi",
      div(class = "kpi-label", label),
      div(class = "kpi-value", style = paste0("color:", color), as.character(value)))
}

hover_text <- function(d) {
  paste0("ID: ", d$Student_ID,
         "<br>Study hours: ", d$Study_Hours,
         "<br>Attendance: ", d$Attendance, "%",
         "<br>Marks: ", d$Marks)
}

# ---------------- UI ----------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f7fc; }

      .title-panel {
        background: linear-gradient(90deg, #4facfe, #00f2fe);
        color: white; padding: 15px; border-radius: 10px;
        text-align: center; margin-bottom: 20px;
      }
      .title-panel h1 { margin: 0 0 5px 0; }
      .title-panel p  { margin: 0; opacity: 0.9; }

      .well {
        background-color: #ffffff !important;
        border-radius: 15px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }

      h3 { color: #2c3e50; font-weight: bold; }
      h4 { color: #3498db; }

      .nav-pills > li > a { border-radius: 20px; font-weight: 600; }
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover { background-color: #3498db; }

      .kpi-row { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 15px; }
      .kpi {
        flex: 1; min-width: 130px; background: #ffffff; border-radius: 15px;
        padding: 12px 8px; text-align: center;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
      .kpi-label {
        color: #7f8c8d; font-size: 12px; text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .kpi-value { font-size: 30px; font-weight: bold; }

      .pred-card {
        text-align: center; background: #ffffff; border-radius: 15px;
        padding: 15px; margin-bottom: 15px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
      .pred-big { font-size: 60px; font-weight: bold; line-height: 1.1; }
      .pred-verdict { font-size: 20px; font-weight: bold; margin-top: 5px; }
      .target-box {
        background: #eef6ff; border-radius: 10px; padding: 10px;
        color: #2c3e50; font-weight: 600;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("🎓 Interactive Student Performance Analysis System"),
      p("Filter, explore, predict and download – everything updates live")),
  
  sidebarLayout(
    
    # ----- Sidebar: data + filters -----
    sidebarPanel(
      width = 3,
      
      h3("📁 Data"),
      selectInput("source", NULL,
                  choices = c("Extended sample (300 students)" = "extended",
                              "My original 15 students"         = "original",
                              "Upload my own CSV"              = "upload")),
      conditionalPanel(
        condition = "input.source == 'upload'",
        fileInput("upload", NULL, accept = ".csv", placeholder = "Choose a CSV file"),
        helpText("Needs the columns: Student_ID, Study_Hours, Attendance, Marks.",
                 "Department and Gender are optional.")
      ),
      
      hr(),
      h3("🎛️ Filters"),
      uiOutput("filters"),
      numericInput("pass_mark", "Pass mark", value = 60, min = 0, max = 100, step = 1),
      textInput("student_search", "🔎 Find Student ID (optional)", value = "",
                placeholder = "e.g. 12"),
      actionButton("reset", "🔄 Reset filters", class = "btn-primary", width = "100%")
    ),
    
    # ----- Main area: tabs -----
    mainPanel(
      width = 9,
      
      tabsetPanel(
        type = "pills",
        
        # ===== Overview =====
        tabPanel(
          "📊 Overview",
          br(),
          uiOutput("kpis"),
          fluidRow(
            column(8, plotlyOutput("scatter", height = "380px")),
            column(4, plotlyOutput("donut", height = "380px"))
          ),
          br(),
          fluidRow(
            column(4, sliderInput("bins", "Histogram bins", min = 5, max = 30, value = 10)),
            column(8, plotlyOutput("histogram", height = "300px"))
          ),
          hr(),
          h3("🏆 Top students"),
          sliderInput("top_n", "How many to show?", min = 3, max = 20, value = 5, step = 1),
          DTOutput("top_students")
        ),
        
        # ===== Explorer =====
        tabPanel(
          "🔍 Explorer",
          br(),
          fluidRow(
            column(3, selectInput("x_var", "X axis", choices = num_vars, selected = "Study_Hours")),
            column(3, selectInput("y_var", "Y axis", choices = num_vars, selected = "Marks")),
            column(3, selectInput("color_by", "Colour by",
                                  choices = c("Pass / Fail" = "Result",
                                              "Department"  = "Department",
                                              "Gender"      = "Gender",
                                              "None"        = "none"))),
            column(3, div(style = "margin-top: 25px;",
                          checkboxInput("trend", "Show trend line", value = TRUE)))
          ),
          plotlyOutput("explore_plot", height = "420px"),
          hr(),
          fluidRow(
            column(6,
                   h3("📦 Marks by group"),
                   selectInput("group_var", NULL,
                               choices = c("Department" = "Department",
                                           "Gender"     = "Gender",
                                           "Pass / Fail" = "Result")),
                   plotlyOutput("box_plot", height = "360px")),
            column(6,
                   h3("🔥 Correlation heatmap"),
                   plotOutput("cor_plot", height = "420px"))
          )
        ),
        
        # ===== Predictor =====
        tabPanel(
          "🎯 Predictor",
          br(),
          fluidRow(
            column(4,
                   wellPanel(
                     h3("What-if inputs"),
                     sliderInput("p_hours", "Study Hours", min = 0, max = 12, value = 5, step = 0.5),
                     sliderInput("p_att", "Attendance (%)", min = 0, max = 100, value = 85, step = 1),
                     hr(),
                     sliderInput("target", "🎯 Target marks", min = 40, max = 100, value = 80),
                     uiOutput("target_out")
                   )),
            column(8,
                   uiOutput("pred_card"),
                   plotlyOutput("pred_plot", height = "340px"))
          ),
          hr(),
          h3("📐 Model quality"),
          uiOutput("model_stats"),
          fluidRow(
            column(6, tableOutput("coef_table")),
            column(6, uiOutput("model_note"))
          ),
          helpText("The model learns from every student in the selected dataset –",
                   "the filters on the left don't change it.")
        ),
        
        # ===== Data =====
        tabPanel(
          "🗂️ Data",
          br(),
          downloadButton("download", "⬇️ Download filtered data (CSV)", class = "btn-success"),
          br(), br(),
          h3("Department summary"),
          tableOutput("dept_summary"),
          h3("All matching students"),
          DTOutput("data_table")
        )
      )
    )
  )
)

# ---------------- Server ----------------

server <- function(input, output, session) {
  
  # ----- Dataset selection -----
  raw_data <- reactive({
    if (input$source == "upload") {
      req(input$upload)
      d <- read_student_file(input$upload$datapath)
      validate(need(all(required_cols %in% names(d)),
                    paste("The CSV must contain these columns:",
                          paste(required_cols, collapse = ", "))))
      d <- prep_data(d)
      validate(need(nrow(d) > 0, "No valid rows found in that CSV."))
      d
    } else if (input$source == "original") {
      base_data %>% filter(Student_ID <= 15)
    } else {
      base_data
    }
  })
  
  # ----- Reset controls -----
  observeEvent(input$reset, {
    updateNumericInput(session, "pass_mark", value = 60)
    updateTextInput(session, "student_search", value = "")
  })

  # ----- Filters (rebuilt whenever the dataset changes or Reset is clicked) -----
  output$filters <- renderUI({
    input$reset
    d <- raw_data()
    req(nrow(d) > 0)
    
    span <- function(x) {
      lo <- floor(min(x))
      c(lo, max(ceiling(max(x)), lo + 1))
    }
    a <- span(d$Attendance)
    m <- span(d$Marks)
    h <- span(d$Study_Hours)
    
    tagList(
      sliderInput("f_att", "Attendance (%)", min = a[1], max = a[2], value = a, step = 1),
      sliderInput("f_marks", "Marks", min = m[1], max = m[2], value = m, step = 1),
      sliderInput("f_hours", "Study Hours", min = h[1], max = h[2], value = h, step = 0.5),
      checkboxGroupInput("f_dept", "Department",
                         choices = sort(unique(d$Department)),
                         selected = sort(unique(d$Department))),
      checkboxGroupInput("f_gender", "Gender",
                         choices = sort(unique(d$Gender)),
                         selected = sort(unique(d$Gender)))
    )
  })
  
  filtered <- reactive({
    req(input$f_att, input$f_marks, input$f_hours, input$pass_mark)
    raw_data() %>%
      filter(
        Attendance  >= input$f_att[1],   Attendance  <= input$f_att[2],
        Marks       >= input$f_marks[1], Marks       <= input$f_marks[2],
        Study_Hours >= input$f_hours[1], Study_Hours <= input$f_hours[2],
        Department %in% input$f_dept,
        Gender %in% input$f_gender
      ) %>%
      { if (!is.null(input$student_search) && nzchar(trimws(input$student_search)))
          filter(., grepl(trimws(input$student_search), as.character(Student_ID), fixed = TRUE))
        else . } %>%
      mutate(Result = factor(ifelse(Marks >= input$pass_mark, "Pass", "Fail"),
                             levels = c("Pass", "Fail")))
  })
  
  # ===================== Overview =====================
  
  output$kpis <- renderUI({
    d <- filtered()
    need_rows(d)
    div(class = "kpi-row",
        kpi_card("Students", nrow(d), "#8e44ad"),
        kpi_card("Average Marks", round(mean(d$Marks), 1), "#27ae60"),
        kpi_card("Highest Marks", max(d$Marks), "#e74c3c"),
        kpi_card("Pass Rate", paste0(round(mean(d$Result == "Pass") * 100, 1), "%"), "#3498db"),
        kpi_card("Avg Study Hours", round(mean(d$Study_Hours), 1), "#e67e22"))
  })
  
  output$scatter <- renderPlotly({
    d <- filtered()
    need_rows(d)
    
    p <- plot_ly(d, x = ~Study_Hours, y = ~Marks, color = ~Result,
                 colors = c(Pass = "#2ecc71", Fail = "#e74c3c"),
                 type = "scatter", mode = "markers",
                 marker = list(size = 9, opacity = 0.75),
                 text = ~hover_text(d), hoverinfo = "text")
    
    if (nrow(d) >= 3 && length(unique(d$Study_Hours)) > 1) {
      fit <- lm(Marks ~ Study_Hours, data = d)
      xs <- range(d$Study_Hours)
      p <- p %>% add_lines(x = xs,
                           y = predict(fit, data.frame(Study_Hours = xs)),
                           inherit = FALSE, name = "Trend line",
                           line = list(color = "#2c3e50", dash = "dash"))
    }
    
    p %>% layout(title = "Study Hours vs Marks (hover, zoom, click legend to hide)",
                 xaxis = list(title = "Study Hours"),
                 yaxis = list(title = "Marks"),
                 legend = list(orientation = "h", y = -0.2))
  })
  
  output$donut <- renderPlotly({
    d <- filtered()
    need_rows(d)
    cnt <- d %>% count(Result)
    
    plot_ly(cnt, labels = ~Result, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = ifelse(cnt$Result == "Pass", "#2ecc71", "#e74c3c")),
            textinfo = "label+percent") %>%
      layout(title = "Pass vs Fail", showlegend = FALSE)
  })
  
  output$histogram <- renderPlotly({
    d <- filtered()
    need_rows(d)
    
    plot_ly(d, x = ~Marks, type = "histogram", nbinsx = input$bins,
            marker = list(color = "#3498db", line = list(color = "white", width = 1))) %>%
      layout(title = "Marks Distribution (red dashed line = pass mark)",
             xaxis = list(title = "Marks"),
             yaxis = list(title = "Count"),
             shapes = list(list(type = "line",
                                x0 = input$pass_mark, x1 = input$pass_mark,
                                y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = "#e74c3c", dash = "dash", width = 2))))
  })
  
  output$top_students <- renderDT({
    d <- filtered()
    need_rows(d)
    top <- d %>%
      arrange(desc(Marks), desc(Attendance)) %>%
      head(input$top_n) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Student_ID, Department, Study_Hours, Attendance, Marks)
    
    datatable(top, rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 20))
  })
  
  # ===================== Explorer =====================
  
  output$explore_plot <- renderPlotly({
    d <- filtered()
    need_rows(d)
    xv <- input$x_var
    yv <- input$y_var
    validate(need(xv != yv, "Pick two different variables for X and Y 🙂"))
    
    if (input$color_by == "none") {
      col_arg <- I("#3498db")
      pal <- NULL
    } else {
      col_arg <- as.factor(d[[input$color_by]])
      pal <- palette_for(input$color_by, nlevels(col_arg))
    }
    
    p <- plot_ly(x = d[[xv]], y = d[[yv]], color = col_arg, colors = pal,
                 type = "scatter", mode = "markers",
                 marker = list(size = 9, opacity = 0.75),
                 text = hover_text(d), hoverinfo = "text")
    
    ttl <- paste(xv, "vs", yv)
    can_fit <- nrow(d) >= 3 && length(unique(d[[xv]])) > 1 &&
      length(unique(d[[yv]])) > 1
    if (can_fit) {
      r <- suppressWarnings(cor(d[[xv]], d[[yv]], use = "complete.obs"))
      if (is.finite(r)) ttl <- paste0(ttl, "  (correlation r = ", round(r, 2), ")")
      if (isTRUE(input$trend)) {
        fit <- lm(y ~ x, data = data.frame(x = d[[xv]], y = d[[yv]]))
        xs <- range(d[[xv]])
        p <- p %>% add_lines(x = xs, y = predict(fit, data.frame(x = xs)),
                             inherit = FALSE, name = "Trend line",
                             line = list(color = "#2c3e50", dash = "dash"))
      }
    }
    
    p %>% layout(title = ttl,
                 xaxis = list(title = xv),
                 yaxis = list(title = yv))
  })
  
  output$box_plot <- renderPlotly({
    d <- filtered()
    need_rows(d)
    g <- as.factor(d[[input$group_var]])
    
    plot_ly(x = g, y = d$Marks, color = g,
            colors = palette_for(input$group_var, nlevels(g)),
            type = "box", boxpoints = "all", jitter = 0.4, pointpos = 0,
            marker = list(size = 5, opacity = 0.6)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Marks"),
             showlegend = FALSE)
  })
  
  output$cor_plot <- renderPlot({
    d <- filtered()
    need_rows(d)
    validate(need(nrow(d) >= 3, "Need at least 3 students for correlations."))
    
    cm <- suppressWarnings(cor(d[, c("Study_Hours", "Attendance", "Marks")]))
    cm[is.na(cm)] <- 0
    long <- as.data.frame(as.table(cm))
    names(long) <- c("Var1", "Var2", "Correlation")
    
    ggplot(long, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlation, 2)), size = 6) +
      scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#3498db",
                           midpoint = 0, limits = c(-1, 1)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14)
  })
  
  # ===================== Predictor =====================
  
  model <- reactive({
    d <- raw_data()
    validate(need(nrow(d) >= 4, "Need at least 4 students to build the prediction model."))
    lm(Marks ~ Study_Hours + Attendance, data = d)
  })
  
  pred <- reactive({
    req(input$p_hours, input$p_att)
    p <- predict(model(),
                 newdata = data.frame(Study_Hours = input$p_hours,
                                      Attendance = input$p_att),
                 interval = "prediction", level = 0.95)
    clamp <- function(x) pmin(pmax(x, 0), 100)
    list(fit = clamp(p[1, "fit"]), lwr = clamp(p[1, "lwr"]), upr = clamp(p[1, "upr"]))
  })
  
  output$pred_card <- renderUI({
    p <- pred()
    ok <- p$fit >= input$pass_mark
    color <- if (ok) "#27ae60" else "#e74c3c"
    div(class = "pred-card",
        div(class = "kpi-label", "Predicted marks"),
        div(class = "pred-big", style = paste0("color:", color), round(p$fit, 1)),
        div(paste0("95% prediction range: ", round(p$lwr, 1), " – ", round(p$upr, 1))),
        div(class = "pred-verdict", style = paste0("color:", color),
            if (ok) "✅ Likely to PASS" else "⚠️ At risk of FAILING"))
  })
  
  output$pred_plot <- renderPlotly({
    d <- raw_data()
    p <- pred()
    
    plot_ly() %>%
      add_markers(data = d, x = ~Study_Hours, y = ~Marks, name = "Students",
                  marker = list(color = "#bdc3c7", size = 7),
                  text = ~hover_text(d), hoverinfo = "text") %>%
      add_markers(x = input$p_hours, y = p$fit, name = "Your prediction",
                  marker = list(color = "#e74c3c", size = 16, symbol = "star"),
                  error_y = list(type = "data", symmetric = FALSE,
                                 array = p$upr - p$fit,
                                 arrayminus = p$fit - p$lwr,
                                 color = "#e74c3c")) %>%
      layout(title = "Where you land among all students",
             xaxis = list(title = "Study Hours"),
             yaxis = list(title = "Marks", range = c(0, 105)),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$target_out <- renderUI({
    co <- coef(model())
    b0 <- unname(co["(Intercept)"])
    b1 <- unname(co["Study_Hours"])
    b2 <- unname(co["Attendance"])
    validate(need(!anyNA(c(b0, b1, b2)) && b1 > 0,
                  "Study hours don't have a positive effect in this data."))
    
    hrs <- (input$target - b0 - b2 * input$p_att) / b1
    msg <- if (hrs <= 0) {
      "You'd already hit this target with almost no extra study 🎉"
    } else if (hrs > 12) {
      "That needs more than 12 study hours – try raising attendance 😅"
    } else {
      paste0("You'd need about ", round(hrs, 1), " study hours at ",
             input$p_att, "% attendance.")
    }
    div(class = "target-box", msg)
  })
  
  output$model_stats <- renderUI({
    m <- model()
    s <- summary(m)
    div(class = "kpi-row",
        kpi_card("R²", round(s$r.squared, 3), "#27ae60"),
        kpi_card("Adjusted R²", round(s$adj.r.squared, 3), "#3498db"),
        kpi_card("RMSE (marks)", round(sqrt(mean(residuals(m)^2)), 2), "#e67e22"),
        kpi_card("Students used", nobs(m), "#8e44ad"))
  })
  
  output$coef_table <- renderTable({
    s <- summary(model())$coefficients
    data.frame(Term = rownames(s), round(s, 4), check.names = FALSE)
  }, striped = TRUE, hover = TRUE)
  
  output$model_note <- renderUI({
    d <- raw_data()
    req(nrow(d) >= 3)
    r <- suppressWarnings(cor(d$Study_Hours, d$Attendance))
    req(!is.na(r))
    tags$p(paste0("ℹ️ Study hours and attendance are correlated (r = ", round(r, 2),
                  "), so they carry overlapping information. The predictions are fine, ",
                  "but don't read each coefficient in isolation."))
  })
  
  # ===================== Data =====================
  
  output$dept_summary <- renderTable({
    d <- filtered()
    need_rows(d)
    d %>%
      group_by(Department) %>%
      summarise(Students = n(),
                `Avg Marks` = round(mean(Marks), 1),
                `Avg Study Hrs` = round(mean(Study_Hours), 1),
                `Avg Attendance` = round(mean(Attendance), 1),
                `Pass Rate (%)` = round(mean(Result == "Pass") * 100, 1),
                .groups = "drop")
  }, striped = TRUE, hover = TRUE)
  
  output$data_table <- renderDT({
    d <- filtered()
    need_rows(d)
    d$Result <- as.character(d$Result)
    
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle("Result",
                  color = styleEqual(c("Pass", "Fail"), c("#27ae60", "#e74c3c")),
                  fontWeight = "bold")
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("filtered_students_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- filtered() %>% mutate(Result = as.character(Result))
      write.csv(out, file, row.names = FALSE)
    }
  )
}

# ---------------- Run App ----------------
shinyApp(ui = ui, server = server)