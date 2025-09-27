# Commit 6: visual polish, About modal, better labels/validation, plot downloads
library(shiny)
library(bslib)      # theme
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)
library(forcats)
library(scales)

# ---------- Load & clean ----------
raw <- readxl::read_excel("data/DATA2x02_survey_2025_Responses.xlsx")
dat <- janitor::clean_names(raw)

req_cols <- c(
  "what_final_grade_are_you_aiming_to_achieve_in_data2x02",
  "how_much_alcohol_do_you_consume_each_week",
  "do_you_submit_assignments_on_time",
  "on_average_how_many_hours_each_week_do_you_spend_exercising",
  "how_much_sleep_do_you_get_on_avg_per_day",
  "how_many_hours_a_week_do_you_spend_studying",
  "what_is_your_wam_weighted_average_mark"
)
stopifnot(all(req_cols %in% names(dat)))

to_num <- function(x) readr::parse_number(as.character(x))
is_zero_text <- function(x) str_detect(x, "\\b(none|no|never|zero|nil|don.?t)\\b")
fmt_p <- function(p) ifelse(is.na(p), NA, ifelse(p < .001, "<0.001", sprintf("%.3f", p)))

dat <- dat %>%
  transmute(
    grade_raw      = .data[[req_cols[1]]],
    alcohol_raw    = .data[[req_cols[2]]],
    assignment_raw = .data[[req_cols[3]]],
    exercise_raw   = .data[[req_cols[4]]],
    sleep_raw      = .data[[req_cols[5]]],
    study_raw      = .data[[req_cols[6]]],
    wam_raw        = .data[[req_cols[7]]],
    
    grade_clean = {
      g <- str_to_lower(str_squish(as.character(grade_raw)))
      case_when(
        str_detect(g, "high\\s*dist|\\bhd\\b|high") ~ "High Distinction",
        str_detect(g, "^dist|\\bd\\b|dist")         ~ "Distinction",
        str_detect(g, "credit|\\bc\\b")             ~ "Credit",
        str_detect(g, "^pass|\\bp\\b|pass")         ~ "Pass",
        str_detect(g, "fail|\\bf\\b")               ~ NA_character_,
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("High Distinction","Distinction","Credit","Pass"), ordered = TRUE),
    
    alcohol_level = {
      txt <- str_to_lower(str_squish(as.character(alcohol_raw)))
      n   <- to_num(txt)
      n   <- if_else(is.na(n) & is_zero_text(txt), 0, n)
      case_when(
        !is.na(n) & n <= 5 ~ "Low",
        !is.na(n) & n >  5 ~ "High",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("Low","High")),
    
    sleep_group = {
      s <- to_num(sleep_raw)
      s <- if_else(s < 3 | s > 14, NA_real_, s)
      if_else(s >= 7, "Adequate (≥7h)", "Short (<7h)")
    } %>% factor(levels = c("Adequate (≥7h)", "Short (<7h)")),
    
    exercise_group = {
      e <- suppressWarnings(as.numeric(exercise_raw))
      e <- if_else(e < 0 | e > 100, NA_real_, e)
      if_else(e >= 3, "High (≥3h/wk)", "Low (<3h/wk)")
    } %>% factor(levels = c("High (≥3h/wk)", "Low (<3h/wk)")),
    
    on_time = {
      a <- str_to_lower(str_squish(as.character(assignment_raw)))
      case_when(
        str_detect(a, "always|usually|mostly|most of the time|yes|y\\b|on time") ~ "Yes",
        str_detect(a, "never|rarely|sometimes|often late|no\\b|late")            ~ "No",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("No","Yes")),
    
    study_hours = {
      h <- to_num(study_raw)
      if_else(h < 0 | h > 80, NA_real_, h)
    },
    sleep_hours = {
      s <- to_num(sleep_raw)
      if_else(s < 3 | s > 14, NA_real_, s)
    },
    wam = {
      w <- to_num(wam_raw)
      if_else(w < 40 | w > 100, NA_real_, w)
    }
  )

cat_choices <- c(
  "Grade Aim"          = "grade_clean",
  "Alcohol (Low/High)" = "alcohol_level",
  "Sleep Group"        = "sleep_group",
  "Exercise Group"     = "exercise_group",
  "On-time Submission" = "on_time"
)
num_choices <- c(
  "Study Hours / week" = "study_hours",
  "Sleep Hours / day"  = "sleep_hours",
  "WAM"                = "wam"
)

# ---------- UI ----------
theme <- bslib::bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = bslib::font_google("Inter")
)

ui <- page_navbar(
  title  = "DATA2902 Student Survey Explorer",
  theme  = theme,
  collapsible = TRUE,
  nav("Two Categorical",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Compare two cleaned categorical variables."),
          selectInput("cat1", "First categorical variable:", choices = cat_choices),
          selectInput("cat2", "Second categorical variable:", choices = cat_choices, selected = "grade_clean"),
          hr(),
          downloadButton("dl_cat_plot", "Download plot (PNG)")
        ),
        card(
          card_header("Contingency Table"),
          tableOutput("contingency")
        ),
        card(
          card_header("Test Results"),
          verbatimTextOutput("chi_result")
        ),
        card(
          card_header("Visualisation"),
          plotOutput("cat_plot", height = "360px")
        )
      )
  ),
  nav("Numeric vs Categorical",
      layout_sidebar(
        sidebar = sidebar(
          helpText("Pick a numeric outcome and a grouping factor (2 levels)."),
          selectInput("num_var", "Numeric variable:", choices = num_choices, selected = "study_hours"),
          selectInput("grp_var", "Grouping variable:", choices = cat_choices, selected = "sleep_group"),
          uiOutput("level_picker"),
          hr(),
          downloadButton("dl_num_plot", "Download plot (PNG)")
        ),
        card(
          card_header("Group Summary"),
          tableOutput("num_summary")
        ),
        card(
          card_header("Assumption Checks"),
          verbatimTextOutput("assumptions")
        ),
        card(
          card_header("Test Results"),
          verbatimTextOutput("ttest_out")
        ),
        card(
          card_header("Visualisation"),
          plotOutput("num_plot", height = "360px")
        )
      )
  ),
  nav_menu("Help",
           nav_item(actionLink("about_btn", "About / Data cleaning"))
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About / Data cleaning",
      easyClose = TRUE, size = "l",
      tagList(
        p("This app uses selected variables from the DATA2X02 student survey (2025)."),
        tags$ul(
          tags$li("Numeric cleaning: parsed text to numbers; clamped implausible values (e.g., study 0–80 h/wk, sleep 3–14 h/day, WAM 40–100)."),
          tags$li("Categoricals: collapsed to interpretable levels (e.g., Alcohol Low ≤5 vs High >5)."),
          tags$li("Assumption checks are reported for context; primary tests are Fisher/Chi-squared (cat×cat) and Welch t-test (num×cat).")
        ),
        p(em("Note: This is a voluntary convenience sample; results are descriptive and not population estimates."))
      )
    ))
  })
  
  # ===== Tab 1: Two categorical =====
  data_cat <- reactive({
    d <- dat %>%
      select(cat1 = all_of(input$cat1),
             cat2 = all_of(input$cat2)) %>%
      drop_na(cat1, cat2)
    validate(need(nrow(d) > 0, "No rows after cleaning for this variable pair. Try another selection."))
    d
  })
  
  output$contingency <- renderTable({
    as.data.frame.matrix(table(data_cat()$cat1, data_cat()$cat2))
  }, rownames = TRUE)
  
  output$chi_result <- renderPrint({
    tab <- table(data_cat()$cat1, data_cat()$cat2)
    chi_try <- suppressWarnings(chisq.test(tab, correct = FALSE))
    if (any(chi_try$expected < 5)) {
      out <- fisher.test(tab)
      cat("Test used: Fisher's Exact (small expected counts)\n",
          "p-value:", fmt_p(out$p.value), "\n")
    } else {
      cat("Test used: Chi-squared test of independence\n",
          "Statistic:", round(unname(chi_try$statistic), 3), "\n",
          "df:", unname(chi_try$parameter), "\n",
          "p-value:", fmt_p(chi_try$p.value), "\n")
    }
  })
  
  cat_plot_obj <- reactive({
    ggplot(data_cat(), aes(x = cat1, fill = cat2)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = names(cat_choices)[match(input$cat1, cat_choices)],
        y = "Proportion",
        fill = names(cat_choices)[match(input$cat2, cat_choices)]
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1),
            panel.grid.minor = element_blank())
  })
  
  output$cat_plot <- renderPlot({ cat_plot_obj() })
  output$dl_cat_plot <- downloadHandler(
    filename = function() paste0("cat_plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = cat_plot_obj(), width = 8, height = 4.5, dpi = 150)
    }
  )
  
  # ===== Tab 2: Numeric vs Categorical =====
  output$level_picker <- renderUI({
    g <- dat[[input$grp_var]]
    levs <- levels(factor(g))
    if (length(levs) > 2) {
      tagList(
        selectInput("lvl_a", "Level A:", choices = levs, selected = levs[1]),
        selectInput("lvl_b", "Level B:", choices = levs, selected = levs[2])
      )
    } else NULL
  })
  
  data_numgrp <- reactive({
    x <- dat[[input$num_var]]
    g <- dat[[input$grp_var]]
    df <- tibble::tibble(value = x, group = g) %>% drop_na()
    levs <- levels(factor(df$group))
    if (length(levs) > 2) {
      req(input$lvl_a, input$lvl_b, input$lvl_a != input$lvl_b)
      df <- df %>% filter(group %in% c(input$lvl_a, input$lvl_b)) %>% mutate(group = fct_drop(group))
    }
    validate(need(n_distinct(df$group) == 2, "Please choose two distinct levels for the grouping variable."))
    df
  })
  
  output$num_summary <- renderTable({
    data_numgrp() %>%
      group_by(group) %>%
      summarise(
        n = n(),
        mean   = mean(value),
        sd     = sd(value),
        median = median(value),
        iqr    = IQR(value),
        .groups = "drop"
      )
  }, digits = 2)
  
  output$assumptions <- renderPrint({
    df <- data_numgrp()
    sw <- by(df$value, df$group, shapiro.test)
    vt <- var.test(value ~ group, data = df)
    cat("Shapiro–Wilk normality (per group):\n")
    purrr::iwalk(sw, ~cat(" ", .y, ": W=", round(unname(.x$statistic),3),
                          ", p=", fmt_p(.x$p.value), "\n", sep=""))
    cat("\nF-test equal variances:\n F=", round(unname(vt$statistic),3),
        ", df1=", vt$parameter[1], ", df2=", vt$parameter[2],
        ", p=", fmt_p(vt$p.value), "\n", sep = "")
  })
  
  output$ttest_out <- renderPrint({
    df <- data_numgrp()
    t_out <- t.test(value ~ group, data = df, var.equal = FALSE)
    w_out <- wilcox.test(value ~ group, data = df, exact = FALSE)
    cat("Welch two-sample t-test:\n",
        " t = ", round(unname(t_out$statistic),2),
        ", df = ", round(unname(t_out$parameter),1),
        ", 95% CI = [", round(t_out$conf.int[1],2), ", ", round(t_out$conf.int[2],2), "]",
        ", p = ", fmt_p(t_out$p.value), "\n", sep = "")
    cat("\nWilcoxon rank-sum:\n",
        " W = ", format(unname(w_out$statistic), big.mark=","),
        ", p = ", fmt_p(w_out$p.value), "\n", sep = "")
  })
  
  num_plot_obj <- reactive({
    df <- data_numgrp()
    ggplot(df, aes(group, value, fill = group)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA, colour = "grey30") +
      geom_jitter(width = 0.15, alpha = 0.45, size = 1.5) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = names(cat_choices)[match(input$grp_var, cat_choices)],
        y = names(num_choices)[match(input$num_var, num_choices)]
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$num_plot <- renderPlot({ num_plot_obj() })
  output$dl_num_plot <- downloadHandler(
    filename = function() paste0("num_plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = num_plot_obj(), width = 8, height = 4.5, dpi = 150)
    }
  )
}

shinyApp(ui, server)
