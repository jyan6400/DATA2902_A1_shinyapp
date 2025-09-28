# Commit 7: more variables, same-var guard, data dictionary tab
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
library(purrr)

# ---------- Helpers ----------
to_num <- function(x) readr::parse_number(as.character(x))
is_zero_text <- function(x) str_detect(x, "\\b(none|no|never|zero|nil|don.?t)\\b")
fmt_p <- function(p) ifelse(is.na(p), NA, ifelse(p < .001, "<0.001", sprintf("%.3f", p)))
normtxt <- function(x) {
  x <- tolower(paste(x))
  x <- str_replace_all(x, "[^a-z]+", " ")
  str_squish(x)
}

# Safe puller: returns NA_character_ vector if col missing
get_chr <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
get_num <- function(df, nm) if (nm %in% names(df)) to_num(df[[nm]]) else rep(NA_real_, nrow(df))

# ---------- Load & minimally validate ----------
raw <- readxl::read_excel("data/DATA2x02_survey_2025_Responses.xlsx")
dat_raw <- janitor::clean_names(raw)

# Columns used (some are optional; we check existence)
cols_core <- c(
  "what_final_grade_are_you_aiming_to_achieve_in_data2x02",
  "how_much_alcohol_do_you_consume_each_week",
  "do_you_submit_assignments_on_time",
  "on_average_how_many_hours_each_week_do_you_spend_exercising",
  "how_much_sleep_do_you_get_on_avg_per_day",
  "how_many_hours_a_week_do_you_spend_studying",
  "what_is_your_wam_weighted_average_mark"
)
stopifnot(all(cols_core %in% names(dat_raw)))

# Optional (best effort)
c_gender   <- "what_is_your_gender"
c_year     <- "which_year_of_university_are_you_currently_in"
c_trans    <- "how_do_you_get_to_university"
c_living   <- "what_are_your_current_living_arrangements"
c_payrent  <- "do_you_pay_rent"
c_age      <- "how_old_are_you"
c_height   <- "how_tall_are_you"
c_workhrs  <- "how_many_hours_a_week_on_average_do_you_work_in_paid_employment"
c_foodsp   <- "what_is_the_average_amount_of_money_you_spend_each_week_on_food_beverages"
c_video    <- "how_much_time_do_you_spend_on_short_video_apps_like_tiktok_or_reels_every_day"

# ---------- Clean to analysis set ----------
dat <- dat_raw %>%
  transmute(
    # raw pulls (present by contract)
    grade_raw      = .data[[cols_core[1]]],
    alcohol_raw    = .data[[cols_core[2]]],
    assignment_raw = .data[[cols_core[3]]],
    exercise_raw   = .data[[cols_core[4]]],
    sleep_raw      = .data[[cols_core[5]]],
    study_raw      = .data[[cols_core[6]]],
    wam_raw        = .data[[cols_core[7]]],
    
    # optional raw pulls
    gender_raw     = get_chr(dat_raw, c_gender),
    year_raw       = get_chr(dat_raw, c_year),
    transport_raw  = get_chr(dat_raw, c_trans),
    living_raw     = get_chr(dat_raw, c_living),
    payrent_raw    = get_chr(dat_raw, c_payrent),
    age_raw        = get_num(dat_raw, c_age),
    height_raw     = get_num(dat_raw, c_height),
    workhrs_raw    = get_num(dat_raw, c_workhrs),
    foodsp_raw     = get_num(dat_raw, c_foodsp),
    video_raw      = get_num(dat_raw, c_video),
    
    # ----------------- CATEGORICALS (clean, collapsed) -----------------
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
      case_when(
        !is.na(e) & e < 3  ~ "Low (<3h/wk)",
        !is.na(e) & e >= 6 ~ "High (≥6h/wk)",
        !is.na(e)          ~ "Moderate (3–5h/wk)",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("Low (<3h/wk)","Moderate (3–5h/wk)","High (≥6h/wk)")),
    
    on_time = {
      a <- str_to_lower(str_squish(as.character(assignment_raw)))
      case_when(
        str_detect(a, "always|usually|mostly|most of the time|yes|y\\b|on time") ~ "Yes",
        str_detect(a, "never|rarely|sometimes|often late|no\\b|late")            ~ "No",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("No","Yes")),
    
    gender = {
      g <- normtxt(gender_raw)
      case_when(
        str_detect(g, "\\b(female|woman|girl|she|her|f)\\b") ~ "Female",
        str_detect(g, "\\b(male|man|boy|he|him|m)\\b")       ~ "Male",
        g == "" | is.na(g)                                   ~ NA_character_,
        TRUE                                                 ~ "Other"
      )
    } %>% factor(levels = c("Female","Male","Other")),
    
    year_uni = {
      y <- normtxt(year_raw)
      yn <- suppressWarnings(to_num(year_raw))
      case_when(
        !is.na(yn) & yn <= 1 ~ "Year 1",
        !is.na(yn) & yn == 2 ~ "Year 2",
        !is.na(yn) & yn >= 3 ~ "Year 3+",
        str_detect(y, "first")   ~ "Year 1",
        str_detect(y, "second")  ~ "Year 2",
        str_detect(y, "third|fourth|fifth|sixth|honour|honor|master|phd|postgrad") ~ "Year 3+",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("Year 1","Year 2","Year 3+")),
    
    transport = {
      t <- normtxt(transport_raw)
      case_when(
        str_detect(t, "walk|foot|bike|bicycle|cycle|scooter") ~ "Walk/Bike",
        str_detect(t, "bus|train|tram|light rail|metro|ferry|public") ~ "Public",
        str_detect(t, "car|drive|uber|taxi|ride share|rideshare")    ~ "Car",
        t == "" | is.na(t) ~ NA_character_,
        TRUE ~ "Other"
      )
    } %>% factor(levels = c("Walk/Bike","Public","Car","Other")),
    
    living = {
      l <- normtxt(living_raw)
      case_when(
        str_detect(l, "parent|family home|home with family|with family") ~ "With parents",
        str_detect(l, "share|rent|flat|apartment|unit|room|house")       ~ "Share/Rent",
        str_detect(l, "campus|college|dorm|student hous")                ~ "On-campus",
        l == "" | is.na(l) ~ NA_character_,
        TRUE ~ "Other"
      )
    } %>% factor(levels = c("With parents","Share/Rent","On-campus","Other")),
    
    pays_rent = {
      r <- normtxt(payrent_raw)
      case_when(
        str_detect(r, "^y|yes") ~ "Yes",
        str_detect(r, "^n|no")  ~ "No",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("No","Yes")),
    
    # ----------------- NUMERICS (clamped to plausible ranges) -----------------
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
    },
    age = if_else(!is.na(age_raw) & age_raw >= 15 & age_raw <= 80, age_raw, NA_real_),
    height_cm = if_else(!is.na(height_raw) & height_raw >= 130 & height_raw <= 220, height_raw, NA_real_),
    work_hours = if_else(!is.na(workhrs_raw) & workhrs_raw >= 0 & workhrs_raw <= 80, workhrs_raw, NA_real_),
    food_spend = if_else(!is.na(foodsp_raw) & foodsp_raw >= 0 & foodsp_raw <= 1500, foodsp_raw, NA_real_),
    video_mins = if_else(!is.na(video_raw) & video_raw >= 0 & video_raw <= 600, video_raw, NA_real_)
  )

# Choice sets (only include columns that actually exist & have data)
cat_map <- c(
  "Grade Aim"              = "grade_clean",
  "Alcohol (Low/High)"     = "alcohol_level",
  "Sleep Group"            = "sleep_group",
  "Exercise Group"         = "exercise_group",
  "On-time Submission"     = "on_time",
  "Gender (collapsed)"     = "gender",
  "Year of University"     = "year_uni",
  "Transport Mode"         = "transport",
  "Living Arrangement"     = "living",
  "Pays Rent (Yes/No)"     = "pays_rent"
)
num_map <- c(
  "Study Hours / week"     = "study_hours",
  "Sleep Hours / day"      = "sleep_hours",
  "WAM"                    = "wam",
  "Age (years)"            = "age",
  "Height (cm)"            = "height_cm",
  "Paid Work Hours / week" = "work_hours",
  "Food Spend / week ($)"  = "food_spend",
  "Short-video time / day (mins)" = "video_mins"
)

has_non_all_na <- function(x) sum(!is.na(x)) > 0
cat_choices <- cat_map[names(cat_map)[map_lgl(cat_map, ~ has_non_all_na(dat[[.x]]))]]
num_choices <- num_map[names(num_map)[map_lgl(num_map, ~ has_non_all_na(dat[[.x]]))]]

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
          selectInput("cat2", "Second categorical variable:", choices = cat_choices, selected = names(cat_choices)[1]),
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
          selectInput("num_var", "Numeric variable:", choices = num_choices, selected = names(num_choices)[1]),
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
  nav("Data Dictionary",
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Variables and cleaning rules"),
          tableOutput("data_dict")
        )
      )
  ),
  nav_menu("Help",
           nav_item(actionLink("about_btn", "About / Data cleaning"))
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # About modal
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About / Data cleaning",
      easyClose = TRUE, size = "l",
      tagList(
        p("This app uses selected variables from the DATA2X02 student survey (2025)."),
        tags$ul(
          tags$li("Numeric cleaning: parsed text to numbers; clamped implausible values (e.g., study 0–80 h/wk, sleep 3–14 h/day, WAM 40–100)."),
          tags$li("Categoricals: collapsed to interpretable levels (e.g., Alcohol: Low ≤5 vs High >5; Sleep: ≥7 vs <7 hours)."),
          tags$li("Primary tests: Fisher/Chi-squared (cat×cat) and Welch t-test with Wilcoxon check (num×cat).")
        ),
        p(em("Note: This is a voluntary convenience sample; results are descriptive and not population estimates."))
      )
    ))
  })
  
  # ===== Guard: don't allow selecting the same categorical twice =====
  observeEvent(input$cat1, ignoreInit = TRUE, {
    # remove the first-selected key from second choices
    new_choices <- cat_choices
    if (input$cat1 %in% new_choices) new_choices <- new_choices[new_choices != input$cat1]
    # keep existing selection if still valid, otherwise pick first available
    new_selected <- if (!is.null(input$cat2) && input$cat2 %in% new_choices) input$cat2 else new_choices[[1]]
    updateSelectInput(session, "cat2", choices = new_choices, selected = new_selected)
  })
  
  # ===== Two categorical =====
  data_cat <- reactive({
    validate(need(input$cat1 != input$cat2, "Please choose two different variables."))
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
  
  # ===== Numeric vs categorical =====
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
      df <- df %>% filter(group %in% c(input$lvl_a, input$lvl_b)) %>%
        mutate(group = fct_drop(group))
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
  
  # ===== Data dictionary =====
  output$data_dict <- renderTable({
    tibble::tibble(
      `Display name` = c(
        names(cat_map), names(num_map)
      ),
      `Column (app)` = c(
        unname(cat_map), unname(num_map)
      ),
      `Type` = c(
        rep("Categorical (cleaned/collapsed)", length(cat_map)),
        rep("Numeric (clamped to plausible range)", length(num_map))
      ),
      `Cleaning notes` = c(
        "High Distinction / Distinction / Credit / Pass; other or 'Fail' -> NA",
        "Parsed numeric; 'none/no/never' -> 0; Low ≤5 vs High >5 drinks/wk",
        "Adequate (≥7h) vs Short (<7h) from numeric sleep hours",
        "Low <3h, Moderate 3–5h, High ≥6h per week",
        "Yes/No using robust text patterns (always/usually/… vs late/no)",
        "Collapsed Male/Female/Other; jokes/empty -> Other/NA",
        "Year 1 / Year 2 / Year 3+ from numeric or text",
        "Walk/Bike / Public / Car / Other from text",
        "With parents / Share-Rent / On-campus / Other from text",
        "Yes/No from text",
        "0–80 hours/week kept; else NA",
        "3–14 hours/day kept; else NA",
        "40–100 kept; else NA",
        "15–80 kept; else NA",
        "130–220 kept; else NA",
        "0–80 kept; else NA",
        "0–1500 kept; else NA",
        "0–600 kept; else NA"
      )
    ) %>%
      # Show only variables that passed availability filter
      filter(
        (`Column (app)` %in% c(unname(cat_choices), unname(num_choices)))
      ) %>%
      arrange(factor(`Type`, levels = c("Categorical (cleaned/collapsed)","Numeric (clamped to plausible range)")),
              `Display name`)
  })
}

shinyApp(ui, server)
