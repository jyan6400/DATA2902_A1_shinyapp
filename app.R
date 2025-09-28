# Polish & UX - page flows like Rmd (no inner scrolls)
library(shiny)
library(bslib)      # theme & layout
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

# HTML badge by p-value (green if < .05, red otherwise, amber if NA)
p_badge <- function(p, label = NULL) {
  col <- if (is.na(p)) "#f39c12" else if (p < .05) "#27ae60" else "#e74c3c"
  lab <- if (is.null(label)) sprintf("p = %s", fmt_p(p)) else label
  HTML(sprintf(
    '<span style="display:inline-block;background:%s;color:white;padding:4px 10px;border-radius:9999px;margin-right:6px;font-weight:600">%s</span>',
    col, lab
  ))
}

# Safe pullers
get_chr <- function(df, nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
get_num <- function(df, nm) if (nm %in% names(df)) to_num(df[[nm]]) else rep(NA_real_, nrow(df))

# ---------- Load & minimally validate ----------
raw <- readxl::read_excel("data/DATA2x02_survey_2025_Responses.xlsx")
dat_raw <- janitor::clean_names(raw)

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
    grade_raw      = .data[[cols_core[1]]],
    alcohol_raw    = .data[[cols_core[2]]],
    assignment_raw = .data[[cols_core[3]]],
    exercise_raw   = .data[[cols_core[4]]],
    sleep_raw      = .data[[cols_core[5]]],
    study_raw      = .data[[cols_core[6]]],
    wam_raw        = .data[[cols_core[7]]],
    
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
    
    # ----------------- CATEGORICALS -----------------
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
    
    # ----------------- NUMERICS -----------------
    study_hours = { h <- to_num(study_raw); if_else(h < 0 | h > 80, NA_real_, h) },
    sleep_hours = { s <- to_num(sleep_raw);  if_else(s < 3 | s > 14, NA_real_, s) },
    wam         = { w <- to_num(wam_raw);    if_else(w < 40 | w > 100, NA_real_, w) },
    age        = if_else(!is.na(age_raw)    & age_raw    >= 15 & age_raw    <= 80,  age_raw,   NA_real_),
    height_cm  = if_else(!is.na(height_raw) & height_raw >=130 & height_raw <= 220, height_raw, NA_real_),
    work_hours = if_else(!is.na(workhrs_raw)& workhrs_raw>= 0  & workhrs_raw<= 80,  workhrs_raw,NA_real_),
    food_spend = if_else(!is.na(foodsp_raw) & foodsp_raw >= 0  & foodsp_raw <=1500, foodsp_raw, NA_real_),
    video_mins = if_else(!is.na(video_raw)  & video_raw  >= 0  & video_raw  <= 600, video_raw,  NA_real_)
  )

# Choice sets (only include columns with data)
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
  
  # light CSS
  header = tags$head(tags$style(HTML("
    .explain {font-size: 0.95rem; color:#34495e}
    .result-box {background:#f8f9fa;border-radius:10px;padding:10px 14px;margin:8px 0;border:1px solid #ecf0f1}
  "))),
  
  nav("Two Categorical",
      # IMPORTANT: make page flow like Rmd (no inner scrollbars)
      layout_sidebar(
        fillable = FALSE,
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
          uiOutput("chi_result_ui")
        ),
        card(
          card_header("Visualisation"),
          plotOutput("cat_plot", height = "420px")
        )
      )
  ),
  
  nav("Numeric vs Categorical",
      layout_sidebar(
        fillable = FALSE,
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
          uiOutput("assumptions_ui")
        ),
        card(
          card_header("Test Results"),
          uiOutput("ttest_ui")
        ),
        card(
          card_header("Visualisation"),
          plotOutput("num_plot", height = "420px")
        )
      )
  ),
  
  nav("Data Dictionary",
      layout_columns(
        fill = FALSE,
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
          tags$li("Cat×Cat uses Fisher or Chi-squared; Num×Cat uses Welch t-test (+ Wilcoxon).")
        ),
        p(em("Note: This is a voluntary convenience sample; results are descriptive and not population estimates."))
      )
    ))
  })
  
  # Guard: different categorical variables
  observeEvent(input$cat1, ignoreInit = TRUE, {
    new_choices <- cat_choices
    if (input$cat1 %in% new_choices) new_choices <- new_choices[new_choices != input$cat1]
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
  
  output$chi_result_ui <- renderUI({
    d <- data_cat()
    tab <- table(d$cat1, d$cat2)
    chi_try <- suppressWarnings(chisq.test(tab, correct = FALSE))
    use_fisher <- any(chi_try$expected < 5)
    
    if (use_fisher) {
      out <- fisher.test(tab)
      tagList(
        div(class="explain",
            tags$b("Hypotheses: "),
            HTML("H<sub>0</sub>: variables are independent; H<sub>1</sub>: variables are associated.")),
        div(class="result-box",
            tags$p(tags$b("Test:"), " Fisher’s exact (small expected counts)"),
            p_badge(out$p.value),
            tags$p(if (out$p.value < .05)
              "Interpretation: evidence suggests dependence (reject H₀)."
              else
                "Interpretation: no evidence of association (fail to reject H₀).")
        )
      )
    } else {
      p <- chi_try$p.value
      N <- sum(tab); V <- sqrt(chi_try$statistic / (N * (min(dim(tab))-1)))
      tagList(
        div(class="explain",
            tags$b("Hypotheses: "),
            HTML("H<sub>0</sub>: variables are independent; H<sub>1</sub>: variables are associated.")),
        div(class="result-box",
            tags$p(tags$b("Test:"), " Chi-squared test of independence"),
            p_badge(p),
            tags$p(sprintf("χ² = %.2f, df = %d; Cramér's V = %.3f",
                           unname(chi_try$statistic), unname(chi_try$parameter), unname(V))),
            tags$p(if (p < .05)
              "Interpretation: evidence suggests dependence (reject H₀)."
              else
                "Interpretation: no evidence of association (fail to reject H₀).")
        )
      )
    }
  })
  
  # Stacked proportion bar (no % labels)
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
    content = function(file) ggsave(file, plot = cat_plot_obj(), width = 8.6, height = 5.0, dpi = 150)
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
  
  output$assumptions_ui <- renderUI({
    df <- data_numgrp()
    gl <- levels(df$group)
    sw1 <- tryCatch(shapiro.test(df$value[df$group==gl[1]])$p.value, error=function(e) NA)
    sw2 <- tryCatch(shapiro.test(df$value[df$group==gl[2]])$p.value, error=function(e) NA)
    vp  <- tryCatch(var.test(value ~ group, data = df)$p.value, error=function(e) NA)
    tagList(
      div(class="explain", tags$b("Assumptions (for Welch t-test context):")),
      div(class="result-box",
          tags$p("Shapiro–Wilk normality per group:"),
          p_badge(sw1, sprintf("%s: SW p=%s", gl[1], fmt_p(sw1))),
          p_badge(sw2, sprintf("%s: SW p=%s", gl[2], fmt_p(sw2))),
          tags$p("Equality of variances (F-test):"),
          p_badge(vp, sprintf("Equal var p=%s", fmt_p(vp))),
          tags$p(em("Welch t-test does not assume equal variances; normality mainly affects very small samples."))
      )
    )
  })
  
  output$ttest_ui <- renderUI({
    df <- data_numgrp()
    gl <- levels(df$group)
    t_out <- tryCatch(t.test(value ~ group, data = df, var.equal = FALSE), error=function(e) NULL)
    w_out <- tryCatch(wilcox.test(value ~ group, data = df, exact = FALSE), error=function(e) NULL)
    
    t_line <- if (!is.null(t_out)) {
      HTML(sprintf("t = %.2f, df = %.1f, 95%% CI = [%.2f, %.2f]",
                   unname(t_out$statistic), unname(t_out$parameter),
                   t_out$conf.int[1], t_out$conf.int[2]))
    } else HTML("Not available")
    w_line <- if (!is.null(w_out)) {
      HTML(sprintf("W = %s", format(unname(w_out$statistic), big.mark=",")))
    } else HTML("Not available")
    
    tagList(
      div(class="explain",
          tags$b("Hypotheses: "),
          HTML(sprintf("H<sub>0</sub>: mean(%s) = mean(%s); H<sub>1</sub>: means differ.", gl[1], gl[2]))),
      div(class="result-box",
          tags$p(tags$b("Welch two-sample t-test")),
          p_badge(if (!is.null(t_out)) t_out$p.value else NA),
          tags$p(t_line),
          tags$p(if (!is.null(t_out) && t_out$p.value < .05)
            "Interpretation: evidence of a difference in means (reject H₀)."
            else
              "Interpretation: no evidence of a difference in means (fail to reject H₀).")
      ),
      div(class="result-box",
          tags$p(tags$b("Wilcoxon rank-sum (nonparametric)")),
          p_badge(if (!is.null(w_out)) w_out$p.value else NA),
          tags$p(w_line),
          tags$p(if (!is.null(w_out) && w_out$p.value < .05)
            "Interpretation: distributions differ (supporting a difference)."
            else
              "Interpretation: no distributional difference detected.")
      )
    )
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
    content  = function(file) ggsave(file, plot = num_plot_obj(), width = 8.6, height = 5.0, dpi = 150)
  )
  
  # ===== Data dictionary =====
  output$data_dict <- renderTable({
    tibble::tibble(
      `Display name` = c(names(cat_map), names(num_map)),
      `Column (app)` = c(unname(cat_map), unname(num_map)),
      `Type` = c(rep("Categorical (cleaned/collapsed)", length(cat_map)),
                 rep("Numeric (clamped to plausible range)", length(num_map))),
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
      filter(`Column (app)` %in% c(unname(cat_choices), unname(num_choices))) %>%
      arrange(factor(`Type`,
                     levels = c("Categorical (cleaned/collapsed)",
                                "Numeric (clamped to plausible range)")),
              `Display name`)
  })
}

shinyApp(ui, server)
