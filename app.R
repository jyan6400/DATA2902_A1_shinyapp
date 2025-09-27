# Commit 4: load real data + tight cleaning + cat vs cat module
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)

# ---------- Load & clean ----------
# Adjust path if your file is elsewhere
raw <- readxl::read_excel("data/DATA2x02_survey_2025_Responses.xlsx")
dat <- janitor::clean_names(raw)

# Defensive: check required raw columns exist
req_cols <- c(
  "what_final_grade_are_you_aiming_to_achieve_in_data2x02",
  "how_much_alcohol_do_you_consume_each_week",
  "do_you_submit_assignments_on_time",
  "on_average_how_many_hours_each_week_do_you_spend_exercising",
  "how_much_sleep_do_you_get_on_avg_per_day"
)
stopifnot(all(req_cols %in% names(dat)))

# Helpers
to_num <- function(x) readr::parse_number(as.character(x))
is_zero_text <- function(x) str_detect(x, "\\b(none|no|never|zero|nil|don.?t)\\b")

# Build cleaned variables
dat <- dat %>%
  transmute(
    # Keep originals for reference if you want to surface later
    grade_raw      = .data[[req_cols[1]]],
    alcohol_raw    = .data[[req_cols[2]]],
    assignment_raw = .data[[req_cols[3]]],
    exercise_raw   = .data[[req_cols[4]]],
    sleep_raw      = .data[[req_cols[5]]],
    
    # Grade (ordered)
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
    
    # Alcohol level (Low ≤5, High >5); map "none/no/never..." to 0
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
    
    # Sleep group (<7 vs ≥7)
    sleep_group = {
      s <- to_num(sleep_raw)
      s <- if_else(s < 3 | s > 14, NA_real_, s)
      if_else(s >= 7, "Adequate (≥7h)", "Short (<7h)")
    } %>% factor(levels = c("Adequate (≥7h)", "Short (<7h)")),
    
    # Exercise group (<3 vs ≥3)
    exercise_group = {
      e <- suppressWarnings(as.numeric(exercise_raw))
      e <- if_else(e < 0 | e > 100, NA_real_, e)
      if_else(e >= 3, "High (≥3h/wk)", "Low (<3h/wk)")
    } %>% factor(levels = c("High (≥3h/wk)", "Low (<3h/wk)")),
    
    # On-time submission (Yes/No)
    on_time = {
      a <- str_to_lower(str_squish(as.character(assignment_raw)))
      case_when(
        str_detect(a, "always|usually|mostly|most of the time|yes|y\\b|on time") ~ "Yes",
        str_detect(a, "never|rarely|sometimes|often late|no\\b|late")            ~ "No",
        TRUE ~ NA_character_
      )
    } %>% factor(levels = c("No","Yes"))
  )

# Categorical choices (cleaned)
cat_choices <- c(
  "Grade Aim"            = "grade_clean",
  "Alcohol (Low/High)"   = "alcohol_level",
  "Sleep Group"          = "sleep_group",
  "Exercise Group"       = "exercise_group",
  "On-time Submission"   = "on_time"
)

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("DATA2902 Student Survey Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pick two cleaned categorical variables to compare."),
      selectInput("cat1", "Select first categorical variable:", choices = cat_choices),
      selectInput("cat2", "Select second categorical variable:", choices = cat_choices, selected = "grade_clean")
    ),
    mainPanel(
      h4("Contingency Table"),
      tableOutput("contingency"),
      h4("Test Results"),
      verbatimTextOutput("chi_result"),
      h4("Visualisation"),
      plotOutput("cat_plot", height = 360)
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  data_cat <- reactive({
    d <- dat %>%
      select(cat1 = all_of(input$cat1),
             cat2 = all_of(input$cat2)) %>%
      drop_na(cat1, cat2)
    
    validate(
      need(nrow(d) > 0, "No rows after cleaning for this variable pair. Try a different selection.")
    )
    d
  })
  
  output$contingency <- renderTable({
    as.data.frame.matrix(table(data_cat()$cat1, data_cat()$cat2))
  }, rownames = TRUE)
  
  output$chi_result <- renderPrint({
    tab <- table(data_cat()$cat1, data_cat()$cat2)
    # χ² with fallback to Fisher if any expected < 5
    chi_try <- suppressWarnings(chisq.test(tab, correct = FALSE))
    if (any(chi_try$expected < 5)) {
      out <- fisher.test(tab)
      list(
        "Test used" = "Fisher's Exact (small expected counts)",
        "p-value"   = out$p.value
      )
    } else {
      list(
        "Test used"  = "Chi-squared test of independence",
        "Statistic"  = unname(chi_try$statistic),
        "df"         = unname(chi_try$parameter),
        "p-value"    = chi_try$p.value
      )
    }
  })
  
  output$cat_plot <- renderPlot({
    ggplot(data_cat(), aes(x = cat1, fill = cat2)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = names(cat_choices)[match(input$cat1, cat_choices)],
        y = "Proportion",
        fill = names(cat_choices)[match(input$cat2, cat_choices)]
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })
}

shinyApp(ui, server)
