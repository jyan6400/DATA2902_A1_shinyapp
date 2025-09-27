library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(ggplot2)

# ---- Load & clean data ----
raw <- readxl::read_excel("data/DATA2x02_survey_2025_Responses.xlsx")
dat <- janitor::clean_names(raw)

# Keep only the categorical variables we want for now
dat <- dat %>%
  select(
    grade_raw = what_final_grade_are_you_aiming_to_achieve_in_data2x02,
    alcohol_raw = how_much_alcohol_do_you_consume_each_week,
    assignment_raw = do_you_submit_assignments_on_time,
    exercise_raw = on_average_how_many_hours_each_week_do_you_spend_exercising,
    sleep_raw = how_much_sleep_do_you_get_on_avg_per_day
  )

# ---- UI ----
ui <- fluidPage(
  titlePanel("DATA2902 Student Survey Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Select first categorical variable:",
                  choices = c("Alcohol Consumption" = "alcohol_raw",
                              "Grade Aim" = "grade_raw",
                              "Assignment Submission" = "assignment_raw",
                              "Exercise Group" = "exercise_raw",
                              "Sleep Group" = "sleep_raw")),
      selectInput("cat2", "Select second categorical variable:",
                  choices = c("Grade Aim" = "grade_raw",
                              "Alcohol Consumption" = "alcohol_raw",
                              "Assignment Submission" = "assignment_raw",
                              "Exercise Group" = "exercise_raw",
                              "Sleep Group" = "sleep_raw"),
                  selected = "grade_raw")
    ),
    mainPanel(
      h4("Contingency Table"),
      tableOutput("contingency"),
      h4("Test Results"),
      verbatimTextOutput("chi_result"),
      h4("Visualisation"),
      plotOutput("cat_plot")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  data_cat <- reactive({
    dat %>%
      select(cat1 = all_of(input$cat1),
             cat2 = all_of(input$cat2)) %>%
      drop_na()
  })
  
  output$contingency <- renderTable({
    table(data_cat()$cat1, data_cat()$cat2)
  }, rownames = TRUE)
  
  output$chi_result <- renderPrint({
    tab <- table(data_cat()$cat1, data_cat()$cat2)
    chi <- suppressWarnings(chisq.test(tab))
    if (any(chi$expected < 5)) {
      list("Test used" = "Fisher’s Exact", fisher.test(tab))
    } else {
      list("Test used" = "Chi-squared", chi)
    }
  })
  
  output$cat_plot <- renderPlot({
    ggplot(data_cat(), aes(x = cat1, fill = cat2)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(x = input$cat1, fill = input$cat2)
  })
}

shinyApp(ui, server)
