# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(shinyWidgets)
library(janitor)
library(tidyr)

# Load Data and clean column names
if (!file.exists("DatasetSM.csv")) {
  stop("Dataset file not found. Please ensure 'DatasetSM.csv' is in the correct directory.")
}

df <- read.csv("DatasetSM.csv")
if (nrow(df) == 0) {
  stop("The dataset is empty.")
}

df <- clean_names(df)

# UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Home", 
             fluidRow(
               column(12, 
                      h2("Welcome to the Smart Grid Perception Dashboard"),
                      p("Objectives of the study:"),
                      tags$ul(
                        tags$li("To evaluate how Generation Z and Millennials differ in their energy consumption patterns, willingness to adopt smart grid technologies, and perceptions of various energy-saving initiatives such as smart meters, rooftop PV systems, electric vehicles (EVs), and vehicle-to-grid (V2G) technology."),
                        tags$li("To investigates the relationship between home ownership status, home size, and energy consumption profiles, and how these factors influence the adoption of energy-efficient technologies"),
                        tags$li("To understand respondents’ comfort levels with sharing energy consumption data and participating in load control programs, and to identify any generational differences in these perceptions.")
                      ),
                      hr(),
                      h4("Dataset Summary"),
                      verbatimTextOutput("dataset_summary")
               )
             )
    ),
    
    tabPanel("Perception Analysis", 
             fluidRow(
               column(12, 
                      h3("Perception of Smart Grid and Smart Appliances"),
                      plotlyOutput("perception_plot")
               )
             )
    ),
    
    tabPanel("Inferential Analysis", 
             tabsetPanel(
               tabPanel("Chi-Square Test", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("chi_var1", "Select First Categorical Variable:", choices = colnames(df)),
                            selectInput("chi_var2", "Select Second Categorical Variable:", choices = colnames(df)),
                            actionButton("run_chi_sq", "Run Chi-Square Test")
                          ),
                          mainPanel(
                            h4("Chi-Square Test Result"),
                            verbatimTextOutput("chi_sq_result")
                          )
                        )
               ),
               tabPanel("T-Test",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("t_var1", "Select Numerical Variable:", choices = colnames(df)),
                            selectInput("t_var2", "Select Categorical Variable (2 levels):", choices = colnames(df)),
                            actionButton("run_t_test", "Run T-Test")
                          ),
                          mainPanel(
                            h4("T-Test Result"),
                            verbatimTextOutput("t_test_result")
                          )
                        )
               ),
               tabPanel("ANOVA",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("anova_var1", "Select Numerical Variable:", choices = colnames(df)),
                            selectInput("anova_var2", "Select Categorical Variable:", choices = colnames(df)),
                            actionButton("run_anova", "Run ANOVA")
                          ),
                          mainPanel(
                            h4("ANOVA Result"),
                            verbatimTextOutput("anova_result")
                          )
                        )
               )
             )
    ),
    
    tabPanel("Demographic Visualization", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("demo_var", "Select Demographic Variable:", choices = colnames(df))
               ),
               mainPanel(
                 plotlyOutput("demo_plot"),
                 DTOutput("demo_table")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dataset summary
  output$dataset_summary <- renderPrint({
    summary(df)
  })
  
  # Perception plot
  output$perception_plot <- renderPlotly({
    melted_df <- df %>% 
      select(contains("perception")) %>% 
      pivot_longer(cols = everything(), names_to = "Perception", values_to = "Score")
    plot_ly(melted_df, x = ~Perception, y = ~Score, type = "box") %>%
      layout(title = "Perception of Smart Grid and Smart Appliances")
  })
  
  # Chi-Square test
  observeEvent(input$run_chi_sq, {
    output$chi_sq_result <- renderPrint({
      req(input$chi_var1, input$chi_var2)
      tryCatch({
        chisq.test(table(df[[input$chi_var1]], df[[input$chi_var2]]))
      }, error = function(e) {
        return(paste("Error:", e$message))
      })
    })
  })
  
  # T-Test
  observeEvent(input$run_t_test, {
    output$t_test_result <- renderPrint({
      req(input$t_var1, input$t_var2)
      if (!is.numeric(df[[input$t_var1]])) {
        return("Error: The first variable must be numeric.")
      }
      if (length(unique(df[[input$t_var2]])) != 2) {
        return("Error: The second variable must have exactly two levels.")
      }
      tryCatch({
        t.test(as.integer(df[[input$t_var1]]) ~ df[[input$t_var2]])
      }, error = function(e) {
        return(paste("Error:", e$message))
      })
    })
  })
  
  # ANOVA
  observeEvent(input$run_anova, {
    output$anova_result <- renderPrint({
      req(input$anova_var1, input$anova_var2)
      tryCatch({
        aov_result <- aov(df[[input$anova_var1]] ~ df[[input$anova_var2]])
        summary(aov_result)
      }, error = function(e) {
        return(paste("Error:", e$message))
      })
    })
  })
  
  # Demographic plot
  output$demo_plot <- renderPlotly({
    req(input$demo_var)
    plot_ly(df, x = ~get(input$demo_var), type = "histogram") %>%
      layout(title = paste("Distribution of", input$demo_var))
  })
  
  # Demographic table
  output$demo_table <- renderDT({
    req(input$demo_var)
    df %>% 
      count(get(input$demo_var)) %>% 
      mutate(Percentage = round(n / sum(n) * 100, 2))
  })
}

# Run the app
shinyApp(ui, server)