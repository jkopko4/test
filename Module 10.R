install.packages("shiny")

library(shiny)
library(dplyr)
library(ggplot2)

setwd("/Users/johnkopko/Downloads")

income <- read.csv("income.csv")

ui <- fluidPage(
  # App Title
  titlePanel("Module 10 Assignment"),
  # Page Layout
  sidebarLayout(
    # Side Panel
    sidebarPanel(
      # Interactive piece 1: inputID = "subset_income"
      selectInput(inputId = "subset_income",
                  label = "Select Income Level", 
                  choices = unique(income$income)),
      # Interactive piece 2: inputId = "set_yaxis"
      selectInput(inputId = "set_yaxis", 
                  label = "Y Axis", 
                  choices = c(HoursPerWeek = "hours_per_week",
                              CapitalLoss = "capital_loss")),
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation", 
                         label = "Include Occupations",
                         choices = unique(income$occupation),
                         selected = unique(income$occupation))),
    # Main panel
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

server <- function(input, output) {
  # Create a reactive subset of the data
  create_subset <- reactive(income %>%
                              filter(capital_loss > 0,
                                     income == input$subset_income,
                                     occupation %in% input$subset_occupation))
  
  # Render Plot
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                               # Boxplot of x = occupation, y = defined by input
                               geom_boxplot(aes_string(x = "occupation",
                                                       y = input$set_yaxis)) +
                               theme_bw(18) +
                               theme(axis.text.x = element_text(angle = 90,
                                                                hjust = 1)))
}

# ---- Run App ----
shinyApp(ui, server)
