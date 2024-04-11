library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("work_customer_segmentation_data.csv")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("genderInput", "Gender", choices = unique(data$Gender), selected = unique(data$Gender)[1], multiple = TRUE),
      selectInput("maritalStatusInput", "Marital Status", choices = unique(data$Marital.Status), selected = unique(data$Marital.Status)[1], multiple = TRUE),
      sliderInput("ageInput", "Age Range", min = min(data$Age), max = max(data$Age), value = c(min(data$Age), max(data$Age)), step = 1),
      sliderInput("incomeInput", "Income Level Range", min = min(data$Income.Level), max = max(data$Income.Level), value = c(min(data$Income.Level), max(data$Income.Level)))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Age Distribution", plotOutput("agePlot")),
                  tabPanel("Income Distribution", plotOutput("incomePlot"))
      )
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(Gender %in% input$genderInput,
             Marital.Status %in% input$maritalStatusInput,
             Age >= input$ageInput[1], Age <= input$ageInput[2],
             Income.Level >= input$incomeInput[1], Income.Level <= input$incomeInput[2])
  })
  
  output$summary <- renderPrint({
    summary(filteredData())
  })
  
  output$agePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Age)) + geom_histogram(bins = 30) + theme_minimal()
  })
  
  output$incomePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Income.Level)) + geom_histogram(bins = 30) + theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
