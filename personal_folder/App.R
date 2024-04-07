library(shiny)
library(ggplot2)
library(dplyr)

data <- customer_data

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Customer Segmentation Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("genderInput", "Gender", choices = unique(data$Gender), selected = unique(data$Gender)[1], multiple = TRUE),
      selectInput("maritalStatusInput", "Marital Status", choices = unique(data$Marital_Status), selected = unique(data$Marital_Status)[1], multiple = TRUE),
      sliderInput("ageInput", "Age Range", min = min(data$Age), max = max(data$Age), value = c(min(data$Age), max(data$Age))),
      sliderInput("incomeInput", "Income Level Range", min = min(data$Income_Level), max = max(data$Income_Level), value = c(min(data$Income_Level), max(data$Income_Level)))
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

# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    data %>%
      filter(Gender %in% input$genderInput,
             Marital_Status %in% input$maritalStatusInput,
             Age >= input$ageInput[1], Age <= input$ageInput[2],
             Income_Level >= input$incomeInput[1], Income_Level <= input$incomeInput[2])
  })
  
  output$summary <- renderPrint({
    summary(filteredData())
  })
  
  output$agePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Age)) + geom_histogram(bins = 30) + theme_minimal()
  })
  
  output$incomePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Income_Level)) + geom_histogram(bins = 30) + theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
