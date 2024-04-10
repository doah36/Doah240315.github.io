library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("work_customer_segmentation_data.csv")
View(data)


ui <- fluidPage(
  titlePanel("Demographics and Insurance Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("demographicVar", "Select Demographic Variable:", 
                  choices = c("Gender", "Age", "Income Level", "Education Level", "Marital Status")),
      selectInput("financialVar", "Select Financial Variable:", 
                  choices = c("Coverage Amount", "Premium Amount")),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      plotOutput("relationshipPlot"),
      verbatimTextOutput("statSummary")
    )
  )
)

server <- function(input, output) {
  data <- read_csv("work_customer_segmentation_data.csv")
  
  observeEvent(input$analyze, {
    output$relationshipPlot <- renderPlot({
      demographic <- sym(input$demographicVar)
      financial <- sym(input$financialVar)
      
      if (input$demographicVar %in% c("Age", "Income Level")) {
        ggplot(data, aes(x = !!demographic, y = !!financial)) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(title = paste("Relationship between", input$demographicVar, "and", input$financialVar),
               x = input$demographicVar, y = input$financialVar)
      } else {  # Categorical variables
        ggplot(data, aes(x = factor(!!demographic), y = !!financial, fill = factor(!!demographic))) +
          geom_boxplot() +
          labs(title = paste("Distribution of", input$financialVar, "across", input$demographicVar),
               x = input$demographicVar, y = input$financialVar)
      }
    })
    
    output$statSummary <- renderPrint({
      if (input$demographicVar %in% c("Age", "Income Level")) {
        with(data, cor.test(data[[input$demographicVar]], data[[input$financialVar]], method = "pearson"))
      } else {
        with(data, summary(aov(as.formula(paste(input$financialVar, '~', input$demographicVar)))))
      }
    })
  })
}

shinyApp(ui = ui, server = server)


