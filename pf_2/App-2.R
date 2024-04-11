library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

data <- read_csv("work_customer_segmentation_data.csv")

ui <- fluidPage(
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
  data <- readr::read_csv("work_customer_segmentation_data.csv")
  
  observeEvent(input$analyze, {
    output$relationshipPlot <- renderPlot({
      demographic <- rlang::sym(input$demographicVar)
      financial <- rlang::sym(input$financialVar)
      
      # Conditional plotting based on the selected demographic variable
      if (input$demographicVar == "Age") {
        # Age: Use line plot for continuous variable
        age_data <- data %>%
          group_by(!!demographic) %>%
          summarise(MeanFinancial = mean(!!financial, na.rm = TRUE))
        
        ggplot(age_data, aes(x = !!demographic, y = MeanFinancial)) +
          geom_line() +
          labs(title = paste("Average", input$financialVar, "by Age"),
               x = "Age", y = paste("Average", input$financialVar))
      } else if (input$demographicVar == "Income Level") {
        # Income Level: Use bar plot for categorical variable
        income_data <- data %>%
          group_by(!!demographic) %>%
          summarise(MeanFinancial = mean(!!financial, na.rm = TRUE))
        
        ggplot(income_data, aes(x = !!demographic, y = MeanFinancial, fill = !!demographic)) +
          geom_col() +
          labs(title = paste("Average", input$financialVar, "by Income Level"),
               x = "Income Level", y = paste("Average", input$financialVar))
      } else {
        # Gender, Education Level, Marital Status: Use boxplot for categorical variables
        ggplot(data, aes(x = factor(!!demographic), y = !!financial, fill = factor(!!demographic))) +
          geom_boxplot() +
          labs(title = paste("Distribution of", input$financialVar, "across", input$demographicVar),
               x = input$demographicVar, y = input$financialVar)
      }
    })
    
    output$statSummary <- renderPrint({
      if (input$demographicVar %in% c("Age", "Income Level")) {
        # Statistical summary for numeric variables
        res <- cor.test(data[[input$demographicVar]], data[[input$financialVar]], method = "pearson")
        cat("Correlation between", input$demographicVar, "and", input$financialVar, ":\n")
        cat("Correlation Coefficient:", res$estimate, "\nP-value:", res$p.value, "\n")
      } else {
        # Statistical summary for categorical variables
        cat("Frequency Table for", input$demographicVar, ":\n")
        print(table(data[[input$demographicVar]]))
      }
    })
  })
}

shinyApp(ui = ui, server = server)


