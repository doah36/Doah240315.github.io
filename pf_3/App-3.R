library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Dataset Variable Descriptions"),
  DTOutput("columnDescriptionTable")
)

server <- function(input, output) {
  column_descriptions <- data.frame(
    Variable = c("Customer ID", "Age", "Gender", "Marital Status", "Education Level",
                 "Occupation", "Income Level", "Behavioral Data", "Insurance Products Owned",
                 "Coverage Amount", "Premium Amount", "Policy Type", "Segmentation Group"),
    Description = c("Unique identifier for each customer", 
                    "Age of the customer", 
                    "Gender of the customer",
                    "Marital status of the customer (e.g., single, married, etc.)",
                    "Highest level of education attained by the customer",
                    "Profession or type of job held by the customer",
                    "The income category or range the customer falls into",
                    "Data collected on the customer's behavior, possibly including purchasing habits or policy usage",
                    "Types of insurance products owned by the customer",
                    "The total amount of insurance coverage the customer has",
                    "The payment amount required to maintain the insurance policy",
                    "The type of insurance policy the customer has purchased",
                    "The market segment to which the customer belongs, based on certain characteristics or behaviors")
  )
  
output$columnDescriptionTable <- renderDT({
  datatable(column_descriptions, options = list(pageLength = 13, autoWidth = TRUE))
  })
}

shinyApp(ui = ui, server = server)


