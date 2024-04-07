library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Insurance Coverage Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown - Policy Type
      selectInput("policyFilter", "Select Policy Type:",
                  choices = c("All", unique(as.character(customer_data$Policy_Type)))),
      # Dropdown - variable  against Policy Type
      selectInput("viewVariable", "View variable:",
                  choices = c("Coverage_Amount", "Premium_Amount")),
      # Sliderbins in a histogram
      sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 10)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot <- renderPlot({
    filtered_data <- customer_data %>%
      filter(Policy_Type == input$policyFilter | input$policyFilter == "All")
    
    if(input$viewVariable == "Coverage_Amount") {
      ggplot(filtered_data, aes_string(x = "Policy_Type", y = "Coverage_Amount")) +
        geom_bar(stat = "identity") +
        facet_wrap(~Insurance_Products_Owned) +
        theme_minimal() +
        labs(title = "Coverage Amount by Policy Type and Insurance Products Owned",
             x = "Policy Type", y = "Coverage Amount")
    } else {
      ggplot(filtered_data, aes_string(x = "Policy_Type", y = "Premium_Amount")) +
        geom_histogram(binwidth = input$bins) +
        facet_wrap(~Insurance_Products_Owned) +
        theme_minimal() +
        labs(title = "Premium Amount Distribution by Policy Type and Insurance Products Owned",
             x = "Policy Type", y = "Premium Amount")
    }
  })
  
  observe({
    updateSelectInput(session, "policyFilter",
                      choices = c("All", unique(as.character(customer_data$Policy_Type))))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
