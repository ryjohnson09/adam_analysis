library(shiny)   # Web app development
library(haven)   # Read in SAS dataset
library(ggplot2) # Data visualization
library(scales)  # Improve axis labels

# Read in Data -------------------------------
adsl <- read_xpt("adsl.xpt")

# User Interface -----------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Drop down select input
      selectInput("subject_data", "Subject Data", 
                  choices = c("Age" = "AGE",
                              "Baseline BMI" = "BMIBL",
                              "Baseline Height" = "HEIGHTBL",
                              "Baseline Weight" = "WEIGHTBL",
                              "Years of Education" = "EDUCLVL"))),
    
    # Main panel (boxplot)
    mainPanel(plotOutput("boxplot"))
  )
)

# Server Function ---------------------------
server <- function(input, output, session) {
  
  # Create Plot
  output$boxplot <- renderPlot({
    ggplot(data = adsl, aes(x = TRT01A, 
                            y = .data[[input$subject_data]], 
                            fill = TRT01A)) +
      geom_boxplot() +
      geom_jitter(width = 0.3, alpha = 0.4) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15)) +
      labs(
        title = "ADSL Data",
        subtitle = "Comparing Treatment Groups",
        x = "",
        y = attributes(adsl[[input$subject_data]])
      ) +
      scale_x_discrete(labels = label_wrap(10))
  }, res = 100)
}

shinyApp(ui, server)