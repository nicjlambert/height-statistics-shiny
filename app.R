# Package Management ----
packages <- c("shiny", "NHANES", "dplyr")

lapply(packages, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
})

# Data Acquisition ----
data(NHANES)
height_data <- NHANES %>% 
  filter(!is.na(Height)) %>% 
  select(Height)

# UI Definition ----
ui <- fluidPage(
  titlePanel("Law of Large Numbers: Height Data"),
  sidebarLayout(
    sidebarPanel(
      actionButton("runMod", "Run Simulation"),
      sliderInput("sample_size", "Sample Size:", min = 10, max = nrow(height_data), value = 100, step = 10)
    ),
    mainPanel(
      plotOutput("heightPlot", height = "500px")
    )
  )
)

# Server Definition ----
server <- function(input, output, session) {
  
  output$heightPlot <- renderPlot({
    input$runMod
    isolate({
      sample_data <- sample_n(height_data, input$sample_size)
      avg_height <- cumsum(sample_data$Height) / 1:input$sample_size
      
      plot(1:input$sample_size, avg_height, type = "l", col = "blue", xlab = "Number of Samples", ylab = "Average Height", main = "Average Height vs. Number of Samples")
      abline(h = mean(height_data$Height), col = "red", lwd = 2) # True population mean
    })
  })
}

shinyApp(ui, server)
