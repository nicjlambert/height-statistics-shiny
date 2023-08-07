# Packages
packages_to_check <- c("shiny", "NHANES", "dplyr")

# Set the CRAN mirror to use
CRAN <- "https://cran.csiro.au"

# Function to install and load required packages if not already installed
install_and_load_packages <- function(packages, repos) {
  lapply(packages, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      install.packages(x, dependencies = TRUE, repos = repos)
    }
    library(x, character.only = TRUE)
  })
}

# Check if packages are installed and install them if necessary
install_and_load_packages(packages_to_check, CRAN)

# Load Data
data(NHANES)
height_data <- NHANES %>% 
  filter(!is.na(Height)) %>% 
  select(Height)

# UI
ui <- fluidPage(
  titlePanel("Height Insight App", windowTitle = "Height Insight App"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("runMod", "Run Simulation"),
      sliderInput("sampleSize", "Sample Size:", min = 10, max = nrow(height_data), value = 100, step = 100, ticks=FALSE)
    ),
    
    mainPanel(
      strong("US National Center for Health Statistics"),
      p("The Law of Large Numbers allows us to approximate a population's average height using sample data. For instance, if we measure 100 individuals and find an average height of 170 cm, this gives us an insight into the population's height.", style = "font-family: 'arial'"),
      br(),
      p("Furthermore, as we increase our sample size, say to 100,000 individuals, our estimate becomes more accurate, honing in on the true average height of the entire population.", style = "font-family: 'arial'"),
      br(),
      plotOutput("heightPlot", height = "600px"),
      br(),
      strong("Note"),
      p("Source: survey data collected by the US National Center for Health Statistics (NCHS)"),
      p("Height: is standing height in cm. Reported for participants aged 2 years or older.")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$heightPlot <- renderPlot({
    input$runMod
    isolate({
      sample_data <- slice_sample(height_data, n = input$sampleSize, replace = TRUE)
      avg_height <- cumsum(sample_data$Height) / 1:input$sampleSize
      plot(1:input$sampleSize, avg_height, type = "l", col = "blue", xlab = "Number of Samples", ylab = "Average Height (cm)", main = "Average Height (cm) vs. Number of Samples")
      abline(h = mean(height_data$Height), col = "red", lwd = 2) # True population mean
      legend('topright', inset=0.02, as.expression(c("Average height after n samples", paste("True average height of the population:", round(mean(height_data$Height),1)))), lty = 1, lwd = 2, col=c("blue","red"))
    })
  })
}

shinyApp(ui, server)
