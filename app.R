#' @title: "Law of Large Numbers Simulation"
#' 
#'This is a Shiny web application that demonstrates the concept of the law of large 
#'numbers. The law of large numbers states that as the number of trials in a 
#'probability experiment increases, the average of the results will tend towards 
#'the expected value of the distribution.
#'
#'In this application, a fair coin flip is simulated to show how the law of large 
#'numbers works in practice. The user can specify the number of simulations they 
#'want to run and whether they want to show the mean line on the plot. Once the 
#'user clicks the "Run Model" button, the results of the simulations will be displayed 
#'in a plot showing the number of coin flips versus the ratio of heads. The expected 
#'value for the ratio of heads in a fair coin flip is 0.5, which is indicated by 
#'the mean line.

# Package Management ----

install_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_package("shiny") # build interactive web applications with R
install_package("bslib")

# UI Definition ----

sidebar_UI <- function() {
  sidebarPanel(
    h3("Simulation Control Panel"),
    helpText("This application demonstrates the Law of Large Numbers using a fair coin flip simulation. To begin:",
             tags$ul(
               tags$li("Select the number of simulations to run"),
               tags$li("Choose whether to display the expected mean line"),
               tags$li("Click 'Run Simulation' to start")
             )),
    numericInput("num", "Number of Simulations:", 1, min = 1),
    checkboxInput("mean.line", "Display Mean Line (0.5)", TRUE),
    actionButton("runMod", "Run Simulation")
  )
}

main_UI <- function() {
  mainPanel(
    tabsetPanel(
      tabPanel("Simulation Plot",
               h4("Simulation Results"),
               plotOutput("distPlot", height = "500px")
      ),
      tabPanel("About",
               h3("About the Simulation"),
               p("The Law of Large Numbers states that as the number of trials in a probability experiment increases, 
                 the experimental probability tends towards the theoretical probability. In this simulation, we demonstrate this concept 
                 using the example of a fair coin flip, which has a theoretical probability of 0.5 for both 'Heads' and 'Tails'. 
                 As the number of flips increases, the ratio of 'Heads' to total flips will converge towards 0.5."),
               br(),
               h4("References and More Information"),
               a("Law of large numbers (Wikipedia)", href = "https://en.wikipedia.org/wiki/Law_of_large_numbers"),
               br(), br(),
               h4("Author"),
               a("N J. Lambert", href = "https://github.com/nicjlambert")
      )
    )
  )
}

ui <- fluidPage(
  bs_theme_dependencies(theme = bs_global_theme()),
  titlePanel("Fair Coin Flip: A Law of Large Numbers Simulation"),
  sidebarLayout(sidebar_UI(), main_UI())
)

# Server Definition ----

server <- function(input, output) {
 
  mean <- reactive({
    if (input$mean.line) 0.5
  })

  observeEvent(input$runMod, {
    flips <- data.frame(1:input$num)
    for (i in 1:nrow(flips)) {
      rslt <- data.frame(matrix(sample(c(0, 1), nrow(flips), replace = TRUE, prob = c(.5, .5)), ncol = 1)) 
      rslt <- cbind(rslt, flips)
      colnames(rslt) <- c("Heads", "Flips")
      rslt[, "Cum_Heads"] <- cumsum(rslt$Heads)
      rslt[, "ratio"] <- rslt$Cum_Heads / rslt$Flips
    }
    output$distPlot <- renderPlot({
      plot(rslt$Flips, rslt$ratio, type = "b", pch = 19, col = "#008080", xlab = "Number of Flips", ylab = "Ratio of Heads", main = "Number of Flips vs. Ratio of Heads")
      abline(h = mean(), col = "orange")
    })
  })

}

shinyApp(ui, server)
