#' @title: "Law of Large Numbers Simulation"
#' n.j.lambert
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

# Package Management ------------------------------------------------------

install_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

install_package("shiny") # build interactive web applications with R
install_package("bslib")

# UI Definition -----------------------------------------------------------

bs_global_theme()
bs_global_add_variables(primary = "orange", secondary = "teal")

ui <- fluidPage(bs_theme_dependencies(theme = bs_global_theme()),
                titlePanel("Law of Large Numbers"),
                sidebarLayout(
                  sidebarPanel(
                    h3("Instructions"),
                    helpText("Click 'Run Model' after changing parameters"),
                    actionButton("runMod", "Run Model"),
                    h4("Initial Conditions"),
                    numericInput("num", "n Simulations", 1)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",
                               h4("Simulation Results"),
                               plotOutput("distPlot", height = "500px"),
                               wellPanel(
                                 h4("Graphical Parameters"),
                                 fluidRow(
                                   column(3, checkboxInput("mean.line", "Mean Line", TRUE))
                                 )
                               )
                      ),
                      tabPanel("About",
                               p("Simulates a fair coin flip to illustrate the law of large numbers."),
                               a("Law of large numbers (Wikipedia)", href = "https://en.wikipedia.org/wiki/Law_of_large_numbers"),
                               br(), br(),
                               strong("Author"),
                               a("N J. Lambert", href = "https://github.com/nicjlambert")
                      )
                    )
                  )
                )
)

# Server Definition -------------------------------------------------------

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