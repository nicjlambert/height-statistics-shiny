#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(bslib)
library(knitr)

bs_global_theme()

bs_global_add_variables(
  primary = "orange",
  secondary = "teal"
)


# Define UI for application that draws a histogram ------------------------

ui <- fluidPage(bootstrap(),
                
                
                # Header
                titlePanel("Law of Large Numbers"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    
                    h3("Instructions", style = "margin-top: 0px"),
                    helpText("Click Run Model after changing model parameters"),
                    
                    actionButton(inputId = "runMod", "Run Model"),
                    
                    h4("Initial Conditions", style = "margin-top: 25px"),
                    numericInput(inputId = "num",
                                 label = "n Simulations",
                                 value = 1
                    ),
                    
                  ), # end sidebarPanel
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    tabsetPanel(
                      tabPanel(
                        "Plot",
                        
                        h4("Simulation Results"),
                        
                        plotOutput("distPlot", height = "500px"),
                        br(),
                        wellPanel(
                          h4("Graphical Parameters"),
                          fluidRow(
                            column(3, checkboxInput(inputId = "mean.line", label = "Mean Line",
                                                    value = TRUE))
                          )
                        )
                      ),
                      tabPanel(
                        "About",
                        p("This application simulates a fair coin flip to illustrate the
                 impact of law of large numbers dynamics"),
                        a("Law of large numbers (Wikipedia entry).", href = "https://en.wikipedia.org/wiki/Law_of_large_numbers"),
                        br(), br(),
                        strong("Authors"),
                        p(
                          "These tools were authored by ",
                          a("N J. Lambert", href = "https://github.com/nicjlambert"))
                      )
                    )
                  )
                )
)


# Define server logic required to draw a histogram ------------------------


server <- function(input, output, session) {
  
  
  mean <- reactive({
    switch(input$mean.line,
           "mean" = 0.5)
  })
  
  
  observeEvent(input$runMod, {
    
    # instantiate a data frame to store data
    trials <- data.frame(1:input$num)
    
    withProgress(message = "Running trials", value = 0, {
      
      for (i in 1:nrow(trials)) {
        
        rslt <- data.frame(matrix(sample(c(0, 1), nrow(trials), replace = TRUE, prob = c(.5, .5)), ncol = 1))
        rslt <- cbind(rslt, trials)
        colnames(rslt) <- c("Heads", "Trial")
        
        rslt[, "Cum_Heads"] <- cumsum(rslt$Heads)
        
        rslt[, "ratio"] <- rslt$Cum_Heads / rslt$Trial
        
        # Increment the progress bar, and update the detail text.
        incProgress(amount = 1 / input$num, detail = paste("Doing coin flip", i))
      }
    })
    
    output$distPlot <- renderPlot({
      
      plot(rslt$Trial, rslt$ratio,
           type = "b", pch = 19,
           col = "#008080", 
           xlab = "Number of simulated coin flips", ylab = "y", 
           main = "Number of coin flips vs. ratio Heads",
      )
      abline(h = mean(), col = "blue")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

