#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a plot of the approximate integral
ui <- fluidPage(

  # Application title
  titlePanel("Pi estimation"),

  # Sidebar with text input for the function to integrate, numeric inputs for the range of integration and number of Monte-Carlo replications
  sidebarLayout(
    sidebarPanel(
      textInput("fun", "Function to integrate:","sin(10*x)*exp(cos(x))"),
      numericInput("seed", "Should be a positive number", 0, min = -100, max = 100),
      numericInput("B", "", 10^5,
                   min = 100, max = 10^9),
      actionButton("button", "Compute Pi")
    ),

    # Show a plot of the integrated area under the function
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw the integration plot
server <- function(input, output) {

  a <- eventReactive(input$button, {
    estimate_pi(x_range = c(input$low, input$up),
           fun = input$fun, B = input$B)
  })

  output$distPlot <- renderPlot({
    plot(a())
  })

}

# Run the application
shinyApp(ui = ui, server = server)
