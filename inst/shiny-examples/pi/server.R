#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ptds2018hw4g4)

shinyServer(function(input, output) {

  simulate <- reactive({
    # simulate pi and measure the time here
    if (input$method == "estimate_pi") {
      estimate_pi(input$seed, input$B)
    } else {estimate_pi2(input$seed, input$B)}
  })

  output$plot <- renderPlot({
    # plot pi
    ...
  })

  output$time <- renderText({
    # extract the time of the execution
    ...
  })

  output$pi <- renderText({
    # extract the estimated value
    ...
  })

})
