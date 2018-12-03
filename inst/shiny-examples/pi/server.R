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

  })

  output$plot <- renderPlot({
    plot(simulate())
})

  output$time <- renderPrint({

    cat(
      c("Time is" ,"\n" ,system.time(

          if( input$method == "estimate_pi") {
            pi <- estimate_pi(input$B, input$seed)
            plot(pi)
          } else {
            pi <- estimate_pi2(input$B, input$seed)
            plot(pi)
          }
      )[1:3]
      )
)
})

  output$pi <- renderPrint({
    # extract the estimated value
    cat(c("Estimated value of pi:",simulate()$estimated_pi))
  })


