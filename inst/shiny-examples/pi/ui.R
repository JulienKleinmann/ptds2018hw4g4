#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Pi Estimation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("method", "Choose a method:",
      list(`Estimation Pi 1` = "estimate_pi",
           `Estimate Pi 2` = "estimate_pi2")),
      
      numericInput("seed", "Seed", 0, min = 0, max = 500),
      
      sliderInput("B", "B", min = 0, max = 10^8, 100)
      
    ),
    
    mainPanel(
      
      plotOutput("plot"),
      
      textOutput("time"),
      
      textOutput("pi")
    )
  )
))