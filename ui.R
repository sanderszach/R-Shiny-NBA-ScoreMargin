
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  uiOutput('style'),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  # Application title
  titlePanel("Basketball Game Trends"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput('date_selector')
      ,uiOutput('game_selector')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput('homescore_box')
      ,plotOutput("mainPlot")

      #,tableOutput("table")
      #,textOutput("text")
    )
  )
))
