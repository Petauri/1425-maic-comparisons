#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(data.table)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(
        title = "Delta Hat standard simple CE model"
    ),
    sidebar = dashboardSidebar(
        minified = TRUE,
        h4("Hi there I'm some settings")
    ),
    body = dashboardBody(
        h1("Hi there, I'm a model")
    ),
    controlbar = dashboardControlbar(
        h4("Hi there, I'm some settings")
    ),
    title = "DH simple shiny",
    skin = "green-light",
    scrollToTop = TRUE
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
