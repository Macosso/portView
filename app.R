#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Import libraries
library(DT)
library(scales)
library(treemapify)
library(xts)
library(zoo)
library(plotly)
library(shiny)
library(shinydashboard)
library(plotly)



# Import User Interface
source("ui.R")

# Import Server Back-End
source("server.R")


# Run the application 
shinyApp(ui = ui, server = server)
