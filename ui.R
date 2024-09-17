library(shiny)
library(shinydashboard)
library(DT)
library(plotly)


title <- tags$a(href = "https://www.github.com/macosso",
                tags$img(src = "lambda.png", height = '50', width = '50'),
                "portView")



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "portView",
                  tags$li(class = "dropdown",
                          tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
                                 icon("address-card"), "ABOUT US",
                                 tags$ul(class = "dropdown-menu",
                                         tags$li("Content for panel 1")))),
                  tags$li(class = "dropdown",
                          tags$a(href = "https://ww.github.com/macosso", class = "dropdown-toggle", `data-toggle` = "dropdown",
                                 icon("envelope"), "Get in Touch",
                                 tags$ul(class = "dropdown-menu",
                                         tags$li("chart-mixed"))))
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Portfolio Overview", tabName = "portview", icon = icon("dashboard")),
      menuItem("Portfolio Performance", tabName = "portperf", icon = icon("dashboard")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidPage(
                tabsetPanel(#title = "IMPORT DATA", width = 12,
                       tabPanel("Import Data",
                                fluidRow(
                                  box(title = "DATA DOWNLOAD SETTINGS", solidHeader = TRUE, width = 3,
                                      shiny::dateRangeInput("inputDate", "select Dates", 
                                                     start = as.character(Sys.Date()-367),
                                                     width = "100%"),
                                      shiny::actionButton("downloadsymbols", "IMPORT ASSETS DATA", width = "100%"),
                                      shiny::textInput("symbol correction", "CORRECT SYMBOLS",
                                                placeholder = "key1:value1, key2:value2", width = "100%"),
                                      shiny::actionButton("submitcorrection", "Submit", width = "100%"),
                                      shiny::textInput("excSymb", "Exclude Symbols",
                                                placeholder = "Type symbols to Exlude", width = "100%"),
                                      shiny::actionButton("submitexclusion", "submit", width = "100%")
                                      ),
                                  box(title = "LATEST CHANGE",width = 9, height = 9, solidHeader = TRUE,
                                      shiny::radioButtons("period", "SELECT RETURN PERIOD", 
                                                         choices = c("1D", "7D", "MTD", "YTD", "1M", "3M", "12M", "3Y"),
                                                         selected = "1D",
                                                         inline = TRUE),
                                      shiny::checkboxGroupInput("asset_class", "SELECT ASSET TYPE", 
                                                         choices = c("CFD", "Crypto", "Stocks"),
                                                         selected = c("CFD", "Crypto", "Stocks"),
                                                         inline = TRUE),  
                                      plotOutput("lastChange")
                                      )
                                  )
                                ),
                       tabPanel("IMPORTED DATA",
                                dataTableOutput("rawData")
                                ),
                       tabPanel("IMPORTED DATA OVERVIEW",
                               plotlyOutput("returns", width = "1400px", height = "800px", reportTheme = FALSE)
                               )
                )
                )),
      tabItem(tabName = "portview",
              fluidRow(
                tabBox(title = "Analysis", width = 12,
                  tabPanel("A",
                           dataTableOutput("weights")),
                  tabPanel("B",
                           dataTableOutput("portReturns")),
                  tabPanel("C", 
                           sliderInput("DatesMerge",
                                       "Dates:",
                                       min = as.Date("2021-03-01","%Y-%m-%d"),
                                       max = Sys.Date(),
                                       value  = c(Sys.Date() - 30, Sys.Date()),
                                       timeFormat="%Y-%m-%d",
                                       width = "100%"),
                           plotOutput("portValue", height = "500px")
                           )
                )
              )),
      tabItem(tabName = "portperf",
              fluidRow(
                column(width = 3,offset = 0,
                       tabBox(
                       tabPanel("A"),
                       tabPanel("B"))
                       ),
                column(width = 9, offset = 0,
                       tabBox(
                         tabPanel("A",
                                  plotOutput("perfPlot")),
                         tabPanel("B"))
                       )
              )
              )
    )
  )
)