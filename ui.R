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
                                                     start = as.character(Sys.Date()-30),
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
                                      shiny::selectInput("period", "SELECT RETURN PERIOD", 
                                                         choices = c("1D", "7D", "MTD", "YTD", "1M", "3M", "12M", "3Y"),
                                                         selected = "1D"),
                                      shiny::selectInput("asset_class", "SELECT ASSET TYPE", 
                                                         choices = c("CFD", "Crypto", "Stocks"),
                                                         selected = c("CFD", "Crypto", "Stocks"),
                                                         multiple = TRUE),  
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
                box(title = "Settings", solidHeader = TRUE, width = 3,
                    shiny::sliderInput("selectID", "Select to N", min = 1, max = 10, value = 4, step = 2),
                    shiny::dateRangeInput("inputDate", "select Dates", start = as.character(Sys.Date()-30)),
                    shiny::selectInput("assetType", "Select Asset Type", choices = c("Crypto", "Stocks", "CFD"), multiple = TRUE)
                    ),
                tabBox(title = "Analysis", width = 9,
                  tabPanel("A",
                           dataTableOutput("weights")),
                  tabPanel("B"),
                  tabPanel("C")
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