library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shiny.semantic)
library(fresh)
#library(rhino)


source("src/preprocessing.R")

title <- tags$a(href = "https://www.github.com/macosso",
                tags$img(src = "lambda.png", height = '50', width = '50'),
                "portView")

my_theme = create_theme(
  adminlte_color(
    light_blue = "#002520",
    black = "#002520",
    navy = "#002520"
  )
)



# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = title,
    tags$li(class = "dropdown",
            tags$a(href = "#", `data-toggle` = "tab", 
                   icon("info-circle"), "Home",
                   style = "color: white;", `data-target` = "#home")),
    tags$li(class = "dropdown",
            tags$a(href = "#", `data-toggle` = "tab", 
                   icon("info-circle"), "Dashboard",
                   style = "color: white;", `data-target` = "#dashboad")),
    tags$li(class = "dropdown",
            tags$a(href = "#", `data-toggle` = "tab",
                   icon("cog"), "Panel 2",
                   style = "color: white;", `data-target` = "#panel2"))
  ),
  dashboardSidebar(),
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),
    tags$img(
      src = "EU.jpg",
      style = 'position: absolute'
    ),
    use_theme(my_theme),
    includeCSS("www/style.css"),
    div(class = "tab-content",
        div(id = "home", class = "tab-pane",
            fluidRow(
              box(title = "Panel 1 - Plot", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("plot1")),
              box(title = "Panel 1 - Table", status = "primary", solidHeader = TRUE, width = 6,
                  tableOutput("table1"))
            )
        ),
        div(id = "dashboad", class = "tab-pane",
            fluidRow(
              box(title = "Dashoard Settings", status = "primary", solidHeader = TRUE, width = 3, height = 15,
                  dateRangeInput("startedate", "select date"),
                  selectInput("assetclass", "select asset class",
                              choices = unique(open_positions$asset_type),
                              selected = unique(open_positions$asset_type))),
              
              box(title = "Panel 1 - Table", status = "primary", solidHeader = TRUE, width = 6,
                  tableOutput("table1"))
            ),
            fluidRow(
              box(title = "Panel 1 - Inputs", status = "primary", solidHeader = TRUE, width = 12,
                  textInput("textInput1", "Input field:", ""),
                  actionButton("button1", "Action Button")
              )
            )
        ),
        div(id = "panel2", class = "tab-pane",
            fluidRow(
              box(title = "Panel 2 - Plot", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("plot2")),
              box(title = "Panel 2 - Table", status = "primary", solidHeader = TRUE, width = 6,
                  tableOutput("table2"))
            ),
            fluidRow(
              box(title = "Panel 2 - Inputs", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("selectInput1", "Select field:", choices = c("Option 1", "Option 2")),
                  actionButton("button2", "Another Action Button")
              )
            )
        )
    )
  )
)

