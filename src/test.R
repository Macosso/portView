library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "EU Themed Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Box 1", status = "primary", solidHeader = TRUE, width = 6,
                    "Box content here"),
                box(title = "Box 2", status = "primary", solidHeader = TRUE, width = 6,
                    "Box content here")
              )
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

# Define server logic
server <- function(input, output) { }

# Run the application
shinyApp(ui = ui, server = server)
