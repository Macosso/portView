
library(shiny)
library(shinyWidgets)
library(DT)

combinations <- replicate(1000, paste(sample(letters, 3, replace = FALSE), collapse = ""))

df <- data.frame(city = rep(LETTERS[1:4], 250),
                 street = combinations, 
                 value = rnorm(1000))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            pickerInput("city", label = "select city", choices = unique(df$city), selected = unique(df$city), multiple = TRUE),
            virtualSelectInput("street", label = "Select Street", choices = NULL, selected = NULL, multiple = TRUE, search = TRUE)),

        mainPanel(
           DTOutput("tabs")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  tbl <- reactive({df[df$city %in% input$city & df$street %in% input$street,]})
  
  observe({
    new_street <- df[df$city %in% input$city, "street"]
    updateVirtualSelect(inputId = "street", session = session, choices = new_street, selected = new_street)
  })
  output$tabs <- renderDT({
    tbl()
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
