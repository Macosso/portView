# Import libraries
library(DT)
library(scales)
library(treemapify)
library(xts)
library(zoo)
library(plotly)
library(shiny)
library(shinydashboard)




# import predefined functions
source("src/preprocessing.R")
source("src/Returns.R")
source("src/support_functions.R")

# Define server logic

server <- function(input, output, session) {
  
  
  raw_symbols <- unique(open_closed$symbol)
  
  raw_sm_react <- reactive({raw_symbols[!raw_symbols %in% input$excSymb]})
  
  
  
  tickers1 <- reactive(unique(open_closed[open_closed$date >= input$inputDate[1],]$symbol))
  tickers <- reactive({tickers1()[tickers1() %in% raw_sm_react()]})
  
  stock_prices <- eventReactive(input$downloadsymbols,
                           {quantR::loadTokens(tickers(), 
                                               start = input$inputDate[1], 
                                               end = input$inputDate[2])})

  
  #tickers <- eventReactive(input$submitexclusion, {tickers()[!tickers %in% input$excSymb]})
  
  
  long_df_prices <- reactive({get_prices_long(stock_prices())})
  merged_acc_prices <- reactive({merge_acc_prices(open_closed, long_df_prices())})
  port_w <- reactive({get_port_weights(merged_acc_prices()) |>
      mutate(across(!date, function(w) paste0(round(w*100,2),"%"))) 
    })
  
  
  
  portReturn <- reactive(
    quantR::pctReturns(stock_prices(), na.rm = TRUE)
  )
  
  
  
  cumReturns <- reactive({get_cum_returns(portReturn(),  period = input$period)})
  
  output$lastChange <- renderPlot({
    cumReturns()|>
      left_join(symbols_mapping |> arrange(desc(asset_type)) |>
                  distinct(symbol , .keep_all = TRUE), 
                by = "symbol") |>
      mutate(gain = ifelse(Return >= 0, "green", "red"),
             pltsize = abs(Return)) |>
      dplyr::filter(asset_type %in% input$asset_class) |>

      ggplot(aes(area = pltsize, fill = gain, label = paste0(symbol,"\n", round(Return*100,2), "%"))) +
      geom_treemap(alpha = 0.7) +
      geom_treemap_text(fontface = "italic", colour = "black", place = "centre",
                        grow = F) +
      scale_fill_manual(values = c("red" = "red", "green" = "green"))+
      theme(legend.position="none") 
      
    })
  
  
  
  
  output$rawData <- DT::renderDT(data.frame(date = zoo::index(stock_prices()),
                                               round(zoo::coredata(stock_prices()[,tickers()]),2)),
                                    options = list(scrollX = TRUE,
                                                   autoWidth = TRUE,
                                                   columnDefs = list(list(width = '100px', targets = "date")))
                                      )


  
  output$returns <- renderPlotly({
    data.frame(date = zoo::index(portReturn()),
               round(zoo::coredata(portReturn()[,tickers()]),2)) |>
      pivot_longer(!date, names_to = "symbol", values_to = "Return") |>
      left_join(symbols_mapping |> arrange(desc(asset_type)) |>
                  distinct(symbol , .keep_all = TRUE), 
                by = "symbol") |>
      mutate(color = ifelse(Return >= 0, "green", "red")) |>
      ggplot(aes(x = date, y = Return, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("red" = "red", "green" = "green")) +
      scale_y_continuous(n.breaks = 5, labels = scales::percent) +
      theme(legend.position="none") + 
      facet_wrap(.~symbol)
  })
  
  
  output$weights <- renderDT(port_w(),
                             options = list(scrollX = TRUE,
                                            autoWidth = TRUE,
                                            columnDefs = list(list(width = '100px', targets = "date"))))
}