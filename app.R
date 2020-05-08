library(shinydashboard)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "black"

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))),
    sliderInput("days", "Select forecast days",
                min = 7, max = 180, value =7 , step = 7))

body <- dashboardBody(
    tabItems(
        tabItem("dashboard",
                fluidRow(
                    box(
                        title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                        textInput("StockCode", "StockCode", value = "^GSPC"),
                        actionButton(inputId = "click", label = "Predict")
                    )),
                fluidRow(
                    box(
                        title = "Stock performance",
                        status = "primary",
                        plotOutput("arima", height = 350),
                        height = 400
                    ),
                    box(
                        title = "Stock Forecast for selected days",
                        width = 6,
                        tableOutput("arima1"),
                        height = 380
                    )))))

header <- dashboardHeader(
    title = "Stock Perdiction")

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {
    
    #Arima - plot here  Tile#4 
    output$arima <- renderPlot({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        data <- eventReactive(input$click, {
            (input$StockCode) 
        })
        Stock <- as.character(data())
        print(Stock)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock, 
                                           src = "yahoo", from = "2015-01-01", env = NULL))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)] 
        
        library(plotly)
        
        #arima
        fit <- Arima(Stock_df$Close, order = c(2,1,0) , include.drift = TRUE)
        fit.forecast <- forecast(fit, h = input$days)
        
        plot(fit.forecast,  main= Stock)
        fit.forecast
        
    })
    
    #Arima1 - plot here  Tile#5
    output$arima1 <- renderTable({
        
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", from = "2015-01-01", env = NULL))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        #arima
        fit <- Arima(Stock_df$Close, order = c(2,1,0) , include.drift = TRUE)
        fit.forecast <- forecast(fit, h= input$days)
        (fit.forecast)
    })
    
}

shinyApp(ui, server)


