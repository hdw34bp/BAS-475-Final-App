library(shiny)
library(fpp3)
library(forecast)
library(datasets)
library(tseries)
library(bslib)
library(shinyWidgets)

Doge_data <- read.csv("DOGE-USD.csv")
Doge_data$Percent_Change <- ((Doge_data$Adj.Close - Doge_data$Open) / Doge_data$Open) * 100
Doge_data <- Doge_data%>%
    mutate(Date = ymd(Date))%>%
    as_tsibble(index = Date)

lambdaChange <- Doge_data %>%
    features(Percent_Change, features = guerrero) %>%
    pull(lambda_guerrero)
lambdaClose <- Doge_data %>%
    features(Close, features = guerrero) %>%
    pull(lambda_guerrero)
lambdaVolume <- Doge_data %>%
    features(Volume, features = guerrero) %>%
    pull(lambda_guerrero)

Doge_data <- Doge_data%>%
    mutate(bc_PercentChange = box_cox(Percent_Change, lambdaChange))%>%
    mutate(bc_Close = box_cox(Close, lambdaClose))%>%
    mutate(bc_volume = box_cox(Volume, lambdaVolume))%>%
    mutate(diff_percentChange = difference(bc_PercentChange, 3))%>%
    mutate(diff_close = difference(bc_Close, 2))%>%
    mutate(diff_volume = difference(bc_volume, 3))

train <- head(Doge_data, nrow(Doge_data)*0.75)
holdout <- tail(Doge_data, nrow(Doge_data)*0.25)

ui <- fluidPage(
    navbarPage("Doge Coin Analysis",
               setBackgroundColor("ghostwhite"),
               tabPanel(title = "Home Page",
                    
                    "This app provides tools to analyze the volume, closing price, and daily percent change in price of
                    Dogecoin. Dogecoin is a popular cryptocurrency that has had a significant influence on the crypto
                    market and is widely regarded as the crypto of the people.",
 	                    "The first tab will allow the user to compare plots of the daily percent change in price, close price, and volume from 
                    data beginning in January 2021 to May 6th, 2022. The user can choose to view the graphs individually or 
                    at the same time using the buttons to the left of the plots.",
                        "The second tab displays the seasonality, autocorrelation, and decomposition of the daily percent change. Similar to the first tab, the plots 
                    shown can be selected with the buttons on the left side. ",
                        "The third tab shows a plot of the trend of the daily percent change and is meant to put the power of market manipulation by everyday people into
                    perspective. ",
                        "The fourth tab displays three forecasts for closing price, volume, and percent change that are predicted using the Naive, 
                    Seasonal Naive, Mean, and Drift models. The user can use the drop 
                    down menu to choose which model is used to generate the forecasts.", 
                        "The fifth tab shows forecasts of the same three measures except it uses exponential
                    smoothing models as opposed to naive models. The user is able to switch between Holt
                    and Holt/Winter’s exponential smoothing models using the buttons to the left. Holt’s allows the use of
                    trend in the forecasting and Holt/Winter’s extends upon Holt’s to allow the use of seasonality with 
                    forecasting in addition to trend.", 
                        "The sixth and seventh tabs use the ARIMA model to forecast the same three measures which have been 
                    transformed using box cox and differencing. 
                    The sixth tab uses automatic parameter estimates that cannot be changed. The seventh
                    tab uses sliders, located on the left panel, to allow the users to change the p, d, and q parameters 
                    of the model themselves. Both tabs allow the user to select which measure they would like to forecast."
               ),
               
               tabPanel(title = "Plot of Percent Change and Volume",
                        sidebarPanel(
                        radioButtons('selected_plot', label = 'Select which graph(s) to view:', 
                                     choices = c("Percent Change", "Volume", "Close", "All"))
                        ),
                        mainPanel(
                        plotOutput("tab2Graphs"),
                        plotOutput("tab2GraphsPt2"),
                        plotOutput("tab2GraphsPt3")
                        )
                        ),
               
               tabPanel(title = "Seasonality, Autocorrelation, Decomposition For Percent Change",
                        sidebarPanel(
                        radioButtons('graph_selection', label = 'Choose a graph to display:',
                                     choices = c('Seasonality', 'Decomposition', 'Autocorrelation'))
                        ),
                        mainPanel(
                        plotOutput('tab3'),
                        textOutput("tab3Text")
                        )
                        ),
               
               tabPanel(title = "Percent Change Trend",
                        sidebarPanel("This trend can be described as relatively flat except for the massive 355.61% change,
                                     a mere $0.0266 change, on January 28th, 2021. This jump is seen as a response to a 
                                     picture of a dog magazine cover tweeted by Elon Musk on the 28th and the collaboration
                                     of members from a Reddit group called SatoshiStreetBets. This percent increase is 
                                     important when evaluating the impact ordinary people can have on the crypto market 
                                     and also gives some clarity to the reputation Elon Musk has today."),
                        mainPanel(
                        textOutput("trendWithOutlier"),
                        plotOutput("tab4Graph"), 
                        textOutput("trendWithoutOutlier"),
                        plotOutput("tab4GraphPt2")
                        )
                        ),
               
               tabPanel(title = "Naive, Seasonal Naive, Mean, and Drift Models",
                        sidebarPanel(
                            selectInput('selected_naive_model', label = 'Select which model to view:', 
                                         choices = c("Naive", "Seasonal Naive", "Mean", "Drift")),
                        ),
                        mainPanel(
                            plotOutput("Naive1"),
                            plotOutput("Naive2"),
                            plotOutput("Naive3")
                        )
               ),
               
               tabPanel(title = "Exponential Smoothing",
                        sidebarPanel(
                            radioButtons("selected_expo_model", label = "Select which exponential smoothing model to use:",
                                         choices = c("Holt", "Holt/Winters"))
                        ),
                        mainPanel(
                            plotOutput("tab5.1"),
                            plotOutput("tab5.2"),
                            plotOutput("tab5.3")
                        )
                        
                ),
               
               tabPanel(title = "Automatic ARIMA Model",
                        sidebarPanel(
                            radioButtons("change_close_volume", label = "Select which field to model:",
                                         choices = c("Percent Change", "Close", "Volume")),
                            
                        ),
                        mainPanel(
                            plotOutput("tab6Auto")
                        )
                        
                        
                ),
               
               tabPanel("Manual ARIMA Model",
                        sidebarPanel(
                            radioButtons("change_close_volumeManual", label = "Select which field to model:",
                                         choices = c("Percent Change", "Close", "Volume")),
                            sliderInput("p", label = "Select a p estimate:",
                                        min = 0, max = 4, step = 1, value = 1),
                            sliderInput("d", label = "Select a d estimate:",
                                        min = 0, max = 2, step = 1, value = 1),
                            sliderInput("q", label = "Select a q estimate:",
                                        min = 0, max = 15, step = 1, value = 1),
                        ),
                        mainPanel(
                            textOutput("tab7Text"),
                            plotOutput("tab7Manual"),
                            plotOutput("tab7ACF")
                            )
                        
                        )
                        
               
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Tab 2
    change_plot <- reactive({
        Doge_data%>%
            autoplot(Percent_Change)
    })
    
    volume_plot <- reactive({
        Doge_data%>%
            autoplot(Volume)
    })
    
    close_plot <- reactive({
        Doge_data%>%
            autoplot(Close)
    })
    
    all_plots <- reactive({
        change <- Doge_data%>%
            autoplot(Percent_Change)
        volume <- Doge_data%>%
            autoplot(Volume)
        close <- Doge_data%>%
            autoplot(Close)
        
        returnList <- list(change, volume, close)
        returnList
    })
    
    plotSelection <- reactive({
        switch (input$selected_plot,
            "Percent Change" = change_plot(),
            "Volume" = volume_plot(),
            "Close" = close_plot(),
            "All" = all_plots()[1],
        )
    })
    plotSelection2 <- reactive({
        switch (input$selected_plot,
                "All" = all_plots()[2]
        )
        })
    plotSelection3 <- reactive({
        switch (input$selected_plot,
                "All" = all_plots()[3]
        )
    })
    
    output$tab2Graphs <- renderPlot({
        plotSelection()
    })
    output$tab2GraphsPt2 <- renderPlot({
        plotSelection2()
    })
    output$tab2GraphsPt3 <- renderPlot({
        plotSelection3()
    })
    
    #Tab 3
    SeasonalityPlot <- reactive({
         gg_season(Doge_data, y = Percent_Change)
    })
    DecompPlot <- reactive({
        Doge_data %>%
            model(STL(Percent_Change))%>%
            components()%>%
            autoplot()
    })
    AutoCorrelation <- reactive({
        ACF(Doge_data, c(Percent_Change))%>%
            autoplot()
    })
    graphInput <- reactive({
        switch(input$graph_selection,
               "Seasonality" = SeasonalityPlot(),
               "Decomposition" = DecompPlot(),
               "Autocorrelation" = AutoCorrelation()
        )
        })
    
    textInputtab3 <- reactive({
        switch(input$graph_selection,
               "Seasonality" = "The daily percent change does not have strong seasonality.",
               "Decomposition" = "The decomposition reinforces the idea of no seasonality in the data, but also indicates that there is hardly a trend except for the massive percent change in early 2021.",
               "Autocorrelation" = "There is significant autocorrelation in daily percent change at lags 3, 26, and 27."
        )
    })
    output$tab3 <- renderPlot(
        graphInput()
    )
    output$tab3Text <- renderText({
        textInput()
    })
    
    textInput <- reactive({
        switch(input$graph_selection,
               'Seasonality' = 'The seasonality plot can be read similarly to a regular time series plot, except the x-axis shows data from within each season so that it is easier to see seasonal trends.',
               'Decomposition' = "The decomposition of a state first shows a graph of 1 of the 3 measures folled by the trend of that measure, its seasonality, and how irregular the measure was at that point in time. ",
               'Autocorrelation' = 'Autocorrelation measures the correlation between a time series and a lagged version of itself. A time series with high autocorrelation means that it is easier to forecast. The chart below can be read as at lag 1 autocorrelation is about 0.9.')
    })
    
    output$tab3Text <- renderText(
        textInputtab3()
    )
    
    #Tab 4
    output$tab4Graph <- renderPlot({
        ggplot(Doge_data, aes(x=Date, y=Percent_Change)) + 
            geom_point() + 
            geom_smooth(method = lm)
    })
    
    output$tab4GraphPt2 <- renderPlot({
        tmp_DOGE <- Doge_data
        tmp_DOGE <- tmp_DOGE[-which(tmp_DOGE$Date == "2021-01-28"), ]
        
        ggplot(tmp_DOGE, aes(x=Date, y=Percent_Change)) + 
            geom_point() + 
            geom_smooth(method = lm)
        
    })
    
    output$trendWithoutOutlier <- renderText({
        "The trend of percent change without the January 28, 2021 outlier."
    })
    
    output$trendWithOutlier <- renderText({
        "The trend of percent change with the January 28, 2021 outlier."
    })
    
    #Tab 5
    
    
    NaiveModels <- reactive({
        
       change <- train%>%
           model(NAIVE(Percent_Change))%>%
           forecast(h=400)%>%
           autoplot(train)
       close <- train%>%
           model(NAIVE(Close))%>%
           forecast(h=400)%>%
           autoplot(train)
       volume <- train%>%
           model(NAIVE(Volume))%>%
           forecast(h=400)%>%
           autoplot(train)
       
       returnList <- list(change, close, volume)
       returnList
    })

    SNaiveModel <- reactive({
        train <- head(Doge_data, nrow(Doge_data)*0.75)
        holdout <- tail(Doge_data, nrow(Doge_data)*0.25)
        
        change <- train%>%
            model(SNAIVE(Percent_Change~lag("month")))%>%
            forecast(holdout)%>%
            autoplot(train)
        close <- train%>%
            model(SNAIVE(Close~lag("month")))%>%
            forecast(holdout)%>%
            autoplot(train)
        volume <- train%>%
            model(SNAIVE(Volume~lag("month")))%>%
            forecast(holdout)%>%
            autoplot(train)
        
        returnList <- list(change, close, volume)
        returnList
    })
    
    MeanModel <- reactive({
        train <- head(Doge_data, nrow(Doge_data)*0.75)
        holdout <- tail(Doge_data, nrow(Doge_data)*0.25)
        
        change <- train%>%
            model(MEAN(Percent_Change))%>%
            forecast(h=300)%>%
            autoplot(train)
        close <- train%>%
            model(MEAN(Close))%>%
            forecast(h=300)%>%
            autoplot(train)
        volume <- train%>%
            model(MEAN(Volume))%>%
            forecast(h=300)%>%
            autoplot(train)
        
        returnList <- list(change, close, volume)
        returnList
    })
    
    DriftModel <- reactive({
        train <- head(Doge_data, nrow(Doge_data)*0.75)
        holdout <- tail(Doge_data, nrow(Doge_data)*0.25)
        
        change <- train%>%
            model(RW(Percent_Change~drift()))%>%
            forecast(h=300)%>%
            autoplot(train)
        close <- train%>%
            model(RW(Close~drift()))%>%
            forecast(h=300)%>%
            autoplot(train)
        volume <- train%>%
            model(RW(Volume~drift()))%>%
            forecast(h=300)%>%
            autoplot(train)
        
        returnList <- list(change, close, volume)
        returnList
    })
    
    
    NaiveModelInput1 <- reactive({
        switch (input$selected_naive_model,
            "Naive" = NaiveModels()[1],
            "Seasonal Naive" = SNaiveModel()[1],
            "Mean" = MeanModel()[1],
            "Drift" = DriftModel()[1]
        )
    })
    NaiveModelInput2 <- reactive({
        switch (input$selected_naive_model,
                "Naive" = NaiveModels()[2],
                "Seasonal Naive" = SNaiveModel()[2],
                "Mean" = MeanModel()[2],
                "Drift" = DriftModel()[2]
        )
    })
    NaiveModelInput3 <- reactive({
        switch (input$selected_naive_model,
                "Naive" = NaiveModels()[3],
                "Seasonal Naive" = SNaiveModel()[3],
                "Mean" = MeanModel()[3],
                "Drift" = DriftModel()[3]
        )
    })
    
    output$Naive1 <- renderPlot({
        NaiveModelInput1()
    })
    
    output$Naive2 <- renderPlot({
        NaiveModelInput2()
    })
    
    output$Naive3 <- renderPlot({
        NaiveModelInput3()
    })
    
    #Tab 5
    HoltExponentialModel <- reactive({
        train <- head(Doge_data, nrow(Doge_data)*0.75)
        holdout <- tail(Doge_data, nrow(Doge_data)*0.25)
        
         change <- train%>%
            model(ETS(Percent_Change~ error("A")+trend("A")+season("N")))%>%
            forecast(h=300)%>%
            autoplot(train)
         close <- train%>%
             model(ETS(Close~ error("A")+trend("Ad", phi = 0.9)+season("N")))%>%
             forecast(h=300)%>%
             autoplot(train)
         volume <- train%>%
             model(ETS(Volume~ error("A")+trend("A")+season("N")))%>%
             forecast(h=300)%>%
             autoplot(train)
         
         returnList <- list(change, close, volume)
         returnList
    })
    
    WinterAndHoltExponentialModel <- reactive({
        train <- head(Doge_data, nrow(Doge_data)*0.75)
        holdout <- tail(Doge_data, nrow(Doge_data)*0.25)
        
        change <- train %>%
            model(
                additive = ETS(Percent_Change ~ error("A") + trend("A") +
                                   season("A"))
            )%>%
            forecast(h=300)%>%
            autoplot(train)
        close <- train %>%
            model(
                additive = ETS(Close ~ error("A") + trend("A") +
                                   season("A")),
            )%>%
            forecast(h=300)%>%
            autoplot(train)
        volume <- train%>%
            model(
                additive = ETS(Volume ~ error("A") + trend("A") +
                                   season("A"))
            )%>%
            forecast(h=300)%>%
            autoplot(train)
        
        
        returnList <- list(change, close, volume)
        returnList
    })
    
    ExpoModelChoice1 <- reactive({
        switch(input$selected_expo_model,
               "Holt" = HoltExponentialModel()[1],
               "Holt/Winters" = WinterAndHoltExponentialModel()[1]
            
        )
    })
    
    ExpoModelChoice2 <- reactive({
        switch(input$selected_expo_model,
               "Holt" = HoltExponentialModel()[2],
               "Holt/Winters" = WinterAndHoltExponentialModel()[2]
               
        )
    })
    
    ExpoModelChoice3 <- reactive({
        switch(input$selected_expo_model,
               "Holt" = HoltExponentialModel()[3],
               "Holt/Winters" = WinterAndHoltExponentialModel()[3]
               
        )
    })
    
    output$tab5.1 <- renderPlot(ExpoModelChoice1())
    output$tab5.2 <- renderPlot(ExpoModelChoice2())
    output$tab5.3 <- renderPlot(ExpoModelChoice3())
    
    #Tab 6 & 7
    ArimaAuto <- reactive({

        change <- train%>%
            model(ARIMA(diff_percentChange))%>%
            forecast(h=300)%>%
            autoplot(train)
        close <- train%>%
            model(ARIMA(diff_close))%>%
            forecast(h=300)%>%
            autoplot(train)
        volume <- train%>%
            model(ARIMA(diff_volume))%>%
            forecast(h=300)%>%
            autoplot(train)
        
        
        returnList <- list(change, close, volume)
        returnList
    })
    
    ArimaManual <- reactive({
        
        p <- input$p
        d <- input$d
        q <- input$q
        
        change <- train%>%
            model(ARIMA(diff_percentChange~pdq(p, d, q)))%>%
            forecast(h=400)%>%
            autoplot(train)

        close <- train%>%
            model(ARIMA(diff_close~pdq(p, d, q)))%>%
            forecast(h=400)%>%
            autoplot(train)
        volume <- train%>%
            model(ARIMA(diff_volume~pdq(p, d, q)))%>%
            forecast(h=400)%>%
            autoplot(train)
        
        changeACF <- Doge_data%>%
            ACF(diff_percentChange)%>%
            autoplot()
        closeACF <- Doge_data%>%
            ACF(diff_close)%>%
            autoplot()
        volumeACF <- Doge_data%>%
            ACF(diff_volume)%>%
            autoplot()
        
        
        
        returnList <- list(change, close, volume, changeACF, closeACF, volumeACF)
        returnList
        
    })


    ARIMAAutoInput <- reactive({
        switch (input$change_close_volume,
                "Percent Change" = ArimaAuto()[1],
                "Close" = ArimaAuto()[2],
                "Volume" = ArimaAuto()[3]
        )
    })
    
    
    ARIMAManualInput <- reactive({
        switch (input$change_close_volumeManual,
                "Percent Change" = ArimaManual()[1],
                "Close" = ArimaManual()[2],
                "Volume" = ArimaManual()[3]
        )
    })
    
    ARIMAACFInput <- reactive({
        switch (input$change_close_volumeManual,
                "Percent Change" = ArimaManual()[4],
                "Close" = ArimaManual()[5],
                "Volume" = ArimaManual()[6]
        )
    })
    
    textInputManual <- reactive({
        switch (input$change_close_volumeManual,
                "Percent Change" = "Try p = 2, d = 1, and q = 0",
                "Close" = "Try p = 1, d = 1, and q = 0",
                "Volume" = "Try p = 4, d = 0, and q = 0"
        )
    })
    
    output$tab6Auto <- renderPlot(ARIMAAutoInput())
    output$tab7Manual <- renderPlot(ARIMAManualInput())
    output$tab7ACF <- renderPlot(ARIMAACFInput())
    output$tab7PACF <- renderPlot(ARIMAACFInput())
    output$tab7Text <- renderText(textInputManual())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
