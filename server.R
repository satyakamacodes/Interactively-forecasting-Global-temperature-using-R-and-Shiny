cat("\f")
closeAllConnections()
rm(list = ls())


options(shiny.maxRequestSize=50*1024^2) 

if (!require("pacman")) install.packages("pacman")

options(shiny.maxRequestSize=50*1024^2) 

pacman::p_load(shiny,
               shinydashboard,
               data.table,
               DT,
               zoo,
               dygraphs,
               ggplot2,
               plotly,
               forecast,
               tseries)


#Fetching data

#Incase your code doesnt run the 1st time, run the code between #====== in your current directory
#=======================================================================
setwd("C:/Users/satyakama.paul/Documents/R_projects/global_land_temperature/shiny_app14")

m.data <- read.csv("GlobalLandTemperaturesByState.csv",
                   header = T,
                   sep = ",",
                   na.strings = c("",
                                  "NA"),
                   stringsAsFactors = T)

m.data$dt <- as.Date(m.data$dt,
                     format = "%Y-%m-%d")


Date <- m.data$dt

m.data <- cbind(m.data,
                Date)

m.data <- m.data[,c(5,4,2,6)]

colnames(m.data) <- c("Country",
                      "State",
                      "Avg.Temp",
                      "Date")

#=======================================================================


server <- function(input, output){
     
     
     
     #Selecting country
     
     selected.country <- reactive({
          data.country <- subset(m.data,
                                 Country == input$Country)
          
          data.country
     })
     
     
     output$state <- renderUI({selectInput("State", 
                                           "Choose State", 
                                           choices = c(as.character(unique(selected.country()$State))),
                                           multiple = FALSE)})
     
     
     #Selecting state
     #selected.state is the final dataset with filtered country and state
     
     selected.state <- reactive({
          data.state <- subset(selected.country(), 
                               State == input$State)
          
          data.state
     })
     
     
     #=========================================
     #First level of display
     #=========================================
     
     output$o.rawdataview.box <- renderValueBox({
          
          valueBox(value = "Raw data view",
                   subtitle = "",
                   color = "fuchsia",
                   width = 4)
          
     })
     
     output$o.basicinfo.box <- renderValueBox({
          
          valueBox(value = "Basic info",
                   subtitle = "",
                   color = "blue",
                   width = 4)
          
     })
     
     output$o.finaldata.view <- DT::renderDataTable({
          
          if(is.null(selected.state())){return()}
          
          else{
               DT:: datatable(selected.state(),
                              options = list(pageLength = 3))}
     })
     
     
     
     output$o.number.datapoints <- renderInfoBox({
          infoBox(title = "Number of records",
                  value = dim(selected.state())[1],
                  color = "red",
                  icon = icon("info"),
                  width = 2
          )
     })
     
     
     output$o.perc.na <- renderInfoBox({
          
          infoBox(title = "Percentage of Missing values",
                  value = round((sum(is.na(selected.state()$Avg.Temp))/length(selected.state()$Avg.Temp)*100),
                                digits = 2),
                  color = "aqua",
                  icon = icon("info"))
     })
     
     output$o.min.date <- renderInfoBox({
          
          infoBox(title = "Record starts from",
                  value = min(selected.state()$Date, na.rm = T),
                  color = "blue",
                  icon = icon("calendar"))
     })
     
     output$o.max.date <- renderInfoBox({
          
          infoBox(title = "Record ends at",
                  value = max(selected.state()$Date, na.rm = T),
                  color = "navy",
                  icon = icon("calendar"))
          
     })
     
     output$o.min.temp <- renderInfoBox({
          
          infoBox(title = "Minimum Average temperature",
                  value = min(selected.state()$Avg.Temp, na.rm = T),
                  color = "fuchsia",
                  icon = icon("info"))
     })
     
     output$o.max.temp <- renderInfoBox({
          
          infoBox(title = "Maximum Average temperature",
                  value = max(selected.state()$Avg.Temp, na.rm = T),
                  color = "purple",
                  icon = icon("info"))
     })
     
     
     
     completedates.data <- reactive({
          
          full.dates <- seq(min(selected.state()$Date),
                            max(selected.state()$Date), 
                            by = "month")
          
          full.dates <- data.frame(Date = full.dates)
          
          
          my.complete.data <- merge(full.dates,
                                    selected.state(),
                                    by = "Date", 
                                    all.x = TRUE)
          
          #my.complete.data <- na.locf(my.complete.data)
          
          my.complete.data <- my.complete.data[,c(2,3,4,1)]
          
          my.complete.data$Country <- as.factor(my.complete.data$Country)
          
          my.complete.data$State <- as.factor(my.complete.data$State)
          
          my.complete.data$Avg.Temp <- as.numeric(my.complete.data$Avg.Temp)
          
          my.complete.data$Date <- as.Date(my.complete.data$Date,
                                           format = "%Y-%m-%d")
          
          
          my.complete.data
     })
     
     #=========================================
     #Second level of display
     #=========================================
     
     output$o.plotly.box <- renderValueBox({
          
          valueBox(value = "Time series interactive plot of Average temperature",
                   subtitle = "",
                   color = "navy",
                   width = 12)
          
     })
     
     
     output$o.fig1 <- renderPlotly({
          
          x.label <- list(title = "Date")
          y.label <- list(title = "Avg. temp")
          
          plot_ly(completedates.data(),
                  x = ~as.Date(Date,
                               format = "%Y-%m-%d"),
                  y = ~as.numeric(Avg.Temp),
                  mode = 'markers+lines',
                  line = list(color = 'rgb(148,0,211)', 
                              width = 0.5))%>%
               layout(xaxis = x.label, 
                      yaxis = y.label)
          
     })
     
     #Training data creation
     
     training.data <- reactive({
          
          rows.training.data <- round(0.80*length(completedates.data()$Avg.Temp),
                                      0)
          
          training.data <- completedates.data()[1:rows.training.data,]
          
          training.data$Date <- format(training.data$Date,
                                       "%Y-%m")
          
          training.data$Date <- gsub("-",
                                     ",",
                                     training.data$Date)
          
          
          training.data
     })
     
     min.year.training.data <- reactive({
          
          min.year.training.data <- min(training.data()$Date, na.rm = T)
          
          min.year.training.data <- substr(min.year.training.data,
                                           start = 1,
                                           stop = 4)
     })
     
     
     min.month.training.data <- reactive({
          
          min.month.training.data <- min(training.data()$Date, na.rm = T)
          
          min.month.training.data <- substr(min.month.training.data,
                                            start = 6,
                                            stop = 7)
     })
     
     
     max.year.training.data <- reactive({
          
          max.year.training.data <- max(training.data()$Date, na.rm = T)
          
          max.year.training.data <- substr(max.year.training.data,
                                           start = 1,
                                           stop = 4)
     })
     
     max.month.training.data <- reactive({
          
          max.month.training.data <- max(training.data()$Date, na.rm = T)
          
          max.month.training.data <- substr(max.month.training.data,
                                            start = 6,
                                            stop = 7)
     })
     
     #Time series object creation
     my.ts <- reactive({
          
          
          my.ts <- ts(training.data()$Avg.Temp,
                      start = as.numeric(c(min.year.training.data(),
                                           min.month.training.data())),
                      end = as.numeric(c(max.year.training.data(),
                                         max.month.training.data())),
                      frequency = 12)    
          
          my.ts <- tsclean(my.ts, 
                           replace.missing = TRUE, 
                           lambda = NULL)
     })
     
     
     
     #Test data creation      
     testing.data <- reactive({
          
          row.starts <- round(0.80*length(completedates.data()$Avg.Temp),
                              0)+1
          row.ends <- length(completedates.data()$Avg.Temp)
          
          testing.data <- completedates.data()[row.starts:row.ends,]
          
          testing.data$Date <- format(testing.data$Date,
                                      "%Y-%m")
          
          testing.data$Date <- gsub("-",
                                    ",",
                                    testing.data$Date)
          
          testing.data
     })
     
     #=========================================
     #Third level of display
     #=========================================
     
     output$o.traintest.box <- renderValueBox({
          
          valueBox(value = "Spliting the dataset",
                   subtitle = "",
                   color = "maroon",
                   width = 3)
          
     })
     
     output$o.train.box.view <- renderValueBox({
          
          valueBox(value = "Training dataset",
                   subtitle = "",
                   color = "yellow",
                   width = 3)
          
     })
     
     output$o.test.box.view <- renderValueBox({
          
          valueBox(value = "Testing dataset",
                   subtitle = "",
                   color = "green",
                   width = 3)
          
     })
     
     
     output$o.training.data.view <- DT::renderDataTable({
          
          DT:: datatable(training.data(),
                         options = list(pageLength = 3))
          
     })
     
     output$o.testing.data.view <- DT::renderDataTable({
          
          DT:: datatable(testing.data(),
                         options = list(pageLength = 3))
          
     })
     
     
     #=========================================
     #Fourth level of display
     #=========================================
     
     output$o.model.box <- renderValueBox({
          
          valueBox(value = "Model building",
                   subtitle = "on the training set",
                   color = "black",
                   width = 4)
          
     })
     
     output$o.model.country <- renderValueBox({
          
          valueBox(value = "For country",
                   subtitle = selected.state()$Country[1],
                   color = "fuchsia",
                   width = 4)
          
     })
     
     output$o.model.state <- renderValueBox({
          
          valueBox(value = "For state",
                   subtitle = selected.state()$State[1],
                   color = "red",
                   width = 4)
          
     })
     
     output$o.no.training.data.points <- renderValueBox({
          
          valueBox(subtitle = "Number of records in the training dataset",
                   value = dim(training.data())[1],
                   color = "blue",
                   icon = icon("info"),
                   width = 12)
     })
     
     observeEvent(
          input$model.click,
          {
               #---
               
               output$o.print.decomp <- renderUI({
                    
                    helpText(h1("Decomposition of time series",
                                style = "color: #8B008B"))
               })
               
               output$o.add.formula <- renderUI({
                    withMathJax(
                         helpText(h3('Additive model:  $$y_t = Seasonality_t + Trend_t + Random_t$$',
                                     style = "color: #ff1493"))
                         )
                    
                    
               })
               
               output$o.mul.formula <- renderUI({
                    withMathJax(
                         helpText(h3('Multiplicative model:  $$y_t = Seasonality_t \\times Trend_t \\times Random_t$$',
                                     style = "color: #000080"))
                    )
                    
                    
               })
               
               
               output$o.decomp.plot.additive <- renderPlot({
                    
                    
                    plot(decompose(my.ts(),
                                   type = c("additive")),
                         col = "deeppink")
                    
                    
               })
               
               output$o.decomp.plot.multiplicative <- renderPlot({
                    
                    
                    plot(decompose(my.ts(),
                                   type = c("multiplicative")),
                         col = "blue3")
                    
               })
               
               output$o.print.stationarity <- renderUI({
                    
                    helpText(h1("Test of Stationarity of time series",
                                style = "color: #8B008B"))
               })
               
               
               
               output$o.adftest <- renderPrint({
                    
                    
                    cat("**************************************************************************************************************************************************************************************", "\n")
                    cat("The null hypothesis of Augmented Dickey-Fuller test is that the time series is non-stationary. p value > 0.05 indicates that the non-stationarity cannnot be rejected, and vice versa.",
                        "\n")
                    cat("**************************************************************************************************************************************************************************************", "\n")
                    
                    adf.test(my.ts(),
                             alternative = "stationary")
               })
               
               
               
               output$o.print.seasonality <- renderUI({
                    
                    helpText(h1("Visualizing seasons and months",
                                style = "color: #8B008B"))
               })
               
               output$o.seasonplot <- renderPlot({
                    
                    seasonplot(my.ts(),
                               s = 12,
                               main = "Seasonality plot",
                               ylab = "Avg. Temp",
                               xlab = "Months",
                               col = rainbow((dim(training.data())[1])/12))
                    
               })
               
               output$o.monthplot <- renderPlot({
                    
                    monthplot(my.ts(),
                              main = "Monthly plot",
                              ylab = "Avg. Temp",
                              xlab = "Months")
                    
               })
               
               output$o.print.correlogram <- renderUI({
                    
                    helpText(h1("Correlograms",
                                style = "color: #8B008B"))
               })
               
               
               output$o.acfpacf.plot <- renderPlot({
                    
                    par(mfrow = c(1,2))
                    
                    Acf(my.ts(),
                        main = "Auto Correlation Factor plot",
                        na.action = na.contiguous)
                    
                    Pacf(my.ts(),
                         main = "Partial Auto Correlation Factor plot",
                         na.action = na.contiguous)
               })
               
               
               ets.fit <- reactive({
                    
                    start.time <- Sys.time()
                    
                    withProgress(message = "Model building in progress",
                                 detail = "This will take a while ...",
                                 value = 0,{
                                      
                                      ets.fit <- ets(my.ts(),
                                                     model = "ZZZ",
                                                     damped = NULL,
                                                     alpha = NULL,
                                                     beta = NULL,
                                                     gamma = NULL,
                                                     phi = NULL,
                                                     allow.multiplicative.trend = T)
                                 })
                    
                    
                    end.time <- Sys.time()
                    
                    time.taken <- end.time - start.time
                    
                    
                    
                    summary(ets.fit)
                    
                    cat("*****************************************************************", "\n")
                    cat("Total time taken in model building on training set",
                        round(time.taken, digits = 1),
                        "secounds",
                        "\n")
                    cat("*****************************************************************", "\n")
                    
               })
               
               
               output$o.print.etsparameters <- renderUI({
                    
                    helpText(h1("Parameters of Exponential Smoothing model",
                                style = "color: #8B008B"))
               })
               
               
               
               
               
               output$o.print.model <- renderPrint({
                    
                    ets.fit()
               })
               
               
               output$o.help <- renderUI({
                    
                    
                    img(height = 600,
                        width = 600,
                        src = "fig2.png",
                        class = "pull-center")
                    
                    
                    
               })
               
               #---
          })
     
     
     #=========================================
     #Fifth level of display
     #=========================================
     
     output$o.test.box <- renderValueBox({

          valueBox(value = "Model testing",
                   subtitle = "on the test set",
                   color = "black",
                   icon = icon("info"),
                   width = 4)

     })
     
     output$o.test.country <- renderValueBox({
          
          valueBox(value = "For country",
                   subtitle = selected.state()$Country[1],
                   color = "fuchsia",
                   width = 4)
          
     })
     
     output$o.test.state <- renderValueBox({
          
          valueBox(value = "For state",
                   subtitle = selected.state()$State[1],
                   color = "red",
                   width = 4)
          
     })
     
     output$o.no.testing.data.points <- renderValueBox({
          
          valueBox(subtitle = "Number of records in the testing dataset",
                   value = dim(testing.data())[1],
                   color = "blue",
                   icon = icon("info"),
                   width = 12)
     })
     

     
        
     
     observeEvent(
          input$test.pred.click,
          {     
               #---
               
               ets.fit <- reactive({
                    
                    start.time <- Sys.time()
                    
                    withProgress(message = "Predictions on test data in progress",
                                 detail = "This will take a while ...",
                                 value = 0,{
                                      
                                      ets.fit <- ets(my.ts(),
                                                     model = "ZZZ",
                                                     damped = NULL,
                                                     alpha = NULL,
                                                     beta = NULL,
                                                     gamma = NULL,
                                                     phi = NULL,
                                                     allow.multiplicative.trend = T)
                                 })
                    
                    
                    end.time <- Sys.time()
                    
                    time.taken <- end.time - start.time
                    
                    fore <- forecast(ets.fit, 
                                     h = dim(testing.data())[1])
                    
                    fore$mean
                    
               })
               

               output$o.dy.pred.on.test <- renderDygraph({
                    
                    df <- cbind(ets.fit(), 
                                testing.data()$Avg.Temp)
                    
                    dygraph(df, 
                            main = "Interactive plot for comparison of Predicted with Actual values")%>% 
                         dyRangeSelector()%>%
                         dySeries("ets.fit()",
                                  label = "Predicted values",
                                  color = "red") %>%
                         dySeries("testing.data()$Avg.Temp", 
                                  label = "Actual values",
                                  color = "black")%>%
                         dyAxis("x", drawGrid = FALSE, label = "Time")%>%
                         dyAxis("y", drawGrid = FALSE, label = "Avg.Temp") %>%
                         dyOptions(drawPoints = TRUE, 
                                   pointSize = 3)
               })
               
               
               #---     
          })
               

     
     output$o.forecast.box <- renderValueBox({
          
          valueBox(value = "Forecasting",
                   subtitle = "Move slider to adjust the number of periods",
                   color = "black",
                   width = 15)
          
     })
     
     output$o.forecast.country <- renderValueBox({
          
          valueBox(value = "For country",
                   subtitle = selected.state()$Country[1],
                   color = "fuchsia",
                   width = 15)
          
     })
     
     output$o.forecast.state <- renderValueBox({
          
          valueBox(value = "For state",
                   subtitle = selected.state()$State[1],
                   color = "red",
                   width = 15)
     })
     
     #Works fine uptill here______________  
     
     #=========================================
     #Seventh level of display
     #=========================================
     
     observeEvent(
          input$forecast.click,
          {     
               #---
               
               #Creating (full) data as time series for future forecast
               
               full.data <- reactive({
                    
                    full.data <- completedates.data()
                    
                    full.data$Date <- format(full.data$Date,
                                             "%Y-%m")
                    
                    full.data$Date <- gsub("-",
                                           ",",
                                           full.data$Date)
                    
                    
                    full.data
               })
               
               min.year.full.data <- reactive({
                    
                    min.year.full.data <- min(full.data()$Date, na.rm = T)
                    
                    min.year.full.data <- substr(min.year.full.data,
                                                 start = 1,
                                                 stop = 4)
               })
               
               min.month.full.data <- reactive({
                    
                    min.month.full.data <- min(full.data()$Date, na.rm = T)
                    
                    min.month.full.data <- substr(min.month.full.data,
                                                  start = 6,
                                                  stop = 7)
               })
               
               
               max.year.full.data <- reactive({
                    
                    max.year.full.data <- max(full.data()$Date, na.rm = T)
                    
                    max.year.full.data <- substr(max.year.full.data,
                                                 start = 1,
                                                 stop = 4)
               })
               
               max.month.full.data <- reactive({
                    
                    max.month.full.data <- max(full.data()$Date, na.rm = T)
                    
                    max.month.full.data <- substr(max.month.full.data,
                                                  start = 6,
                                                  stop = 7)
               })
               
               
               full.ts <- reactive({
                    
                    
                    full.ts <- ts(as.numeric(full.data()$Avg.Temp),
                                  start = as.numeric(c(min.year.full.data(),
                                                       min.month.full.data())),
                                  end = as.numeric(c(max.year.full.data(),
                                                     max.month.full.data())),
                                  frequency = 12)    
                    
                    full.ts <- tsclean(full.ts, 
                                       replace.missing = TRUE, 
                                       lambda = NULL)
                    
                    full.ts
               })
               
               full.ets.fit <- reactive({
                    
                    withProgress(message = "Forecasting in progress",
                                 detail = "This will take a while ...",
                                 value = 0,{
                    full.ets.fit <- ets(full.ts(),
                                        model = "ZZZ",
                                        damped = NULL,
                                        alpha = NULL,
                                        beta = NULL,
                                        gamma = NULL,
                                        phi = NULL,
                                        allow.multiplicative.trend = T)
                    
                    
                    full.fore <- forecast(full.ets.fit,
                                          h = input$no.days)
                                 })
                    
                    full.fore <- as.data.frame(full.fore)
                    
                    full.fore
                    
                    
                    
               })
               
               output$o.forecast <- DT::renderDataTable({
                    
                    DT:: datatable(full.ets.fit(),
                                   options = list(pageLength = 6))
                    
               })
               
               
               
               
               
               
               
               #---
          })
     
     
     
     
     
     
     
     
     
               
     
     
     
     
     
     
     
     
     
}
