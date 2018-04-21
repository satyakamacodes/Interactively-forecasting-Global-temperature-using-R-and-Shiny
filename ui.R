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

ui <- dashboardPage(
     
     skin = "purple",
     
     dashboardHeader(
          title = "Interactively Forecasting Global Temperature",
          titleWidth = 600
     ),
     
     dashboardSidebar(
          
          width = 250,
          
          #helpText(h4("Using Exponential Smoothing")),
          
          helpText(h4("Based upon Exponential Smoothing Time Series Algorithm",
                      style = "color: #ffffff")),
          
          selectInput("Country", 
                      "Choose Country", 
                      choices = c(as.character(unique(m.data$Country))),
                      multiple = FALSE),
          
          uiOutput("state"),
          
          br(),
          
          actionButton("model.click", 
                       "Click for modeling",
                       icon = icon("cogs", "fa-3x"),
                       width = '88%',
                       style="color: #ffffff; 
                       background-color: #087a04; 
                       border-color: #087a04"),
          
          br(),
          
          actionButton("test.pred.click", 
                       "Click for model testing",
                       icon = icon("flask", "fa-3x"),
                       width = '88%',
                       style="color: #ffffff; 
                       background-color: #ea09f2; 
                       border-color: #ea09f2"),
          
          br(),
          
          actionButton("forecast.click", 
                       "Click for forecasting",
                       icon = icon("edit", "fa-3x"),
                       width = '88%',
                       style="color: #ffffff; 
                       background-color: #e52240; 
                       border-color: #e52240"),
          
          br(),
          
          br(),
          
          br(),
          
          actionButton("credit",
                       "Created by: Dr. Satyakama Paul",
                       width = '88%',
                       style="color: #ffffff; 
                       background-color: #000000; 
                       border-color: #000000"),
          
          actionButton("email",
                       "satyakama.paul@gmail.com",
                       icon = icon("envelope-open", "fa-2x"),
                       width = '88%',
                       style="color: #fff; 
                       background-color: #3b49b5; 
                       border-color: #3b49b5")
     ),
     
     dashboardBody(
          
          tags$head(
               tags$style(HTML('
                               
                               .main-header .logo {
                               font-family: Times;
                               font-weight: bold;
                               font-size: 28px;
                               }
                               
                               .content-wrapper,
                               .right-side {
                               background-color: #FFFFFF;
                               }
                               '))),
          
       
          
          #=========================================
          #First level of display
          #=========================================
          
          fluidRow(
               column(width = 4,
                      valueBoxOutput("o.rawdataview.box",
                                     width = 15)),
               
               column(width = 8,
                      valueBoxOutput("o.basicinfo.box",
                                     width = 15))
               
          ),
          
          fluidRow(
               
               column(width = 4,
                      DT::dataTableOutput("o.finaldata.view")),
               
               column(width = 8,
                      infoBoxOutput("o.number.datapoints",
                                    width = 4),
                      infoBoxOutput("o.min.date",
                                    width = 4),
                      infoBoxOutput("o.min.temp",
                                    width = 4),
                      
                      infoBoxOutput("o.perc.na",
                                    width = 4),
                      infoBoxOutput("o.max.date",
                                    width = 4),
                      
                      infoBoxOutput("o.max.temp",
                                    width = 4))
          ),
          
          br(),
          
          #=========================================
          #Second level of display
          #=========================================
          
          fluidRow(
               
               column(width = 12,
                      valueBoxOutput("o.plotly.box",
                                     width = 12))
          ),
          
          fluidRow(
               column(width = 12,
                      plotlyOutput("o.fig1"))
          ),
          
          br(),
          
          #=========================================
          #Third level of display
          #=========================================
          
          fluidRow(
               
               column(width = 4,
                      valueBoxOutput("o.traintest.box",
                                     width = 15)),
               
               column(width = 4,
                      valueBoxOutput("o.train.box.view",
                                     width = 15)),
               
               column(width = 4,
                      valueBoxOutput("o.test.box.view",
                                     width = 15))
               
          ),
          
          fluidRow(
               
               column(width =4),
               
               column(width =4,
                      DT::dataTableOutput("o.training.data.view")),
               
               column(width =4,
                      DT::dataTableOutput("o.testing.data.view"))),
          
          
          br(),         
          #=========================================
          #Fourth level of display
          #=========================================
          
          fluidRow(
               column(width = 3,
                      valueBoxOutput("o.model.box",
                                     width = 15)),
               column(width = 3,
                      valueBoxOutput("o.model.country",
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.model.state",
                                     width = 15)),
               
               column(width =3,
                      valueBoxOutput("o.no.training.data.points",
                                     width = 15))
          ),
          
          
          br(),
          
          
          fluidRow(
               
               column(width = 6, uiOutput("o.print.decomp"))
               
           ),
          
          fluidRow(
               
               column(width = 6,
                      uiOutput("o.add.formula")),
               
               column(width = 6,
                      uiOutput("o.mul.formula"))
          ),
          
          fluidRow(
               
               column(width = 6,
                      plotOutput("o.decomp.plot.additive")),

               column(width = 6,
                      plotOutput("o.decomp.plot.multiplicative"))
               
          ),
          
          
          
          
          fluidRow(
               
               uiOutput("o.print.stationarity"),
               
               column(width = 12,
                      verbatimTextOutput("o.adftest"))
          ),
          
          br(),
          
          fluidRow(
               
               uiOutput("o.print.seasonality"),
               
               column(width = 6,
                      plotOutput("o.seasonplot")),
               
               column(width = 6,
                      plotOutput("o.monthplot"))
          ),
          
          br(),
          
          fluidRow(
               
               uiOutput("o.print.correlogram"),
               
               column(width = 12,
                      plotOutput("o.acfpacf.plot"))
          ),
          
          br(),
          
          fluidRow(
               
               uiOutput("o.print.etsparameters"),
               
               column(width = 8,
                      verbatimTextOutput("o.print.model")),
               
               column(width = 4,
                      uiOutput("o.help"))
          ),
          
          br(),
          
          #=========================================
          #Fifth level of display
          #=========================================
          
          fluidRow(
               
               column(width = 3,
                      valueBoxOutput("o.test.box",
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.test.country", 
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.test.state",
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.no.testing.data.points",
                                     width = 15))
          ),
          

          fluidRow(
               
               column(width = 12,
                      dygraphOutput("o.dy.pred.on.test"))
          ),
          

          br(),
          
          fluidRow(
               
               column(width = 6,
                      valueBoxOutput("o.forecast.box",
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.forecast.country",
                                     width = 15)),
               
               column(width = 3,
                      valueBoxOutput("o.forecast.state",
                                     width = 15))
          ),
          
          #Works fine uptill here_________
          
          #=========================================
          #Seventh level of display
          #========================================= 
          
          fluidRow(
               
               column(width = 3,
                      sliderInput(inputId = "no.days",
                                  label = "No. of months of forecast",
                                  min = 1,
                                  max = 6,
                                  value = 2)),
               
               column(width = 9,
                      DT::dataTableOutput("o.forecast"))
          )
          
          
          
          
          
          
          
          
     )
)