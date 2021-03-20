
library(DT)
library(shiny)
library(tidyverse)



#import data
hotels <- read.csv("hotel_bookings.csv")
#data transformation
hotels <- as.data.frame(hotels)
#tidy data: select factors mainly infect the hotel booking price
hotels <- hotels %>% select(hotel,arrival_date_year,arrival_date_month,lead_time,
                            stays_in_week_nights,stays_in_weekend_nights,adr) 
#remove any data with 0 booking price
hotels <-  hotels[hotels$adr != 0.00,]
################################################################################


ui <- fluidPage(
    titlePanel("Hotel Booking Demand"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("hotel1",
                        label = "Choose a hotel type (Left)",
                        choices = list("City Hotel","Resort Hotel"),
                        selected = NULL ),
            selectInput("hotel2",
                        label = "Choose a hotel type (Right)",
                        choices = list("City Hotel","Resort Hotel"),
                        selected = NULL ),
            
            uiOutput("obs1"),
            
            uiOutput("obs2"),
            uiOutput("year1"),
            uiOutput("year2")
        ),
        
        
        
        
        mainPanel(
            #pages for showing tables and plots
            tabsetPanel(
                id='dataset',
                #page 1
                tabPanel("tables",  fluidPage(fluidRow(
                    column(6, DT::dataTableOutput("hotelchoice1") ),
                    column(6, DT::dataTableOutput("hotelchoice2") )
                    
                ))),
                #page 2
                tabPanel("Plots", fluidPage(fluidRow(
                    column(6,plotOutput("plot1")),
                    column(6, plotOutput("plot2"))
                ))) 
                
            )
        ))
)





server <- function(input, output){
    
    #give a value everytime the selection changes
    values1 <- reactiveValues(tbl = NULL, #dataset
                              obsList = NULL, plot.df = NULL)
    
    
    values2 <- reactiveValues(
        tbl = NULL, obsList = NULL)
    
    
    
    #show table on the left of main panel
    observeEvent(input$hotel1, {
        if (!NA %in% match(input$hotel1, c("City Hotel", "Resort Hotel"))) {
            
            #get dataset twice
            values1$tbl <- as.data.frame((hotels[hotels$hotel==input$hotel1,] ))
            
            #get column names in the dataset 
            values1$obsList <- colnames(values1$tbl)
            
            #select factors
            output$obs1 <- renderUI({
                selectInput(
                    inputId = "observationInput1",
                    label = "Factor for Left Plot",
                    choices =  values2$obsList[2:6]
                )
            })
            #show pictures
            output$hotelchoice1 <- DT::renderDataTable({
                values1$tbl},
                
                #add scroller
                extensions = c('Scroller', 'FixedColumns'),
                options = list(
                    deferRender = TRUE,
                    scrollX = TRUE,
                    scrollY = 800,
                    scroller = TRUE,
                    dom = 'Bfrtip',
                    fixedColumns = TRUE
                ))
        }
    })
    
    
    #show table on the right of main panel
    observeEvent(input$hotel2, {
        if (!NA %in% match(input$hotel2, c("City Hotel", "Resort Hotel"))) {
            
            #get dataset twice
            values2$tbl <- as.data.frame((hotels[hotels$hotel==input$hotel2,] ))
            
            #get column names in the dataset 
            values2$obsList <- colnames(values2$tbl)
            
            output$obs2 <- renderUI({
                selectInput(
                    inputId = "observationInput2",
                    label = "Factor for Right Plot",
                    choices =  values2$obsList[2:6] ) })
            
            output$hotelchoice2 <- DT::renderDataTable({
                values2$tbl},
                
                
                extensions = c('Scroller', 'FixedColumns'),
                options = list(
                    deferRender = TRUE,
                    scrollX = TRUE,
                    scrollY = 800,
                    scroller = TRUE,
                    dom = 'Bfrtip',
                    fixedColumns = TRUE
                ))
        }
    })
    
    
    #Plots on the left panel
    observeEvent(input$observationInput1, {
        
        output$plot1 <- renderPlot({
            if(input$observationInput1 =="arrival_date_year"){
                
                #plot Year vs price
                means <- aggregate(adr ~ arrival_date_year, values1$tbl , mean)
                
                ggplot(means,aes(x=arrival_date_year,y=adr)) +
                    geom_bar(stat = "identity", fill=c("lightskyblue2", "lightsalmon2", "mediumpurple3"))+
                    geom_text(data = means, aes(label = round(adr,2), y = adr ),
                              position = position_dodge(width = 1),vjust = -0.5, size = 3)+ 
                    xlab("Year")+ylab("Price_per_Day")
            } else if (input$observationInput1 =="arrival_date_month" ){
                
                output$year1 <- renderUI({
                    selectInput(
                        inputId = "yearchoice1",
                        label = "Year for Left Plots",
                        choices =  list("2015","2016","2017") )})
                
            } else if (input$observationInput1 == "lead_time" ) {
                
                #plot of lead_time vs price
                ggplot(values1$tbl,aes(x=lead_time,y=adr))+ geom_point(stat="identity")+
                    facet_wrap(values1$tbl$arrival_date_year)+xlim(0,365) +  ylim(0,500)+
                    xlab("lead_time")+ylab("price_per_day")
                
            } else if (input$observationInput1 == "stays_in_weekend_nights") {
                #plot of weekend_stay_length vs price
                ggplot(values1$tbl,aes(x=stays_in_weekend_nights,y=adr))+ 
                    geom_point(stat = "identity")+facet_wrap(values1$tbl$arrival_date_year)+
                    xlim(0,30)+ ylim(0,500)+xlab("stays_in_weekend_nights")+ylab("price_per_day")
                
            } else if (input$observationInput1 == "stays_in_week_nights"){
                #plot of weekday_stay_length vs price
                ggplot(values1$tbl,aes(x=stays_in_week_nights, y=adr))+geom_point(stat ="identity")+
                    facet_wrap(values1$tbl$arrival_date_year)+xlim(0,30) + ylim(0,500)+ 
                    xlab("stays_in_weekday_nights")+ylab("price_per_day") 
            }
        })
        
    })
    
    #Choice of Year on left plots
    observeEvent(input$yearchoice1,{
        
        values1$plot.df <- as.data.frame(values1$tbl)
        
        #sort the month in dataset, may need a function
        tables1 <- values1$plot.df
        tables1$arrival_date_month <- factor(tables1$arrival_date_month,
                                             levels = c("January","February","March","April","May","June","July",
                                                        "August","September","October","November","December"))
        
        years <- input$yearchoice1
        output$plot1 <- renderPlot({
            
            #plot year=2015, Month VS price
            means1 <- aggregate(adr ~ arrival_date_month, tables1[tables1$arrival_date_year== years, ], mean)
            
            ggplot(means1,aes(x=arrival_date_month,y=adr, fill=arrival_date_month))+
                geom_bar(stat = "identity")+geom_text(data = means1, aes(label = round(adr,2), y = adr ),
                                                      position = position_dodge(width = 1),vjust = -0.5, size = 3)+ 
                xlab("Month")+ylab("Price_per_Day")+theme(legend.position ="none")
        })
        
    })
    
    #Plots on the right panel
    observeEvent(input$observationInput2, {
        
        output$plot2 <- renderPlot({
            if(input$observationInput2 =="arrival_date_year"){
                
                #plot Year vs price
                means <- aggregate(adr ~ arrival_date_year, values2$tbl , mean)
                
                ggplot(means,aes(x=arrival_date_year,y=adr)) +
                    geom_bar(stat = "identity", fill=c("lightskyblue2", "lightsalmon2", "mediumpurple3"))+
                    geom_text(data = means, aes(label = round(adr,2), y = adr ),
                              position = position_dodge(width = 1),vjust = -0.5, size = 3)+ 
                    xlab("Year")+ylab("Price_per_Day")
                
            } else if (input$observationInput2 =="arrival_date_month" ){
                
                output$year2 <- renderUI({
                    selectInput(
                        inputId = "yearchoice2",
                        label = "Year for Right Plots",
                        choices =  list("2015","2016","2017") )})
                
            } else if (input$observationInput2 == "lead_time" ) {
                
                #plot of lead_time vs price
                ggplot(values2$tbl,aes(x=lead_time,y=adr))+ geom_point(stat="identity")+
                    facet_wrap(values2$tbl$arrival_date_year)+xlim(0,365)+ ylim(0,500)+
                    xlab("lead_time")+ylab("price_per_day")
                
            } else if (input$observationInput2 == "stays_in_weekend_nights") {
                #plot of weekend_stay_length vs price
                ggplot(values2$tbl,aes(x=stays_in_weekend_nights,y=adr))+ 
                    geom_point(stat = "identity")+facet_wrap(values2$tbl$arrival_date_year)+
                    xlim(0,30)+ ylim(0,500)+xlab("stays_in_weekend_nights")+ylab("price_per_day")
                
            } else if (input$observationInput2 == "stays_in_week_nights"){
                #plot of weekday_stay_length vs price
                ggplot(values2$tbl,aes(x=stays_in_week_nights, y=adr))+geom_point(stat ="identity")+
                    facet_wrap(values2$tbl$arrival_date_year)+xlim(0,30) + ylim(0,500)+ 
                    xlab("stays_in_weekday_nights")+ylab("price_per_day") 
            }
        })
        
    })
    
    #Choice of Year on right plots
    observeEvent(input$yearchoice2,{
        
        values2$plot.df <- as.data.frame(values2$tbl)
        
        #sort the month in dataset, may need a function
        tables2 <- values2$plot.df
        tables2$arrival_date_month <- factor(tables2$arrival_date_month,
                                             levels = c("January","February","March","April","May","June","July",
                                                        "August","September","October","November","December"))
        
        years <- input$yearchoice2
        output$plot2 <- renderPlot({
            
            means2 <- aggregate(adr ~ arrival_date_month, tables2[tables2$arrival_date_year== years, ], mean)
            
            ggplot(means2,aes(x=arrival_date_month,y=adr, fill=arrival_date_month)) +
                geom_bar(stat = "identity")+geom_text(data = means2, aes(label = round(adr,2), y = adr ),
                                                      position = position_dodge(width = 1),vjust = -0.5, size = 3)+ 
                xlab("Month")+ylab("Price_per_Day")+theme(legend.position ="none")
        })
        
    })
    
}

shinyApp(ui,server)