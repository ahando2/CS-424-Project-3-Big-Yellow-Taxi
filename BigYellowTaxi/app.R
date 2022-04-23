# import libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)
library(scales)
library(jpeg)
library(grid)
library(leaflet)
library(RColorBrewer)

# get all of the tsv files in the same directory
geo_counties <- rgdal::readOGR("Boundaries - Community Areas (current).geojson") 
data_company <- lapply("companyList.tsv", read.delim)
data_company <- do.call(rbind, data_company)

data_community <- lapply("CommAreas.tsv", read.delim)
data_community <- do.call(rbind, data_community)

data_taxi <- lapply(c("Taxi_Trips_2019_Clean_1.tsv","Taxi_Trips_2019_Clean_2.tsv","Taxi_Trips_2019_Clean_3.tsv","Taxi_Trips_2019_Clean_4.tsv"), read.delim)
data_taxi <- do.call(rbind, data_taxi)

# format data
data_taxi$StartDate <- ymd(data_taxi$StartDate)
data_taxi$StartHour <- as.factor(data_taxi$StartHour)
data_taxi$Company <- as.factor(data_taxi$Company)
data_taxi$`Pickup.Community.Area` <- as.factor(data_taxi$`Pickup.Community.Area`)
data_taxi$`Dropoff.Community.Area` <- as.factor(data_taxi$`Dropoff.Community.Area`)

data_company$id <- as.character(data_company$id)

# Create the menu items to select the different years and the different stations
views<- c("Date", "Hour", "Day", "Month", "Mileage", "Trip Time")
# mapViews <- c('Default','Contrast','Geographic')
communityDir <- c("To","From")
mileageUnit <- c('KM', 'miles')
# * 1.609344
timeUnit <- c('24 hr', 'AM/PM')
# format(strptime('23', "%H"), "%I %p") 
company_names <- c(c("All"),sort(unique(data_company$companyName)))
community <- c(c("All"),sort(data_community$community))

# Create the shiny dashboard
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Big Yellow Taxi"),
                    dashboardSidebar(disable = TRUE, collapsed = TRUE,
                                     
                                     sidebarMenu( id = 'tabs',
                                                  menuItem("About", tabName = "about", icon = NULL),
                                                  menuItem("Analytic Dashboard", tabName = "dashboard", icon = NULL, selected = TRUE)
                                     )
                                     
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  column(1,div(
                                    div(style ='height: 1400px; display: flex; flex-direction: column;align-items: center; justify-content: center; padding-left:15px; padding-right:15px; padding-top: 300px;',
                                        tags$head(tags$style(HTML('.box{border-top: 0px;-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                                        box(width =12,
                                            div(
                                            selectInput("community", "Community", community, selected = "All"),
                                            checkboxInput('showToFrom', 'show Map To From ', value = FALSE),
                                            radioButtons("communityDir", "", communityDir, selected = "To"),
                                            selectInput("company", "Taxi Company", company_names, selected = "All"),
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Upper", box(width =12,fluidRow(
                                                          selectInput("view1","View",views,selected = "Date"),
                                                          radioButtons("mileageUnit1", "Mileage Unit", mileageUnit, selected = "miles"),
                                                          radioButtons("timeUnit1", "Time Unit", timeUnit, selected = "24 hr"),
                                                        ))),
                                                        tabPanel("Lower", box(width =12,fluidRow(
                                                          
                                                          selectInput("view2","View",views,selected = "Date"),
                                                          radioButtons("mileageUnit2", "Mileage Unit", mileageUnit, selected = "miles"),
                                                          radioButtons("timeUnit2", "Time Unit", timeUnit, selected = "24 hr"),
                                                        )))
                                            )))
                                    ),
                                    div(style='display: flex;justify-content: center;align-items: flex-start; margin-bottom: 20px',
                                        actionButton('aboutTab', 'about')) ,style='background: whitesmoke; margin-top: -15px; padding-bottom: 5px;')),
                                  column(2, fluidRow(tags$style(HTML(' .leaflet-top .leaflet-control-zoom {
                                                                          top:1244px
                                                                        } ')),
                                    
                                                     box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                         uiOutput("leaf",height="1368px"), height = '1432px'
                                                     )
                                  )),
                                  column(9,
                                         fluidRow(
                                           fluidRow(style='width: 100%;',
                                                    column( 12,
                                                            
                                                            fluidRow(
                                                              column(12,h2(textOutput("tab1"))),
                                                              column(12,
                                                                     
                                                                        div(
                                                                          fluidRow(
                                                                            column(10,
                                                                                   fluidRow(
                                                                                     box( title = textOutput('boxHistTitle1'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                          uiOutput("histData1", height = 500), style="height: 560px;"
                                                                                     )
                                                                                   )
                                                                            ),
                                                                            
                                                                            column(2,
                                                                                   fluidRow(
                                                                                     box(title = textOutput('boxTabTitle1'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                         uiOutput("tabData1", height = 500), style="height: 560px;"
                                                                                     )
                                                                                     
                                                                                   )),
                                                                          ))
                                                                     ),
                                                   
                                                              
                                                            ))),
                                           tags$hr(style='border-top: 8px solid #a9a9a9; margin-left:-15px;margin-top: 35px;margin-bottom: 35px;'),
                                           
                                           fluidRow(style='width: 100%;',
                                                    column( 12,
                                                            
                                                            fluidRow(
                                                              column(12,h2(textOutput("tab2"))),
                                                              column(12,
                                                                     
                                                                     div(
                                                                      
                                                                       fluidRow(
                                                                         column(10,
                                                                                fluidRow(
                                                                                  box( title = textOutput('boxHistTitle2'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                       uiOutput("histData2", height = 500), style="height: 560px;"
                                                                                  )
                                                                                )
                                                                         ),
                                                                         
                                                                         column(2,
                                                                                fluidRow(
                                                                                  box(title = textOutput('boxTabTitle2'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                      uiOutput("tabData2", height = 500), style="height: 560px;"
                                                                                  )
                                                                                  
                                                                                )),
                                                                       ))
                                                              ),
                                                              
                                                              
                                                            ))),
                                           
                                         )))),
                        tabItem(tabName = "about",
                                fluidRow(
                                  div(style='height: 1400px;display: flex;flex-direction: column;justify-content: center;',
                                      h1("About"),
                                      div(
                                        span("Written by Athalia Rochelle Handowo for CS 424 Project 3 Spring 2022 on April Data taken from Chicago Data Portal on April 18, 2022 ",
                                             style = "white-space: pre-wrap"),
                                        a(href="https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy", "(Taxi Trip data link)"),
                                        a(href="https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6", "(Community geojson link)"),
                                        span(". The application display the number of rides/trips to and from of the chosen community area per chosen timeframe or distance with its map location."),
                                        style='display:block;font-size:24px;'),
                                  )),
                                div(actionButton('dashboardTab', 'dashboard'), style= 'display: flex;align-items: flex-end;')
                                ,style='padding-right: 15px;padding-left: 15px;')
                      )
                    ))

server <- function(input, output,session) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  output$boxHistTitle1 <- renderText("Number of Rides")
  output$boxTabTitle1 <- renderText("Number of Rides as Table")

  observeEvent(input$aboutTab, {
    newtab <- switch(input$tabs, "dashboard" = "about","about" = "dashboard")
    updateTabItems(session, "tabs", newtab)
  })

  observeEvent(input$dashboardTab, {
    newtab <- switch(input$tabs, "about" = "dashboard","dashboard" = "about")
    updateTabItems(session, "tabs", newtab)
  })
  
  # generate data for window 1
  allDataReactive1 <- reactive({
    data <- data_taxi
    if (input$community != "All" && input$communityDir == "From"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Pickup.Community.Area` == currComm),]
    }
    
    if (input$community != "All" && input$communityDir == "To"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Dropoff.Community.Area` == currComm),]
    }
    
    if (input$company != "All" ){
      currComp <- data_company$id[which(input$company == data_company$companyName)]
      data <- data[which(data$Company == currComp),]
    }
    
    if ( length(data$StartHour) > 0){
      if (input$view1 == "Date"){
        dateTable<-table(data$StartDate)
        data <- as.data.frame(dateTable)
       
        colnames(data) <- c("date","rides")
        
        
        
      } else if (input$view1 == "Hour"){
        
        dateTable<-table(data$StartHour)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("hour","rides")
        if (input$timeUnit1 == "AM/PM"){
          levels(data$hour)<-format(strptime(data$hour, "%H"), "%I %p")
        }
        
      } else if (input$view1 == "Day"){
        dateTable<-table(wday(data$StartDate, label = TRUE, abbr = FALSE))
        data <- as.data.frame(dateTable)
        colnames(data) <- c("day","rides")
      } else if (input$view1 == "Month"){
        dateTable<-table(month(data$StartDate, label = TRUE, abbr = FALSE))
        data <- as.data.frame(dateTable)
        colnames(data) <- c("month","rides")
      } else if (input$view1 == "Mileage"){
        
        if (input$mileageUnit1 == "KM") {
          data$`Trip.Miles` <- data$`Trip.Miles`* 1.609344
        }
        dateTable<-table(data$`Trip.Miles`)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("mileage","rides")
        data$mileage <- as.numeric(as.character(data$mileage))
        
        
        uniqueLen <- length(unique(data$mileage))
        quant <- c()
        if( uniqueLen <= 7) {
          quant <-factor(data$mileage)
        } else {
          quantiles<- round(quantile(data$mileage, probs = seq(0, 1, 1/7)))
          quantiles <- unique(quantiles)[2:(length(unique(quantiles))-1)]
          quant <-factor(findInterval(data$mileage, quantiles))
          levelsQuant <-  c(paste('<=',quantiles[1]))
          for (i in c(1:(length(quantiles)-1))) {
            levelsQuant <-  append(levelsQuant, paste('(',quantiles[i],'-',quantiles[i+1],']'))
          }
          levelsQuant <- append(levelsQuant,paste(quantiles[length(quantiles)],'<'))
          levels(quant) <-levelsQuant
        }
        data$quantiles <- quant
        
      } else if (input$view1 == "Trip Time"){
        dateTable<-table(data$`Trip.Seconds`)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("time","rides")
        data$time <- as.numeric(as.character(data$time))
        
        uniqueLen <- length(unique(data$time))
        quant <- c()
        if( uniqueLen <= 7) {
          quant <-factor(data$time)
        } else {
          quantiles<- round(quantile(data$time, probs = seq(0, 1, 1/7)))
          quantiles <- unique(quantiles)[2:(length(unique(quantiles))-1)]
          quant <-factor(findInterval(data$time, quantiles))
          levelsQuant <-  c(paste('<=',quantiles[1]))
          for (i in c(1:(length(quantiles)-1))) {
            levelsQuant <-  append(levelsQuant, paste('(',quantiles[i],'-',quantiles[i+1],']'))
          }
          levelsQuant <- append(levelsQuant,paste(quantiles[length(quantiles)],'<'))
          levels(quant) <-levelsQuant
        }
        data$quantiles <- quant
        
      }
    }
    data
  })
  
  
  # generate data for window 2
  allDataReactive2 <- reactive({
    data <- data_taxi
    if (input$community != "All" && input$communityDir == "From"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Pickup.Community.Area` == currComm),]
    }
    
    if (input$community != "All" && input$communityDir == "To"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Dropoff.Community.Area` == currComm),]
    }
    
    if (input$company != "All" ){
      currComp <- data_company$id[which(input$company == data_company$companyName)]
      data <- data[which(data$Company == currComp),]
    }
    
    if ( length(data$StartHour) > 0){
      if (input$view2 == "Date"){
        dateTable<-table(data$StartDate)
        data <- as.data.frame(dateTable)
        
        colnames(data) <- c("date","rides")
        
        
        
      } else if (input$view2 == "Hour"){
        
        dateTable<-table(data$StartHour)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("hour","rides")
        if (input$timeUnit2 == "AM/PM"){
          levels(data$hour)<-format(strptime(data$hour, "%H"), "%I %p")
        }
        
      } else if (input$view2 == "Day"){
        dateTable<-table(wday(data$StartDate, label = TRUE, abbr = FALSE))
        data <- as.data.frame(dateTable)
        colnames(data) <- c("day","rides")
      } else if (input$view2 == "Month"){
        dateTable<-table(month(data$StartDate, label = TRUE, abbr = FALSE))
        data <- as.data.frame(dateTable)
        colnames(data) <- c("month","rides")
      } else if (input$view2 == "Mileage"){
        
        if (input$mileageUnit2 == "KM") {
          data$`Trip.Miles` <- data$`Trip.Miles`* 1.609344
        }
        dateTable<-table(data$`Trip.Miles`)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("mileage","rides")
        data$mileage <- as.numeric(as.character(data$mileage))
        
        
        uniqueLen <- length(unique(data$mileage))
        quant <- c()
        if( uniqueLen <= 7) {
          quant <-factor(data$mileage)
        } else {
          quantiles<- round(quantile(data$mileage, probs = seq(0, 1, 1/7)))
          quantiles <- unique(quantiles)[2:(length(unique(quantiles))-1)]
          quant <-factor(findInterval(data$mileage, quantiles))
          levelsQuant <-  c(paste('<=',quantiles[1]))
          for (i in c(1:(length(quantiles)-1))) {
            levelsQuant <-  append(levelsQuant, paste('(',quantiles[i],'-',quantiles[i+1],']'))
          }
          levelsQuant <- append(levelsQuant,paste(quantiles[length(quantiles)],'<'))
          levels(quant) <-levelsQuant
        }
        data$quantiles <- quant
        
      } else if (input$view2 == "Trip Time"){
        dateTable<-table(data$`Trip.Seconds`)
        data <- as.data.frame(dateTable)
        colnames(data) <- c("time","rides")
        data$time <- as.numeric(as.character(data$time))
        
        uniqueLen <- length(unique(data$time))
        quant <- c()
        if( uniqueLen <= 7) {
          quant <-factor(data$time)
        } else {
          quantiles<- round(quantile(data$time, probs = seq(0, 1, 1/7)))
          quantiles <- unique(quantiles)[2:(length(unique(quantiles))-1)]
          quant <-factor(findInterval(data$time, quantiles))
          levelsQuant <-  c(paste('<=',quantiles[1]))
          for (i in c(1:(length(quantiles)-1))) {
            levelsQuant <-  append(levelsQuant, paste('(',quantiles[i],'-',quantiles[i+1],']'))
          }
          levelsQuant <- append(levelsQuant,paste(quantiles[length(quantiles)],'<'))
          levels(quant) <-levelsQuant
        }
        data$quantiles <- quant
        
      }
    }
    data
  })
  
  allDataGeoReactive1 <- reactive({
    data <- data_taxi
    if (input$company != "All" ){
      currComp <- data_company$id[which(input$company == data_company$companyName)]
      data <- data[which(data$Company == currComp),]
    }
    
    if (input$community == "All" && input$communityDir == "From"){
      dateTable<-table(data$`Pickup.Community.Area`)
      data <- as.data.frame(dateTable)
      colnames(data) <- c("community","rides")
    }
    else if (input$community == "All" && input$communityDir == "To"){
      dateTable<-table(data$`Dropoff.Community.Area`)
      data <- as.data.frame(dateTable)
      colnames(data) <- c("community","rides")
    }
    else if (input$community != "All" && input$communityDir == "From"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Pickup.Community.Area` == currComm),]
      dateTable<-table(data$`Dropoff.Community.Area`)
      data <- as.data.frame(dateTable)
      colnames(data) <- c("community","rides")
    }
    
    else if (input$community != "All" && input$communityDir == "To"){
      currComm <- data_community$area_id[which(input$community == data_community$community)]
      data <- data[which(data$`Dropoff.Community.Area` == currComm),]
      dateTable<-table(data$`Pickup.Community.Area`)
      data <- as.data.frame(dateTable)
      colnames(data) <- c("community","rides")
    }
    
  
    data
    
    
  })
 
  #
  # window 1
  #
  output$tab1 <- renderText({paste("Rides per",input$view1,input$communityDir,input$community)})
  # show a bar chart of entries 
  output$histData1 <- renderUI({
    if(nrow(allDataReactive1()) == 0)
      return("No data to show, please try other filter")
    
    plotOutput("histDataOut1", height = 500)
  })
  output$histDataOut1 <- renderPlot({
    allData <- allDataReactive1()
    if (input$view1 == "Date"){
      ggplot(allData, aes(x=date, y=rides)) +
        labs(x="Dates", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 90))

    } else if (input$view1 == "Hour"){
      ggplot(allData, aes(x=hour, y=rides)) +
        labs(x="Hours", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 

    } else if (input$view1 == "Day"){
      ggplot(allData, aes(x=day, y=rides)) +
        labs(x="Days", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 

    } else if (input$view1 == "Month"){
      ggplot(allData, aes(x=month, y=rides)) +
        labs(x="Months", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 

    } else if (input$view1 == "Mileage"){
      ggplot(allData, aes(x=quantiles, y=rides)) +
        labs(x="Mileage", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()

    } else if (input$view1 == "Trip Time"){
      ggplot(allData, aes(x=quantiles, y=rides)) +
        labs(x="Trip Time", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()

    }
  })
    
  # show a table of entries on date1 on all stations
  output$tabData1 <- renderUI({
    if(nrow(allDataReactive1()) == 0)
      return("No data to show, please try other filter")
    
    DT::dataTableOutput("tabDataOut1", height = 500)
  })
  output$tabDataOut1 <- DT::renderDataTable(
    if (input$view1 == "Date"){
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Dates <- data.frame(date = allData$date, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    } else if (input$view1 == "Hour"){
      
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Hours <- data.frame(hour = allData$hour, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view1 == "Day"){
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Days <- data.frame(day = allData$day, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    } else if (input$view1 == "Month"){
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Months <- data.frame(month = allData$month, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view1 == "Mileage"){
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Mileage <- data.frame(mileage = allData$mileage, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view1 == "Trip Time"){
      DT::datatable({
        allData <- allDataReactive1()
        rides <- allData$rides
        Time <- data.frame(time = allData$time, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    }
  )
  
  #
  # window 2
  #
  output$tab2 <- renderText({paste("Rides per",input$view2,input$communityDir,input$community)})
  # show a bar chart of entries 
  output$histData2 <- renderUI({
    if(nrow(allDataReactive2()) == 0)
      return("No data to show, please try other filter")
    
    plotOutput("histDataOut2", height = 500)
  })
  output$histDataOut2 <- renderPlot({
    allData <- allDataReactive2()
    if (input$view2 == "Date"){
      ggplot(allData, aes(x=date, y=rides)) +
        labs(x="Dates", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
        theme(axis.text.x = element_text(angle = 90))
      
    } else if (input$view2 == "Hour"){
      ggplot(allData, aes(x=hour, y=rides)) +
        labs(x="Hours", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 
      
    } else if (input$view2 == "Day"){
      ggplot(allData, aes(x=day, y=rides)) +
        labs(x="Days", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 
      
    } else if (input$view2 == "Month"){
      ggplot(allData, aes(x=month, y=rides)) +
        labs(x="Months", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() 
      
    } else if (input$view2 == "Mileage"){
      ggplot(allData, aes(x=quantiles, y=rides)) +
        labs(x="Mileage", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
      
    } else if (input$view2 == "Trip Time"){
      ggplot(allData, aes(x=quantiles, y=rides)) +
        labs(x="Trip Time", y = "Rides") +
        geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
      
    }
  })
  
  # show a table of entries
  output$tabData2 <- renderUI({
    if(nrow(allDataReactive2()) == 0)
      return("No data to show, please try other filter")
    
    DT::dataTableOutput("tabDataOut2", height = 500)
  })
  output$tabDataOut2 <- DT::renderDataTable(
    if (input$view2 == "Date"){
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Dates <- data.frame(date = allData$date, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    } else if (input$view2 == "Hour"){
      
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Hours <- data.frame(hour = allData$hour, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view2 == "Day"){
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Days <- data.frame(day = allData$day, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    } else if (input$view2 == "Month"){
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Months <- data.frame(month = allData$month, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view2 == "Mileage"){
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Mileage <- data.frame(mileage = allData$mileage, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
      
    } else if (input$view2 == "Trip Time"){
      DT::datatable({
        allData <- allDataReactive2()
        rides <- allData$rides
        Time <- data.frame(time = allData$time, rides=rides)
      },
      options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE
      )
    }
  )
  
  output$leaf <- renderUI({
    if(nrow(allDataGeoReactive1()) == 0)
      return("No data to show, please try other filter")
    if(input$showToFrom){
      leafletOutput("leafOutDir", height = "1368px")
    } else{
      leafletOutput("leafOut", height = "1368px")
    }
  })
  output$leafOutDir <- renderLeaflet({
    allData <- allDataGeoReactive1()
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = allData$rides
    )

    map <- leaflet(geo_counties)
    map <- addTiles(map = map)
    map <- setView(map = map, lng = -87.7298, lat = 41.8781, zoom = 11)
   
    map <- addPolygons(map = map,stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                       fillColor = ~pal(allData$rides), 
                       label = ~paste0(data_community$community[which(geo_counties$area_numbe == data_community$area_id)]),
                       popup = ~paste0("<b>",data_community$community[which(geo_counties$area_numbe == data_community$area_id)],
                                     "</b><br/> Rides: ",allData$rides,
                                     "</b><br/> Rides in %: ",allData$rides*100/max(allData$rides),"%"))
    map <- addLegend(map,pal=pal, values= ~allData$rides, labFormat = labelFormat(transform = function(x) (x*100/max(allData$rides)), suffix = "%"), title = paste('Total Rides',tolower(input$communityDir), input$community) )

   
    map
  })
  
  output$leafOut <- renderLeaflet({
    allData <- allDataGeoReactive1()
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = allData$rides
    )

    
    map <- leaflet(geo_counties)
    map <- addTiles(map = map)
    map <- setView(map = map, lng = -87.7298, lat = 41.8781, zoom = 11)
    
    map <- addPolygons(map = map, smoothFactor = 0.3, fillOpacity = ~ifelse(data_community$community[which(geo_counties$area_numbe == data_community$area_id)] == input$community,0.7,0.3),
                       fillColor = ~ifelse(data_community$community[which(geo_counties$area_numbe == data_community$area_id)] == input$community,"#fff533","#6b6b6b"),
                       color = ~ifelse(data_community$community[which(geo_counties$area_numbe == data_community$area_id)] == input$community,"#fff533","#6b6b6b"), stroke = TRUE,opacity = 0.5,
                       label = ~paste0(data_community$community[which(geo_counties$area_numbe == data_community$area_id)]),
                       popup = ~paste0("<b>",data_community$community[which(geo_counties$area_numbe == data_community$area_id)],"</b><br/> Rides: ",allData$rides))
    
    map
  })
  
}

shinyApp(ui = ui, server = server)

