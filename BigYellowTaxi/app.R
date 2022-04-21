setwd("C:/Users/roche/Classes/CS 424/Project 3 Big Yellow Taxi/BigYellowTaxi")

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

data_taxi <- lapply(list.files(pattern="*.tsv")[3:6], read.delim)
data_taxi <- do.call(rbind, data_taxi)

# format data
data_taxi$StartDate <- ymd(data_taxi$StartDate)
data_taxi$StartHour <- as.factor(data_taxi$StartHour)
data_taxi$Company <- as.factor(data_taxi$Company)
data_taxi$`Pickup.Community.Area` <- as.factor(data_taxi$`Pickup.Community.Area`)
data_taxi$`Dropoff.Community.Area` <- as.factor(data_taxi$`Dropoff.Community.Area`)

data_company$id <- as.character(data_company$id)

# Create the menu items to select the different years and the different stations
views<- c("Date", "Time", "Day", "Month", "Mileage", "Trip Time")
mapViews <- c('Default','Contrast','Geographic')

mileageViews <- c('KM', 'miles')
# * 1.609344
timeViews <- c('24 hr', 'AM/PM')
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
                                            selectInput("company", "Taxi Company", company_names, selected = "All"),
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Upper", box(width =12,fluidRow(
                                                          checkboxInput('reorderMinMax2', 'Order min-max', value = FALSE),
                                                          checkboxInput('reorderMaxMin2', 'Order max-min', value = FALSE),
                                                          conditionalPanel(condition = "input.viewPanel1 != 'All Stations'",div(
                                                            # selectInput("StationName1", "Station", station_names, selected = "UIC-Halsted"),
                                                            # sliderInput("Year1","Year",value = 2021, min = 2001,max = 2021,sep="")
                                                          )),
                                                          
                                                        ))),
                                                        tabPanel("Lower", box(width =12,fluidRow(
                                                          checkboxInput('reorderMinMax2', 'Order min-max', value = FALSE),
                                                          checkboxInput('reorderMaxMin2', 'Order max-min', value = FALSE),
                                                          conditionalPanel(condition = "input.viewPanel2 != 'All Stations'",div(
                                                            # selectInput("StationName2", "Station", station_names, selected = "O'Hare Airport"),
                                                            
                                                          )),
                                                          conditionalPanel(condition = "input.viewPanel2 == 'All Stations'",div(
                                                            # dateInput("date2", label = h3("Date"), value = "2021-08-23", min = min(CTA_daily$date), max=max(CTA_daily$date), format = 'D-yyyy-mm-dd'),
                                                            
                                                            
                                                          ))
                                                        )))
                                            )))
                                    ),
                                    div(style='display: flex;justify-content: center;align-items: flex-start; margin-bottom: 20px',
                                        actionButton('aboutTab', 'about')) ,style='background: whitesmoke; margin-top: -15px; padding-bottom: 5px;')),
                                  column(11,
                                         fluidRow(
                                           
                                           fluidRow(style='width: 100%;',
                                                    column( 12,
                                                            fluidRow(
                                                              
                                                              
                                                              column(12,h2(textOutput("Tab1"))),
                                                              column(12,
                                                                     conditionalPanel(condition = "input.viewPanel1 == 'All Stations'",
                                                                                      div(
                                                                                        h3("All Stations"),
                                                                                        fluidRow(
                                                                                          column(2,
                                                                                                 fluidRow(
                                                                                                   
                                                                                                   box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                                                                       leafletOutput("leafStations1", height = 500)
                                                                                                   )
                                                                                                 )),
                                                                                          column(8,
                                                                                                 fluidRow(
                                                                                                   box( title = textOutput('boxHistStationTitle1'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                                        plotOutput("histStations1", height = 500)
                                                                                                   )
                                                                                                 )
                                                                                          ),
                                                                                          
                                                                                          column(2,
                                                                                                 fluidRow(
                                                                                                   box(title = textOutput('boxTabStationTitle1'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                                       dataTableOutput("tabStations1", height = 500)
                                                                                                   )
                                                                                                   
                                                                                                 )),
                                                                                        ))
                                                                     )),
                                                   
                                                              
                                                            ))),
                                           tags$hr(style='border-top: 8px solid #a9a9a9; margin-left:-15px;'),
                                           
                                           fluidRow(style='width: 100%;',
                                                    column( 12,
                                                            fluidRow(
                                                              column(12,h2(textOutput("Tab2"))),
                                                              column(12,
                                                                     conditionalPanel(condition = "input.viewPanel2 == 'All Stations'",
                                                                                      div(
                                                                                        h3("All Stations"),
                                                                                        fluidRow(
                                                                                          column(2,
                                                                                                 fluidRow(
                                                                                                   
                                                                                                   box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                                                                                                       leafletOutput("leafStations2", height = 500)
                                                                                                   )
                                                                                                 )),
                                                                                          column(8,
                                                                                                 fluidRow(
                                                                                                   box( title = textOutput('boxHistStationTitle2'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                                        plotOutput("histStations2", height = 500)
                                                                                                   )
                                                                                                 )
                                                                                          ),
                                                                                          
                                                                                          column(2,
                                                                                                 fluidRow(
                                                                                                   box(title = textOutput('boxTabStationTitle2'), solidHeader = TRUE, status = "primary", width = 12,
                                                                                                       dataTableOutput("tabStations2", height = 500)
                                                                                                   )
                                                                                                   
                                                                                                 )),
                                                                                        ))
                                                                     )),
                                                              
                                                            )))
                                           
                                         )))),
                        tabItem(tabName = "about",
                                fluidRow(
                                  div(style='height: 1400px;display: flex;flex-direction: column;justify-content: center;',
                                      h1("About"),
                                      div(
                                        span("Written by Athalia Rochelle Handowo for CS 424 Project 2 Spring 2022 on March Data taken from Chicago Data Portal on March 4, 2022 ",
                                             style = "white-space: pre-wrap"),
                                        a(href="https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f", "(Rides data link)"),
                                        a(href="https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme", "(Location data link)"),
                                        span(". Additional missing location data are take from ",style = "white-space: pre-wrap"),
                                        a(href="https://en.wikipedia.org/", "Wikipedia"),
                                        span(". The application display the number of rides/entries of the chosen stations per chosen timeframe and year with its map location."),
                                        style='display:block;font-size:24px;'),
                                  )),
                                div(actionButton('dashboardTab', 'dashboard'), style= 'display: flex;align-items: flex-end;')
                                ,style='padding-right: 15px;padding-left: 15px;')
                      )
                    ))

server <- function(input, output,session) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 14)) 
  
  # output$Tab1 <- renderText({ 
  #   date_format <- ymd(input$date1)
  #   
  #   if (input$viewPanel1 == 'All Stations' && input$compareDate1){
  #     date2_format <- ymd(input$secondDate1)
  #     paste(wday(date_format,label=TRUE, abbr = FALSE),',',month(date_format,label=TRUE, abbr = FALSE),' ', mday(date_format), ' ', year(date_format)," & ",
  #           wday(date2_format,label=TRUE, abbr = FALSE),',',month(date2_format,label=TRUE, abbr = FALSE),' ', mday(date2_format), ' ', year(date2_format))
  #   }
  #   else if (input$viewPanel1 == 'All Stations'){
  #     paste(wday(date_format,label=TRUE, abbr = FALSE),',',month(date_format,label=TRUE, abbr = FALSE),' ', mday(date_format), ' ', year(date_format))
  #   }
  #   else{paste(input$StationName1,' ', input$Year1) }
  # })
  # 
  # output$Tab2 <- renderText({ 
  #   date_format <- ymd(input$date2)
  #   
  #   if (input$viewPanel2 == 'All Stations' && input$compareDate2){
  #     date2_format <- ymd(input$secondDate2)
  #     paste(wday(date_format,label=TRUE, abbr = FALSE),',',month(date_format,label=TRUE, abbr = FALSE),' ', mday(date_format), ' ', year(date_format)," & ",
  #           wday(date2_format,label=TRUE, abbr = FALSE),',',month(date2_format,label=TRUE, abbr = FALSE),' ', mday(date2_format), ' ', year(date2_format))
  #   }
  #   else if (input$viewPanel2 == 'All Stations'){
  #     paste(wday(date_format,label=TRUE, abbr = FALSE),',',month(date_format,label=TRUE, abbr = FALSE),' ', mday(date_format), ' ', year(date_format))
  #   }
  #   else{paste(input$StationName2,' ', input$Year2) }
  # })
  # 
  # output$boxHistStationTitle1 <- renderText({ifelse(!input$compareDate1,"Entries For Each Stations", "Entries Difference For Each Stations")})
  # output$boxHistStationTitle2 <- renderText({ifelse(!input$compareDate2,"Entries For Each Stations", "Entries Difference For Each Stations")})
  # output$boxTabStationTitle1 <- renderText({ifelse(!input$compareDate1,"Entries For Each Stations as Table", "Entries Difference For Each Stations as Table")})
  # output$boxTabStationTitle2 <- renderText({ifelse(!input$compareDate2,"Entries For Each Stations as Table", "Entries Difference For Each Stations as Table")})
  # 
  observeEvent(input$aboutTab, {
    newtab <- switch(input$tabs, "dashboard" = "about","about" = "dashboard")
    updateTabItems(session, "tabs", newtab)
  })

  observeEvent(input$dashboardTab, {
    newtab <- switch(input$tabs, "about" = "dashboard","dashboard" = "about")
    updateTabItems(session, "tabs", newtab)
  })
  # 
  # observeEvent(input$reorderMinMax1, {
  #   if(input$reorderMinMax1 && input$reorderMaxMin1){
  #     updateCheckboxInput(session, "reorderMaxMin1", value = FALSE)
  #   }
  # })
  # 
  # observeEvent(input$reorderMaxMin1, {
  #   if(input$reorderMinMax1 && input$reorderMaxMin1){
  #     updateCheckboxInput(session, "reorderMinMax1", value = FALSE)
  #   }
  # })
  # 
  # observeEvent(input$reorderMinMax2, {
  #   if(input$reorderMinMax2 && input$reorderMaxMin2){
  #     updateCheckboxInput(session, "reorderMaxMin2", value = FALSE)
  #   }
  # })
  # 
  # observeEvent(input$reorderMaxMin2, {
  #   if(input$reorderMinMax2 && input$reorderMaxMin2){
  #     updateCheckboxInput(session, "reorderMinMax2", value = FALSE)
  #   }
  # })
  # 
  # observeEvent(input$secondDate1, {
  #   if(input$date1 == input$secondDate1){
  #     if(input$secondDate1 < max(CTA_daily$date)){updateDateInput(session, "secondDate1",value = ymd(input$secondDate1) + days(1))}
  #     else{updateDateInput(session, "secondDate1",value = ymd(input$secondDate1) - days(1))}
  #   }
  # })
  # 
  # observeEvent(input$secondDate2, {
  #   if(input$date2 == input$secondDate2){
  #     if(input$secondDate2 < max(CTA_daily$date)){updateDateInput(session, "secondDate2",value = ymd(input$secondDate2) + days(1))}
  #     else{updateDateInput(session, "secondDate2",value = ymd(input$secondDate2) - days(1))}
  #   }
  # })
  # 
  # observeEvent(input$date1, {
  #   if(input$date1 == input$secondDate1){
  #     if(input$date1 < max(CTA_daily$date)){updateDateInput(session, "date1",value = ymd(input$date1) + days(1))}
  #     else{updateDateInput(session, "date1",value = ymd(input$date1) - days(1))}
  #   }
  # })
  # 
  # observeEvent(input$date2, {
  #   if(input$date2 == input$secondDate2){
  #     if(input$date2 < max(CTA_daily$date)){updateDateInput(session, "date2",value = ymd(input$date2) + days(1))}
  #     else{updateDateInput(session, "date2",value = ymd(input$date2) - days(1))}
  #   }
  # })
  # 
  # 
  # observeEvent(input$nextDate1, {
  #   if(input$date1 < max(CTA_daily$date)){updateDateInput(session, "date1",value = ymd(input$date1) + days(1))}
  # })
  # 
  # observeEvent(input$nextDate2, {
  #   if(input$date2 < max(CTA_daily$date)){updateDateInput(session, "date2",value = ymd(input$date2) + days(1))}
  # })
  # 
  # observeEvent(input$prevDate1, {
  #   if(input$date1 > min(CTA_daily$date)){updateDateInput(session, "date1",value = ymd(input$date1) - days(1))}
  # })
  # 
  # observeEvent(input$prevDate2, {
  #   if(input$date2 > min(CTA_daily$date)){updateDateInput(session, "date2",value = ymd(input$date2) - days(1))}
  # })
  # 
  # observeEvent(input$next2Date1, {
  #   if(input$secondDate1 < max(CTA_daily$date)){updateDateInput(session, "secondDate1",value = ymd(input$secondDate1) + days(1))}
  # })
  # 
  # observeEvent(input$next2Date2, {
  #   if(input$secondDate2 < max(CTA_daily$date)){updateDateInput(session, "secondDate2",value = ymd(input$secondDate2) + days(1))}
  # })
  # 
  # observeEvent(input$prev2Date1, {
  #   if(input$secondDate1 > min(CTA_daily$date)){updateDateInput(session, "secondDate1",value = ymd(input$secondDate1) - days(1))}
  # })
  # 
  # observeEvent(input$prev2Date2, {
  #   if(input$secondDate2 > min(CTA_daily$date)){updateDateInput(session, "secondDate2",value = ymd(input$secondDate2) - days(1))}
  # })
  # 
  # observeEvent(input$resetMap1, {
  #   if(input$viewPanel1 == 'All Stations'){
  #     output$leafStations1 <- renderLeaflet({
  #       location <- allStationDateReactive1()
  #       if(input$compareDate1){location <-allStationCompareDateReactive1()}
  #       map <- leaflet(location)
  #       map <- addTiles(map)
  #       if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile1 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
  #       if (input$compareDate1){
  #         pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #         map <- addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                                 color = ~pal(location$rides), label = location$stationname,
  #                                 stroke = FALSE, fillOpacity = 0.9,
  #                                 popup = paste("<b>",location$stationname,"</b><br/> Rides difference: ",location$rides,
  #                                               "<br/> Rides on ",input$date1,": ",location$rides_1,
  #                                               "<br/> Rides on ",input$secondDate1,": ",location$rides_2)
  #         )
  #         map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Entries Difference' )
  #         
  #       }
  #       else {
  #         pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #         map <-  addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                                  color = ~pal(location$rides), label = location$stationname,
  #                                  stroke = FALSE, fillOpacity = 0.9,
  #                                  popup = paste("<b>",location$stationname,"</b><br/> Rides: ",location$rides))
  #         
  #         map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Total Entries' )
  #       }
  #       map
  #     })
  #   }
  #   if(input$viewPanel1 == 'Year'){
  #     output$leafYear1<- renderLeaflet({
  #       location <- stationNameLocationReactive1()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #       
  #       if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile1 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel1 == 'Month'){
  #     output$leafMonth1<- renderLeaflet({
  #       location <- stationNameLocationReactive1()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #       
  #       if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile1 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel1 == 'Day'){
  #     output$leafDay1<- renderLeaflet({
  #       location <- stationNameLocationReactive1()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #       
  #       if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile1 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel1 == 'Date'){
  #     output$leafDate1<- renderLeaflet({
  #       location <- stationNameLocationReactive1()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #       
  #       if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile1 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  # })
  # 
  # observeEvent(input$resetMap2, {
  #   if(input$viewPanel2 == 'All Stations'){
  #     output$leafStations2 <- renderLeaflet({
  #       location <- allStationDateReactive2()
  #       if(input$compareDate2){location <-allStationCompareDateReactive2()}
  #       map <- leaflet(location)
  #       map <- addTiles(map)
  #       if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile2 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
  #       if (input$compareDate2){
  #         pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #         map <- addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                                 color = ~pal(location$rides),label = location$stationname,
  #                                 stroke = FALSE, fillOpacity = 0.9,
  #                                 popup = paste("<b>",location$stationname,"</b><br/> Rides difference: ",location$rides,
  #                                               "<br/> Rides on ",input$date2,": ",location$rides_1,
  #                                               "<br/> Rides on ",input$secondDate2,": ",location$rides_2)
  #         )
  #         map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Entries Difference' )
  #       }
  #       else {
  #         pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #         map <-  addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                                  color = ~pal(location$rides), label = location$stationname,
  #                                  stroke = FALSE, fillOpacity = 0.9,
  #                                  popup = paste("<b>",location$stationname,"</b><br/> Rides: ",location$rides))
  #         
  #         map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Total Entries' )
  #       }
  #       map
  #     })
  #   }
  #   if(input$viewPanel2 == 'Year'){
  #     
  #     output$leafYear2<- renderLeaflet({
  #       location <- stationNameLocationReactive2()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #       
  #       if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile2 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel2 == 'Month'){
  #     output$leafMonth2<- renderLeaflet({
  #       location <- stationNameLocationReactive2()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #       
  #       if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile2 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel2 == 'Day'){
  #     output$leafDay2<- renderLeaflet({
  #       location <- stationNameLocationReactive2()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #       
  #       if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile2 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  #   if(input$viewPanel2 == 'Date'){
  #     output$leafDate2<- renderLeaflet({
  #       location <- stationNameLocationReactive2()
  #       map <- leaflet(CTA_location)
  #       map <- addTiles(map)
  #       longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #       latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #       
  #       if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #       if(input$mapTile2 == 'Geographic') {
  #         map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #         map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #       }
  #       
  #       map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #       map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #       map
  #     })
  #   }
  # })
  # 
  # # generate data for window 1
  # allStationDateReactive1 <- reactive({
  #   dataDate <- subset(CTA_daily, date == input$date1)
  #   quantiles<- round(quantile(dataDate$rides, probs = seq(0, 1, 1/7)))
  #   dataDate$quantiles <-factor(findInterval(dataDate$rides, quantiles[2:7]))
  #   levels(dataDate$quantiles) <- c(paste('<=',quantiles[2]),
  #                                   paste('(',quantiles[2],'-',quantiles[3],']'),
  #                                   paste('(',quantiles[3],'-',quantiles[4],']'),
  #                                   paste('(',quantiles[4],'-',quantiles[5],']'),
  #                                   paste('(',quantiles[5],'-',quantiles[6],']'),
  #                                   paste('(',quantiles[6],'-',quantiles[7],']'),
  #                                   paste(quantiles[7],'<'))
  #   dataDate
  # })
  # allStationCompareDateReactive1 <- reactive({
  #   dataDate1 <- subset(CTA_daily, date == input$date1)
  #   dataDate2 <- subset(CTA_daily, date == input$secondDate1)[,c("station_id","rides")]
  #   dataDate1$date <- NULL
  #   dataDate <- merge(dataDate1,dataDate2,by = "station_id")
  #   names(dataDate)[names(dataDate)=='rides.x'] <- "rides_1"
  #   names(dataDate)[names(dataDate)=='rides.y'] <- "rides_2"
  #   
  #   dataDate$rides <- dataDate$rides_2 - dataDate$rides_1
  #   quantiles<- round(quantile(dataDate$rides, probs = seq(0, 1, 1/7)))
  #   dataDate$quantiles <-factor(findInterval(dataDate$rides, quantiles[2:7]))
  #   levels(dataDate$quantiles) <- c(paste('<=',quantiles[2]),
  #                                   paste('(',quantiles[2],'-',quantiles[3],']'),
  #                                   paste('(',quantiles[3],'-',quantiles[4],']'),
  #                                   paste('(',quantiles[4],'-',quantiles[5],']'),
  #                                   paste('(',quantiles[5],'-',quantiles[6],']'),
  #                                   paste('(',quantiles[6],'-',quantiles[7],']'),
  #                                   paste(quantiles[7],'<'))
  #   dataDate
  # })
  # 
  # stationNameReactive1 <- reactive({subset(CTA_daily, stationname == input$StationName1)})
  # stationNameandYearsReactive1 <- reactive({subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)})
  # 
  # stationNameLocationReactive1 <- reactive({subset(CTA_location, station_id == CTA_daily$station_id[[which(CTA_daily$stationname== input$StationName1)[1]]])})
  # 
  # 
  # stationNameYearsReactive1 <- reactive({
  #   data <- subset(CTA_daily, stationname == input$StationName1)
  #   tapply(data$rides, year(data$date), FUN=sum)
  # })
  # stationNameMonthsReactive1 <- reactive({
  #   data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
  #   tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
  # })
  # stationNameDaysReactive1 <- reactive({
  #   data <- subset(CTA_daily, year(date) == input$Year1 & stationname == input$StationName1)
  #   tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  # })
  # 
  # # generate data for window 2
  # allStationDateReactive2 <- reactive({
  #   dataDate<-subset(CTA_daily, date == input$date2)
  #   
  #   quantiles<- round(quantile(dataDate$rides, probs = seq(0, 1, 1/7)))
  #   dataDate$quantiles <-factor(findInterval(dataDate$rides, quantiles[2:7]))
  #   levels(dataDate$quantiles) <- c(paste('<=',quantiles[2]),
  #                                   paste('(',quantiles[2],'-',quantiles[3],']'),
  #                                   paste('(',quantiles[3],'-',quantiles[4],']'),
  #                                   paste('(',quantiles[4],'-',quantiles[5],']'),
  #                                   paste('(',quantiles[5],'-',quantiles[6],']'),
  #                                   paste('(',quantiles[6],'-',quantiles[7],']'),
  #                                   paste(quantiles[7],'<'))
  #   dataDate
  # })
  # allStationCompareDateReactive2 <- reactive({
  #   dataDate1 <- subset(CTA_daily, date == input$date2)
  #   dataDate2 <- subset(CTA_daily, date == input$secondDate2)[,c("station_id","rides")]
  #   dataDate1$date <- NULL
  #   dataDate <- merge(dataDate1,dataDate2,by = "station_id")
  #   names(dataDate)[names(dataDate)=='rides.x'] <- "rides_1"
  #   names(dataDate)[names(dataDate)=='rides.y'] <- "rides_2"
  #   
  #   dataDate$rides <- dataDate$rides_2 - dataDate$rides_1
  #   quantiles<- round(quantile(dataDate$rides, probs = seq(0, 1, 1/7)))
  #   dataDate$quantiles <-factor(findInterval(dataDate$rides, quantiles[2:7]))
  #   levels(dataDate$quantiles) <- c(paste('<=',quantiles[2]),
  #                                   paste('(',quantiles[2],'-',quantiles[3],']'),
  #                                   paste('(',quantiles[3],'-',quantiles[4],']'),
  #                                   paste('(',quantiles[4],'-',quantiles[5],']'),
  #                                   paste('(',quantiles[5],'-',quantiles[6],']'),
  #                                   paste('(',quantiles[6],'-',quantiles[7],']'),
  #                                   paste(quantiles[7],'<'))
  #   dataDate
  # })
  # 
  # stationNameReactive2 <- reactive({subset(CTA_daily, stationname == input$StationName2)})
  # stationNameandYearsReactive2 <- reactive({subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)})
  # 
  # stationNameLocationReactive2 <- reactive({subset(CTA_location, station_id == CTA_daily$station_id[[which(CTA_daily$stationname== input$StationName2)[1]]])})
  # 
  # stationNameYearsReactive2 <- reactive({
  #   data <- subset(CTA_daily, stationname == input$StationName2)
  #   tapply(data$rides, year(data$date), FUN=sum)
  # })
  # stationNameMonthsReactive2 <- reactive({
  #   data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
  #   tapply(data$rides, month(data$date,label = TRUE), FUN=sum)
  # })
  # stationNameDaysReactive2 <- reactive({
  #   data <- subset(CTA_daily, year(date) == input$Year2 & stationname == input$StationName2)
  #   tapply(data$rides, wday(data$date, label=TRUE), FUN=sum)
  # })
  # 
  # 
  # # 
  # # window 1
  # # 
  # 
  # # show a bar chart of entries on date1 at all Station
  # output$histStations1 <- renderPlot({
  #   allStationDate <- allStationDateReactive1()
  #   if( input$compareDate1){ allStationDate <- allStationCompareDateReactive1()}
  #   
  #   if(input$compareDate1 & input$reorderMinMax1){
  #     ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if(input$compareDate1 & input$reorderMaxMin1){
  #     ggplot(allStationDate, aes(x=reorder(stationname,-rides), y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if(input$compareDate1){
  #     ggplot(allStationDate, aes(x=stationname, y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if ( input$reorderMinMax1){
  #     ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  #   
  #   else if ( input$reorderMaxMin1){
  #     ggplot(allStationDate, aes(x=reorder(stationname,-rides), y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  #   else{
  #     ggplot(allStationDate, aes(x=stationname, y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  # })
  # 
  # # show a bar chart of entries per Year at StationName1
  # output$histYear1 <- renderPlot({
  #   oneYear <- stationNameReactive1()
  #   if(input$reorderMinMax1){
  #     ggplot(oneYear, aes(x=reorder(as.character.Date(year(date)),+rides), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin1){
  #     ggplot(oneYear, aes(x=reorder(as.character.Date(year(date)),-rides), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=as.character.Date(year(date)), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  # })
  # 
  # # show a bar chart of entries per Date at StationName1
  # output$histDate1 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive1()
  #   if(input$reorderMinMax1){
  #     ggplot(oneYear, aes(x=reorder(date,+rides), y=rides)) + 
  #       labs(x=paste("Dates in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       theme(axis.text.x = element_text(angle = 90))
  #   }
  #   else if(input$reorderMaxMin1){
  #     ggplot(oneYear, aes(x=reorder(date,-rides), y=rides)) + 
  #       labs(x=paste("Dates in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       theme(axis.text.x = element_text(angle = 90))
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=date, y=rides)) + 
  #       labs(x=paste("Dates in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       scale_x_date(date_labels = "%b-%d",date_breaks ='1 weeks')
  #   }
  #   
  # })
  # 
  # 
  # # show a bar chart of entries per Month at StationName1
  # output$histMonth1 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive1()
  #   if(input$reorderMinMax1){
  #     ggplot(oneYear, aes(x=reorder(month(date,label = TRUE),+rides), y=rides)) + 
  #       labs(x=paste("Months in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin1){
  #     ggplot(oneYear, aes(x=reorder(month(date,label = TRUE),-rides), y=rides)) + 
  #       labs(x=paste("Months in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
  #       labs(x=paste("Months in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  # })
  # 
  # 
  # # show a bar chart of entries per Day at StationName1
  # output$histDay1 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive1()
  #   if(input$reorderMinMax1){
  #     ggplot(oneYear, aes(x=reorder(wday(date,label = TRUE),+rides), y=rides)) + 
  #       labs(x=paste("Days in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin1){
  #     ggplot(oneYear, aes(x=reorder(wday(date,label = TRUE),-rides), y=rides)) + 
  #       labs(x=paste("Days in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
  #       labs(x=paste("Days in", input$Year1), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   
  # })
  # 
  # 
  # # show a table of entries on date1 on all stations
  # output$tabStations1 <- DT::renderDataTable(
  #   if (input$compareDate1){
  #     DT::datatable({
  #       allStationDate <- allStationCompareDateReactive1()
  #       date1<- allStationDate$rides_1
  #       date2<- allStationDate$rides_2
  #       rides_difference <- allStationDate$rides
  #       Dates <- data.frame(stationname = allStationDate$stationname, "date1 rides"=date1, "date2 rides"=date2,"rides difference"=rides_difference)
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax1 || input$reorderMaxMin1,3,0), ifelse(input$reorderMaxMin1,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else {DT::datatable({
  #     allStationDate <- allStationDateReactive1()
  #     rides <- allStationDate$rides
  #     Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax1 || input$reorderMaxMin1,1,0), ifelse(input$reorderMaxMin1,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  #   }
  # )
  # 
  # # show a table of entries per Year at StationName1
  # output$tabYear1 <- DT::renderDataTable(
  #   DT::datatable({
  #     rides <-  stationNameYearsReactive1()
  #     Dates <- data.frame(years = names(rides), rides=as.numeric(rides))
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax1 || input$reorderMaxMin1,1,0), ifelse(input$reorderMaxMin1,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  # )
  # 
  # # show a table of entries per Date at StationName1
  # output$tabDate1 <- DT::renderDataTable(
  #   DT::datatable({
  #     oneYear <-  stationNameandYearsReactive1()
  #     Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax1 || input$reorderMaxMin1,1,0), ifelse(input$reorderMaxMin1,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  # )
  # 
  # # show a table of entries per Month at StationName1
  # output$tabMonth1 <- DT::renderDataTable(
  #   if (input$reorderMinMax1|| input$reorderMaxMin1){
  #     DT::datatable({
  #       rides <-  stationNameMonthsReactive1()
  #       Months <- data.frame(months = names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1,ifelse(input$reorderMaxMin1,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else {DT::datatable({
  #     rides <-  stationNameMonthsReactive1()
  #     Months <- data.frame(months = names(rides), rides=as.numeric(rides))
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE
  #   ), rownames = FALSE
  #   )}
  # )
  # 
  # # show a table of entries per Day at StationName1  
  # output$tabDay1 <- DT::renderDataTable(
  #   if (input$reorderMinMax1|| input$reorderMaxMin1){
  #     DT::datatable({
  #       rides <-  stationNameDaysReactive1()
  #       Dates <- data.frame(days=names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1,ifelse(input$reorderMaxMin1,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else{
  #     DT::datatable({
  #       rides <-  stationNameDaysReactive1()
  #       Dates <- data.frame(days=names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE
  #     ), rownames = FALSE
  #     )
  #   }
  # )
  # 
  # output$leafStations1 <- renderLeaflet({
  #   location <- allStationDateReactive1()
  #   if(input$compareDate1){location <-allStationCompareDateReactive1()}
  #   map <- leaflet(location)
  #   map <- addTiles(map)
  #   if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile1 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
  #   if (input$compareDate1){
  #     pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #     map <- addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                             color = ~pal(location$rides),
  #                             stroke = FALSE, fillOpacity = 0.9,
  #                             popup = paste("<b>",location$stationname,"</b><br/> Rides difference: ",location$rides,
  #                                           "<br/> Rides on ",input$date1,": ",location$rides_1,
  #                                           "<br/> Rides on ",input$secondDate1,": ",location$rides_2)
  #     )
  #     map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Entries Difference' )
  #     
  #   }
  #   else {
  #     pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #     map <-  addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                              color = ~pal(location$rides), label = location$stationname,
  #                              stroke = FALSE, fillOpacity = 0.9,
  #                              popup = paste("<b>",location$stationname,"</b><br/> Rides: ",location$rides))
  #     
  #     map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Total Entries' )
  #   }
  #   map
  # })
  # 
  # output$leafYear1<- renderLeaflet({
  #   location <- stationNameLocationReactive1()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #   
  #   if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile1 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafMonth1<- renderLeaflet({
  #   location <- stationNameLocationReactive1()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #   
  #   if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile1 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafDay1<- renderLeaflet({
  #   location <- stationNameLocationReactive1()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #   
  #   if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile1 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafDate1<- renderLeaflet({
  #   location <- stationNameLocationReactive1()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName1)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName1)]
  #   
  #   if(input$mapTile1 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile1 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # 
  # # 
  # # window 2
  # # 
  # 
  # # show a bar chart of entries on date2 at all Station
  # output$histStations2 <- renderPlot({
  #   allStationDate <- allStationDateReactive2()
  #   if( input$compareDate2){ allStationDate <- allStationCompareDateReactive2()}
  #   
  #   if(input$compareDate2 & input$reorderMinMax2){
  #     ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if(input$compareDate2 & input$reorderMaxMin2){
  #     ggplot(allStationDate, aes(x=reorder(stationname,-rides), y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if(input$compareDate2){
  #     ggplot(allStationDate, aes(x=stationname, y=rides,fill=quantiles)) + 
  #       labs(x="Station Name", y = "Rides", fill='Entries Difference') + 
  #       geom_bar(stat="identity") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))+
  #       scale_fill_brewer(palette = "RdYlBu",direction = -1)
  #   }
  #   
  #   else if ( input$reorderMinMax2){
  #     ggplot(allStationDate, aes(x=reorder(stationname,+rides), y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  #   
  #   else if ( input$reorderMaxMin2){
  #     ggplot(allStationDate, aes(x=reorder(stationname,-rides), y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  #   else{
  #     ggplot(allStationDate, aes(x=stationname, y=rides)) + 
  #       labs(x="Station Name", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous() +
  #       theme(axis.text.x = element_text(angle = 45))
  #   }
  # })
  # 
  # # show a bar chart of entries per Year at StationName2
  # output$histYear2 <- renderPlot({
  #   oneYear <- stationNameReactive2()
  #   if(input$reorderMinMax2){
  #     ggplot(oneYear, aes(x=reorder(as.character.Date(year(date)),+rides), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin2){
  #     ggplot(oneYear, aes(x=reorder(as.character.Date(year(date)),-rides), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=as.character.Date(year(date)), y=rides)) + 
  #       labs(x="Years", y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  # })
  # 
  # # show a bar chart of entries per Date at StationName2
  # output$histDate2 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive2()
  #   if(input$reorderMinMax2){
  #     ggplot(oneYear, aes(x=reorder(date,+rides), y=rides)) + 
  #       labs(x=paste("Dates in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       theme(axis.text.x = element_text(angle = 90))
  #   }
  #   else if(input$reorderMaxMin2){
  #     ggplot(oneYear, aes(x=reorder(date,-rides), y=rides)) + 
  #       labs(x=paste("Dates in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       theme(axis.text.x = element_text(angle = 90))
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=date, y=rides)) + 
  #       labs(x=paste("Dates in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()+
  #       scale_x_date(date_labels = "%b-%d",date_breaks ='1 weeks')
  #   }
  #   
  # })
  # 
  # 
  # # show a bar chart of entries per Month at StationName2
  # output$histMonth2 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive2()
  #   if(input$reorderMinMax2){
  #     ggplot(oneYear, aes(x=reorder(month(date,label = TRUE),+rides), y=rides)) + 
  #       labs(x=paste("Months in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin2){
  #     ggplot(oneYear, aes(x=reorder(month(date,label = TRUE),-rides), y=rides)) + 
  #       labs(x=paste("Months in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=month(date,label = TRUE), y=rides)) + 
  #       labs(x=paste("Months in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  # })
  # 
  # 
  # # show a bar chart of entries per Day at StationName2
  # output$histDay2 <- renderPlot({
  #   oneYear <- stationNameandYearsReactive2()
  #   if(input$reorderMinMax2){
  #     ggplot(oneYear, aes(x=reorder(wday(date,label = TRUE),+rides), y=rides)) + 
  #       labs(x=paste("Days in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else if(input$reorderMaxMin2){
  #     ggplot(oneYear, aes(x=reorder(wday(date,label = TRUE),-rides), y=rides)) + 
  #       labs(x=paste("Days in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   else{
  #     ggplot(oneYear, aes(x=wday(date,label = TRUE), y=rides)) + 
  #       labs(x=paste("Days in", input$Year2), y = "Rides") + 
  #       geom_bar(stat="identity", fill="steelblue") + scale_y_continuous()
  #   }
  #   
  # })
  # 
  # # show a table of entries on date2 on all stations
  # output$tabStations2 <- DT::renderDataTable(
  #   if (input$compareDate2){
  #     DT::datatable({
  #       allStationDate <- allStationCompareDateReactive2()
  #       date1<- allStationDate$rides_1
  #       date2<- allStationDate$rides_2
  #       rides_difference <- allStationDate$rides
  #       Dates <- data.frame(stationname = allStationDate$stationname, "date1 rides"=date1, "date2 rides"=date2,"rides difference"=rides_difference)
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax2 || input$reorderMaxMin2,3,0), ifelse(input$reorderMaxMin2,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else {DT::datatable({
  #     allStationDate <- allStationDateReactive2()
  #     rides <- allStationDate$rides
  #     Dates <- data.frame(stationname = allStationDate$stationname, rides=rides)
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax2 || input$reorderMaxMin2,1,0), ifelse(input$reorderMaxMin2,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  #   }
  # )
  # 
  # # show a table of entries per Year at StationName2
  # output$tabYear2 <- DT::renderDataTable(
  #   DT::datatable({
  #     rides <-  stationNameYearsReactive2()
  #     Dates <- data.frame(years = names(rides), rides=as.numeric(rides))
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax2 || input$reorderMaxMin2,1,0), ifelse(input$reorderMaxMin2,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  # )
  # 
  # # show a table of entries per Date at StationName2
  # output$tabDate2 <- DT::renderDataTable(
  #   DT::datatable({
  #     oneYear <-  stationNameandYearsReactive2()
  #     Dates <- data.frame(dates=oneYear$date, rides=oneYear$rides)
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(ifelse(input$reorderMinMax2 || input$reorderMaxMin2,1,0), ifelse(input$reorderMaxMin2,'desc','asc')))
  #   ), rownames = FALSE
  #   )
  # )
  # 
  # # show a table of entries per Month at StationName2
  # output$tabMonth2 <- DT::renderDataTable(
  #   if (input$reorderMinMax2 || input$reorderMaxMin2){
  #     DT::datatable({
  #       rides <-  stationNameMonthsReactive2()
  #       Months <- data.frame(months = names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1,ifelse(input$reorderMaxMin2,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else {DT::datatable({
  #     rides <-  stationNameMonthsReactive2()
  #     Months <- data.frame(months = names(rides), rides=as.numeric(rides))
  #   },
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE
  #   ), rownames = FALSE
  #   )}
  # )
  # 
  # # show a table of entries per Day at StationName2  
  # output$tabDay2 <- DT::renderDataTable(
  #   if (input$reorderMinMax2 || input$reorderMaxMin2){
  #     DT::datatable({
  #       rides <-  stationNameDaysReactive2()
  #       Dates <- data.frame(days=names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1,ifelse(input$reorderMaxMin2,'desc','asc')))
  #     ), rownames = FALSE
  #     )
  #   }
  #   else{
  #     DT::datatable({
  #       rides <-  stationNameDaysReactive2()
  #       Dates <- data.frame(days=names(rides), rides=as.numeric(rides))
  #     },
  #     options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE
  #     ), rownames = FALSE
  #     )
  #   }
  # )
  # 
  # output$leafStations2 <- renderLeaflet({
  #   location <- allStationDateReactive2()
  #   if(input$compareDate2){location <-allStationCompareDateReactive2()}
  #   map <- leaflet(location)
  #   map <- addTiles(map)
  #   if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile2 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng = -87.6298, lat = 41.8781, zoom = 10)
  #   if (input$compareDate2){
  #     pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #     map <- addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                             color = ~pal(location$rides), label = location$stationname,
  #                             stroke = FALSE, fillOpacity = 0.9,
  #                             popup = paste("<b>",location$stationname,"</b><br/> Rides difference: ",location$rides,
  #                                           "<br/> Rides on ",input$date2,": ",location$rides_1,
  #                                           "<br/> Rides on ",input$secondDate2,": ",location$rides_2)
  #     )
  #     map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Entries Difference' )
  #     
  #   }
  #   else {
  #     pal <-  colorQuantile("RdYlBu", domain = location$rides,reverse = TRUE, n=7)
  #     map <-  addCircleMarkers(map, lng = location$longtitude, lat = location$latitude,radius =  10,
  #                              color = ~pal(location$rides), label = location$stationname,
  #                              stroke = FALSE, fillOpacity = 0.9,
  #                              popup = paste("<b>",location$stationname,"</b><br/> Rides: ",location$rides))
  #     
  #     map <- addLegend(map,colors = rev(brewer.pal(n = 7, name = "RdYlBu")), group = "legends", position = "bottomleft", labels = levels(location$quantiles), title = 'Total Entries' )
  #   }
  #   map
  # })
  # 
  # output$leafYear2<- renderLeaflet({
  #   location <- stationNameLocationReactive2()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #   
  #   if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile2 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafMonth2<- renderLeaflet({
  #   location <- stationNameLocationReactive2()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #   
  #   if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile2 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafDay2<- renderLeaflet({
  #   location <- stationNameLocationReactive2()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #   
  #   if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile2 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  # 
  # output$leafDate2<- renderLeaflet({
  #   location <- stationNameLocationReactive2()
  #   map <- leaflet(CTA_location)
  #   map <- addTiles(map)
  #   longtitude <- CTA_location$longtitude[which(CTA_location$stationname == input$StationName2)]
  #   latitude <- CTA_location$latitude[which(CTA_location$stationname == input$StationName2)]
  #   
  #   if(input$mapTile2 == 'Contrast') {map <- addProviderTiles(map=map, provider = "Stamen.Toner")}
  #   if(input$mapTile2 == 'Geographic') {
  #     map <- addProviderTiles(map = map,provider = "Esri.WorldImagery")
  #     map <- addProviderTiles(map = map,provider = "CartoDB.VoyagerOnlyLabels")
  #   }
  #   
  #   map <- setView(map, lng =longtitude, lat = latitude, zoom = 17)
  #   map <- addMarkers(map, lng = ~longtitude, lat = ~latitude, label = ~stationname)
  #   map
  # })
  
  
}

shinyApp(ui = ui, server = server)

