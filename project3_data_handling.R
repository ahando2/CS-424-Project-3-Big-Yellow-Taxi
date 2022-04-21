setwd("C:/Users/roche/Classes/CS 424/Project 3 Big Yellow Taxi")
dir <- getwd()
library(lubridate)
library(R.utils)
library(readr)
Taxi_Trips_2019 <- read_delim("Taxi_Trips_-_2019.tsv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

Taxi_Trips_2019_cut <- Taxi_Trips_2019[,c(3,5,6,9,10,17)]
Taxi_Trips_2019_cut <- na.omit(Taxi_Trips_2019_cut)

new_Taxi_Trips_2019 <- subset(Taxi_Trips_2019_cut, `Trip Seconds`>=60 & `Trip Seconds`<=1800 & `Trip Miles`>=0.5 & `Trip Miles`<=100)

unique(new_Taxi_Trips_2019$Company)


new_Taxi_Trips_2019$`Trip Start Timestamp` <- parse_date_time(new_Taxi_Trips_2019$`Trip Start Timestamp`,
                                                            '%m/%d/%Y %I:%M:%S %p', exact = TRUE)
new_Taxi_Trips_2019$`StartDate`<-date(new_Taxi_Trips_2019$`Trip Start Timestamp`)
new_Taxi_Trips_2019$`StartHour`<-hour(new_Taxi_Trips_2019$`Trip Start Timestamp`)
     


companyList <- sort(unique(new_Taxi_Trips_2019$Company)) 

company <- data.frame(companyName = companyList, id=c(1:length(companyList)))

new_Taxi_Trips_2019 <- merge(new_Taxi_Trips_2019, company, by.x="Company", by.y="companyName")

new_Taxi_Trips_2019$Company <- new_Taxi_Trips_2019$id

new_Taxi_Trips_2019 <- subset(new_Taxi_Trips_2019,select= -c(`id`))


companyList[1:23] <- c("N and W Cab Co","Omar Jada","Sbeih company",
                       "24 Seven Taxi", "Benny Jona", "JBL Cab Inc.",
                       "G.L.B. Cab Co", "312 Medallion Management Corp", "RC Andrews Cab",                 
                       "Chuks Cab", "David K. Cab Corp.", "Arrington Enterprises",          
                       "Santamaria Express, Alvaro Santamaria", "Adwar H. Nikola", "Jay Kim",                        
                       "5 Star Taxi", "Salifu Bawa", "Sam Mestas",                     
                       "Ahzmi Inc", "Sergey Cab Corp.", "Babylon Express Inc.",                 
                       "Tasha ride inc", "Luhak Corp")
company$companyName <- companyList

bin<-ceiling(nrow(new_Taxi_Trips_2019)/4)
start <- 1
end <- bin
new_Taxi_Trips_2019_1 <- new_Taxi_Trips_2019[c(start:end),]

start <- end+1
end <- bin*2
new_Taxi_Trips_2019_2 <- new_Taxi_Trips_2019[c(start:end),]

start <- end+1
end <- bin*3
new_Taxi_Trips_2019_3 <- new_Taxi_Trips_2019[c(start:end),]

start <- end+1
new_Taxi_Trips_2019_4 <- new_Taxi_Trips_2019[c(start:nrow(new_Taxi_Trips_2019)),]

write_tsv(
  new_Taxi_Trips_2019_1, file.path(paste(dir,"/BigYellowTaxi/",sep=""), "Taxi_Trips_2019_Clean_1.tsv")
)

write_tsv(
  new_Taxi_Trips_2019_2, file.path(paste(dir,"/BigYellowTaxi/",sep=""), "Taxi_Trips_2019_Clean_2.tsv")
)

write_tsv(
  new_Taxi_Trips_2019_3, file.path(paste(dir,"/BigYellowTaxi/",sep=""), "Taxi_Trips_2019_Clean_3.tsv")
)

write_tsv(
  new_Taxi_Trips_2019_4, file.path(paste(dir,"/BigYellowTaxi/",sep=""), "Taxi_Trips_2019_Clean_4.tsv")
)

write.table(
  company, file=file.path(paste(dir,"/BigYellowTaxi/companyList.tsv",sep="")), quote=FALSE, sep='\t', row.names = FALSE
)




CommAreas <- read_delim("CommAreas.tsv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)


CommAreas$community <- capitalize(tolower(CommAreas$`COMMUNITY`))
CommAreas$area_id <- CommAreas$`AREA_NUMBE`

CommAreas <- CommAreas[,c("community","area_id")]

write.table(
  CommAreas, file=file.path(paste(dir,"/BigYellowTaxi/CommAreas.tsv",sep="")), quote=FALSE, sep='\t', row.names = FALSE
)

