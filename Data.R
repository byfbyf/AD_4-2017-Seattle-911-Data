#Code for filtering the data of year 2017:(commented out)
#library(dplyr)
#
#data <- read.csv("Seattle_Police_Department_911_Incident_Response.csv", stringsAsFactors = FALSE)
#library(lubridate)

#data$Event.Clearance.Date <-as_datetime(data$Event.Clearance.Date,format="%m/%d/%Y %I:%M:%S %p", tz = "")
#data$Event.Clearance.Date <-as.POSIXct(data$Event.Clearance.Date)
#data <- filter(data, Event.Clearance.Date >= as.Date("2017-01-01") &Event.Clearance.Date < as.Date("2018-01-01"))
#write.csv(data, "2017_Seattle_911.csv")
data.2017 <-read.csv("2017_Seattle_911.csv")
