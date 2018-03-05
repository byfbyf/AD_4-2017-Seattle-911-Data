
library(dplyr)
library(tools)
library(lubridate)
library(maps)
library(shiny)
data.2017 <-read.csv("./2017_Seattle_911.csv", stringsAsFactors = FALSE)
# data.2017 <- select(data.2017, CAD.CDW.ID, Event.Clearance.Description, 
#                     Event.Clearance.SubGroup, District.Sector, Longitude, 
#                     Latitude, At.Scene.Time)
# write.csv(data.2017, "2017_Seattle_911.csv")
ChangeCase <- function(x) {
  paste0(substring(x,1,1), tolower(substring(x, 2)))
}
data.2017$Event.Clearance.Date <-as_datetime(data.2017$Event.Clearance.Date)


data.2017$Event.Clearance.Date <-as.POSIXct(data.2017$Event.Clearance.Date)
data.2017$Event.Clearance.Group <- sapply(data.2017$Event.Clearance.Group, ChangeCase)

month.start <- c(paste0("2017-", 1:12, "-01"), "2018-01-01")

by.types <- split(data.2017, data.2017$Event.Clearance.Group)
by.types <- by.types[order(sapply(by.types,nrow),decreasing=T)]
