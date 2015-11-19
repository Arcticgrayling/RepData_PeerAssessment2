#reproducible Research PA 2
# some initial hacking a data exploration
#

# clear out global environment, 
rm(list=ls())  

## Loading and preprocessing the data

```{r, echo = TRUE, results = "hide", warnings = FALSE, message= FALSE, errors=FALSE}
setwd("~/Documents/datasciencecoursera/ReproducibleResearch/PA2")
d <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
d <- readRDS(file="stormData.RDS")
#saveRDS(d, file="stormData.RDS")
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
```

head(d)
str(d)
dim(d)

#list of column names
names <- colnames(d)


##CLEAN DATA
##limit dates:  date > jan 1996

rdate <- as.POSIXct("1/1/1996", "%m/%d/%Y", tz="")
dt <- d[which(strptime(as.character(d$BGN_DATE), "%m/%d/%Y %H:%M:%S") > rdate),]
 
dt$PROPX[dt$PROPDMGEXP %in% c("K","k") ] <- 1000
dt$PROPX[dt$PROPDMGEXP %in% c("M","m") ] <- 1000000
dt$PROPX[dt$PROPDMGEXP %in% c("B","b") ] <- 1000000000
dt <- mutate(dt, PropertyDamage = PROPX * PROPDMG)        
        
## for exploratory purposed limit columns to most useful at moment
d1 <- dt[ , c(2,7,8,23,24)]

c_names <- c("Date","State", "EventType","Fatalities", "Injuries")
colnames(d1) <- c_names

## create a new column summing Fatalities and Injuries calling it Casulaties
d1 <- mutate(d1, Casualties = Fatalities + Injuries)

#look at fatalities
d$FATALITIES
#total fatalities
sum(d$FATALITIES)
# number of incidents with Fatalities
sum(d$FATALITIES > 0)

median(d$FATALITIES)

mean(d$FATALITIES[d$FATALITIES > 0])
    
#looak at injuries
#total injuries
sum(d$INJURIES)
# number of incidents with injuries
sum(d$INJURIES > 0)

mean(d$INJURIES > 0)

#sna <- sum(is.na(d$FATALITIES))
#mna <- mean(is.na(d$FATALITIES))

#Events
unique(d$EVTYPE)


## combined Casualties

eventsCasualties <- d1[d1$Casualties > 0,]
#unique(eventsCasualtiesl$EventType)
eventsCasualties[order(eventsCasualties$Casualties, decreasing=TRUE)[1:100],]

## group by event types , sum casualties, show which events have the most Casualties

g <- aggregate(Casualties ~ EventType, data = eventsCasualties, FUN = "sum")

g[order(g$Casualties, decreasing = TRUE)[1:100],]


##look at property damage
unique(d$PROPDMGEXP)
sum(d$PROPDMGEXP == "M")

sum(d$PROPDMGEXP == "B")

sum(d$PROPDMGEXP == "k")

sum(d$PROPDMGEXP == "K")

sum(d$PROPDMGEXP == "m")


propertyData <- dt[ , c(2,7,8,39,27,28)]

c_namesProperty <- c("Date","State", "EventType","PropertyDamage", "CropDamage", "CropExpo")
colnames(propertyData) <- c_namesProperty

####NEED TO MULTIPLY BY EXPO
propertyData <- mutate(propertyData, TotalPropertyDamage = PropertyDamage + CropDamage)
propertyData <- propertyData[propertyData$TotalPropertyDamage > 0,]

propertyData <- aggregate(TotalPropertyDamage ~ EventType, data = propertyData, FUN = "sum")

orderedData <- propertyData[order(propertyData$TotalPropertyDamage, decreasing = TRUE)[1:156],]

# Eventes types with fatalities
#eventsFatal <- d1[d1$Fatalities > 0,]
#unique(eventsFatal$EventType)

#eventsFatal[order(eventsFatal$Fatalities, decreasing=TRUE)[1:50],]
# Event types with Injuries
#eventsInjuries <- d1[d1$Injuries > 0,]
#unique(eventsInjuries$EventType)
#eventsInjuries[order(eventsInjuries$Injuries, decreasing=TRUE)[1:50],]