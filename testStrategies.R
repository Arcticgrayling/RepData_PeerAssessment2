## try out strategies for 
## correcting Event data
## and making propdmgexp relevant
##
rm(list=ls()) 
ptm <- proc.time()
setwd("~/Documents/datasciencecoursera/ReproducibleResearch/PA2")
library(dplyr)
library(lubridate)
library(ggplot2)

Data <- readRDS(file="stormData.RDS")
dim(data)
head(data)

##subset by date - 19996
rdate <- as.POSIXct("1/1/1996", "%m/%d/%Y", tz="")
dt <- Data[which(strptime(as.character(Data$BGN_DATE), "%m/%d/%Y %H:%M:%S") > rdate),]
str(dt)
#subset on events with damage to persons or property
dsub <- subset(dt, dt$PROPDMG > 0 | dt$CROPDMG > 0 | 
                   dt$FATALITIES > 0 | dt$INJURIES > 0)

data <- dsub[ , c(8,23,24,25, 27)]

c_names <- c("EventType","Fatalities", "Injuries", "PropertyDamage", "CropDamage")
colnames(data) <- c_names

rm(dsub)
rm(dt)
rm(Data)

#get a list of unique event names
# data <- data %>% group_by(EventType)  %>%  summarise_each(funs(sum))
#data <- data.frame(unique(data$EventType))

#list of valid names from report
ValidNames <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", 
"Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil","Dust Storm","Excessive Heat", 
"Extreme Cold/Wind Chill", "Flash Flood", 
"Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", 
"High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", 
"Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", 
"Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
"Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", 
"Wildfire", "Winter Storm", "Winter Weather")

## Start making corrections to event names

## trim extra spaces
library(stringr)
datat <- str_trim(as.character(data[,1]), side = "both")

## Thunderstorm
data[,1] <- sub("tstm","thunderstorm", data[,1], ignore.case = TRUE)

##GET Captializations right
data[,1] <- tolower(as.character(data[,1]))  ## start with all lower case
## functions to Uppercase first letter
capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
## Capitalize words with / before
capwordsSlash <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = "/" )
sapply(strsplit(s, split = "/"), cap, USE.NAMES = !is.null(names(s)))
}
# run functions
data[,1] <- sapply(data[,1], capwords)
data[,1] <- sapply(data[,1], capwordsSlash)

#Make some changes to Event names with sub
data[,1] <- sub("Thunderstorm Wind.+", "Thunderstorm Wind", data[,1], ignore.case = FALSE, perl = TRUE
    )
data[,1] <- sub(".*Snow.*", "Heavy Snow", data[,1], ignore.case = FALSE, perl = TRUE
)

data[,1] <- sub(".*wind.*", "High Wind", data[,1], ignore.case = TRUE, perl = TRUE
)
data[,1] <- sub(".*(hurricane|typhoon).*", "Hurricane (Typhoon)", data[,1], ignore.case = TRUE, perl = TRUE
)

data[,1] <- sub(".*Ice Jam Flood \\(minor.*", "Flood", data[,1], ignore.case = TRUE)

# use the event update table to make more changes to the Event types to get them to match Valid Event names.
updateTable <- read.csv("EventUpdate.csv", stringsAsFactors = FALSE)


for (j in 1:nrow(data)){
       if (data[j,][[1]] %in% ValidNames)
               next
    for (i in 1:nrow(updateTable)){
        if (grepl (updateTable[i,][[1]], data[j,][[1]]) )
                data[j,][[1]] <- updateTable[i,][[2]]        
    }
}
proc.time() - ptm
# if(data[100,][[1]] %in% ValidNames)  print("yes") 
## print Event types that still don't match
x=1
for  (i in 1:nrow(data))
        if (!(as.character(data[i,][[1]]) %in% ValidNames)){
                print(as.character(data[i,]))
                x <- x + 1
        }
print(x)

