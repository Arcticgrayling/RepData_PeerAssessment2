## try out strategies for 
## correcting Event data
## and making propdmgexp relevant
##

setwd("~/Documents/datasciencecoursera/ReproducibleResearch/PA2")

data <- readRDS(file="stormData.RDS")

head(data$PROPDMGEXP)

data$PROPX[data$PROPDMGEXP %in% c("K","k") ] <- 1000
data$PROPX[data$PROPDMGEXP %in% c("M","m") ] <- 1000000
data$PROPX[data$PROPDMGEXP %in% c("B","b") ] <- 1000000000
data <- mutate(data, PropertyDamage = PROPX * PROPDMG)


## renameing Eventstype
data <- readRDS(file="orderedData.RDS")
data <- data[,c(2,8)]

#http://nicercode.github.io/blog/2013-07-09-modifying-data-with-lookup-tables/

nameIsOK <-  expectedColumns %in% names(import)

repDat <- c( Marine/Hail= "Hail",
             Large Hail = "Hail",
             High Winds="Wind" , 
             Big Winds="Wind" )
tname = "Marine/Hail"

if (tname %in% names(repDat))
  print (repDat[[tname]])
   

ValidNames <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", 
"Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil","Dust Storm","Excessive Heat", 
"Extreme Cold/Wind Chill", "Flash Flood", 
"Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", 
"High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", 
"Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", 
"Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
"Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", 
"Wildfire", "Winter Storm", "Winter Weather")

data[,1] <- tolower(as.character(data[,1]))
library(stringr)
data[,1] <- str_trim(data[,1], side = "both")

capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

capwordsSlash <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = "/" )
sapply(strsplit(s, split = "/"), cap, USE.NAMES = !is.null(names(s)))
}

data[,1] <- sub("tstm","thunderstorm", data[,1], ignore.case = TRUE)


data[,1] <- sapply(data[,1], simpleCap)
data[,1] <- sapply(data[,1], capwordsSlash)

data[,1] <- sub("Thunderstorm Wind.+", "Thunderstorm Wind", data[,1], ignore.case = FALSE, perl = TRUE
    )
data[,1] <- sub(".*Snow.*", "Heavy Snow", data[,1], ignore.case = FALSE, perl = TRUE
)

data[,1] <- sub(".*wind.*", "High Wind", data[,1], ignore.case = TRUE, perl = TRUE
)
data[,1] <- sub(".*(hurricane|typhoon).*", "Hurricane (Typhoon)", data[,1], ignore.case = TRUE, perl = TRUE
)

updateTable <- read.csv("EventUpdate.csv", stringsAsFactors = FALSE)


for (j in 1:nrow(data)){
    for (i in 1:nrow(updateTable)){
        if (grepl (updateTable[i,][[1]], data[j,][[1]]) )
                data[j,][[1]] <- updateTable[i,][[2]]        
    }
}



if(updateTable[i,][[1]] == data[j,][[1]]){
        print(data[j,][[1]])
        print(updateTable[i,][[1]])
}

#data[,1] <- simpleCap(as.character(data[,1]))
x=1
for  (i in 1:nrow(data))
         #if ("Extreme Cold/Wind Chill" %in% ValidNames)
        if (!(as.character(data[i,][[1]]) %in% ValidNames)){
                print(as.character(data[i,]))
                x <- x + 1
        }
print(x)