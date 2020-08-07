# Load libraries
library(lubridate)
library(MuMIn)
library(boot)
library(ggplot2)
library(tidyr)
library(cowplot)
library(lme4)
library(dplyr)
library(visreg)

#### Load and prep data ####
mergedClean <- read.csv("../data/public_bodyCondition_Dec2019.csv")
str(mergedClean)
summary(mergedClean)

# Format date variables
mergedClean$saveTime <- dmy_hm(mergedClean$saveTime)
mergedClean$DateTimeOriginal <- dmy_hm(mergedClean$DateTimeOriginal)
str(mergedClean)
summary(mergedClean)

# Get rid of 11 rows with NA DateTimeOriginal
mergedClean <- mergedClean[!is.na(mergedClean$DateTimeOriginal), ]
mergedClean <- droplevels(mergedClean)
summary(mergedClean)

# Get rid of 2 rows with NA bodyScore
mergedClean <- mergedClean[!is.na(mergedClean$bodyScore), ]
mergedClean <- droplevels(mergedClean)
summary(mergedClean)

# Compare frequency plots of body condition scores for all scorers
mergedClean <-
  mergedClean[!duplicated(paste0(mergedClean$scorerCode, mergedClean$uniqueName)), ]

# par(mfrow = c(4, 4))
# for (i in 1:length(unique(mergedClean$scorerCode))) # {
#   score <-
#     mergedClean[mergedClean$scorerCode == unique(mergedClean$scorerCode)[i], "bodyScore"]
#   hist(score, main =  unique(mergedClean$scorerCode)[i])
#   
# }

# Summary of scorer effort
table(mergedClean$scorerCode)

# Remove anyone who did less than 150 images (C, F, H, K)
# Remove 'A' as they did not complete the test dataset
# Remove 'B' who failed to meet the scoring criteria of the test dataset
# Remove 'M' (author Robin Whytock) who was not blinded from the research questions and had knowledge of the dates when images were taken

mergedClean <- mergedClean[mergedClean$scorerCode %in% c(#"A",
                                                         #"B",
                                                         #"C",
                                                         "D",
                                                         "E",
                                                         #"F",
                                                         "G",
                                                         #"H",
                                                         "I",
                                                         "J",
                                                         #"K",
                                                         "L"
                                                         #"M"
                                                         ), ]

mergedClean <- droplevels(mergedClean)
table(mergedClean$scorerCode)

summary(mergedClean)

# Remove first 50 scores for each person as training 'burn in'
scorerCode <- unique(mergedClean$scorerCode)

removeBurnIn <- vector("list", length = length(scorerCode))

for (i in 1:length(scorerCode)) {
  newDat <- mergedClean[mergedClean$scorerCode == scorerCode[i],]
  newDat <- newDat[order(ymd_hms(newDat$saveTime)), ]
  removeBurnIn[[i]] <- newDat[51:nrow(newDat), ]
  
}

mergedClean <- do.call("rbind", removeBurnIn)
summary(mergedClean)

#### Get summary stats for final dataset ####
# Combine sources Schuttler, S and Whittaker, A (these are all from Whittaker, A)
#levels(mergedClean$source)[7] <- "Whittaker, A"
#levels(mergedClean$source)
mergedClean <- droplevels(mergedClean)

# Calculate n unique images per source
uniqueImages <- mergedClean[,c("uniqueName", "monthOnly", "yearOnly", "source")]
uniqueImages <- unique(uniqueImages) # 5172 scores, 2824 unique images
imageEffort <- as.data.frame.matrix(table(uniqueImages$source,
      uniqueImages$yearOnly))

# Save image effort
write.csv(imageEffort, "../data/imageEffort.csv")

# Calculate n unique images per month and year
yearMonthEffort <- as.data.frame.matrix(table(uniqueImages$monthOnly,
      uniqueImages$yearOnly))

write.csv(yearMonthEffort, "../data/yearMonthEffort.csv")

# Look at scoring effort over time

# Eyeball data
par(mfrow = c(1, 1))
hist(mergedClean$bodyScore)

# Scale year and month
mergedClean$monthOnlyScale <-
  as.numeric(scale(mergedClean$monthOnly))
mergedClean$yearOnlyScale <-
  as.numeric(scale(mergedClean$yearOnly))

# Create capture events
head(mergedClean)

# Store unique sources and remove those that don't have time information to nearest minute
uniqueSources <- unique(mergedClean$source)
uniqueSources <- droplevels(uniqueSources[-c(8,9)])

# Create empty list to store results
eventList <- vector("list", length = length(uniqueSources))

for(i in 1:length(uniqueSources)){
newDat <- mergedClean[as.character(mergedClean$source) == as.character(uniqueSources[i]),]

# Select the unique images only
newDat <- newDat[,c("uniqueName", "DateTimeOriginal", "source")]
newDat <- unique(newDat)
newDat <- newDat[order(newDat$DateTimeOriginal),]
newDat$offsetTime[2:nrow(newDat)] <-
  as.character(newDat[2:nrow(newDat) - 1, c("DateTimeOriginal")])

newDat$offsetTime <- ymd_hms(newDat$offsetTime)
newDat$timeOffset <- interval(newDat$offsetTime, ymd_hms(newDat$DateTimeOriginal))
newDat$timeOffset <- newDat$timeOffset/minutes(1)

newDat$eventSwitch <- ifelse(newDat$timeOffset > 10, 1, 0)

# Create switches to detect event
newDat$eventDetect <- 0
event <- 1

for(j in 1:(nrow(newDat)-1)){
  
  if(newDat$eventSwitch[j+1] == 1){
    
    event <- event+1
    newDat$eventDetect[j+1] <- event
    
    
  } else {
    
    newDat$eventDetect[j+1] <- event
  }
  
}
 eventList[[i]] <- newDat

}

# Create unique events for each image in data from Momont, L and White, L
newDat <- mergedClean[as.character(mergedClean$source) %in% c("White, L", "Momont, L"),]
newDat <- droplevels(newDat[,c("uniqueName", "source")])
newDat <- unique(newDat)

newDat$eventDetect <- 1:nrow(newDat) # 59
newDat <- newDat[,c("uniqueName", "eventDetect")]

# merge all the data back together
eventListDF <- do.call("rbind", eventList)
eventListDF <- eventListDF[,c("uniqueName", "eventDetect")]
eventListDF <- rbind(eventListDF, newDat)

# Merge eventDetect with mergedClean
mergedClean <- merge(eventListDF, mergedClean)
mergedClean$event <- paste0(mergedClean$source, "_", mergedClean$eventDetect)

# One image from Whytock, R has wrong date time and needs removed (noticed during later analysis)
mergedClean <- mergedClean[-which(mergedClean$source == "Whytock, R" & mergedClean$yearOnly == 2000),]

length(unique(mergedClean$event)) # 891 unique events

# Calculate n unique events per source
uniqueEvents <- mergedClean[,c("event", "monthOnly", "yearOnly", "source")]
uniqueEvents <- unique(uniqueEvents) # 5172 scores, 2824 unique images
eventEffort <- as.data.frame.matrix(table(uniqueEvents$source,
                                          uniqueEvents$yearOnly))

# Save image effort
write.csv(eventEffort, "../data/eventEffort.csv")

# Calculate n unique images per month and year
yearMonthEffortEvents <- as.data.frame.matrix(table(uniqueEvents$monthOnly,
                                              uniqueEvents$yearOnly))

write.csv(yearMonthEffortEvents, "../data/yearMonthEffortEvents.csv")

