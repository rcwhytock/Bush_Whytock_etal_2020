# Copy events to new folder
head(mergedClean)

# Fix error in fullPath
mergedClean$fullPath <- gsub(mergedClean$fullPath, pattern = "archive_archive_", replacement = "archive_")


mergedClean$newNames <- paste0("E:/Biomonitoring_archive/elephants_bodyCondition_2019/eventsAll", "/", mergedClean$event, "_",  mergedClean$uniqueName)

# Copy unique events into a directory and check visually
copyImages <- mergedClean[,c("fullPath", "newNames")]
copyImages <- unique(copyImages)
file.copy(as.character(copyImages$fullPath), as.character(copyImages$newNames))
