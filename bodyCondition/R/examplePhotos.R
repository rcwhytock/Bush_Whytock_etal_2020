# Load data
mergedClean <- read.csv("../data/bodyCondition_Dec2019.csv")

# Get side photos by SB rated good
mergedClean_SB <- mergedClean[mergedClean$scorerCode == "L" &
                                        mergedClean$quality == "good" &
                                        mergedClean$angle == "side" & 
                                mergedClean$age == "adult",]

summary(mergedClean_SB)

# Get rid of incomplete rows
mergedClean_SB <- mergedClean_SB[!is.na(mergedClean_SB$fullPath),]

table(mergedClean_SB$bodyScore, mergedClean_SB$source)

mergedClean_SB$newPath <- paste0("../examplePhotos/", mergedClean_SB$bodyScore, "_", mergedClean_SB$uniqueName)

# There's a mistake in the fullPath that was introduced long time ago and needs fixed
mergedClean_SB$fullPath <- gsub(mergedClean_SB$fullPath, pattern = "archive_archive", replacement = "archive")

# Keep only Laila and Ana photos
mergedClean_SB <- mergedClean_SB[mergedClean_SB$source %in% c("Bahaa-el-din, L", "Cardoso, A"),]

# Copy files to a folder for viewing
file.copy(from = as.character(mergedClean_SB$fullPath), to = mergedClean_SB$newPath)
  
  
