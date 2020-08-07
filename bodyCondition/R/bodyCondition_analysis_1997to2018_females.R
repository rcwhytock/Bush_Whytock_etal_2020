# Load data
source("bodyCondition_loadData_ v.2019.R")

# Years 1997to2018
mergedClean1997to2018 <-
  mergedClean[mergedClean$yearOnly > 1996,]

# Select the subset of variables to be used in analysis
subset_mergedClean1997to2018 <-
  mergedClean1997to2018[, c(
    "uniqueName",
    "bodyScore",
    "yearOnly",
    "monthOnly",
    "DateTimeOriginal",
    "source",
    "scorerCode",
    "sex",
    "quality",
    "angle",
    "age",
    "source",
    "nEles",
    "event"
  )]

str(subset_mergedClean1997to2018)

subset_mergedClean1997to2018 <-
  subset_mergedClean1997to2018[complete.cases(subset_mergedClean1997to2018),]

head(subset_mergedClean1997to2018)

# Centre and scale continuous variables
subset_mergedClean1997to2018$yearOnlyScale <-
  as.numeric(scale(subset_mergedClean1997to2018$yearOnly))
subset_mergedClean1997to2018$monthOnlyScale <-
  as.numeric(scale(subset_mergedClean1997to2018$monthOnly))

# Get only scaled variables
subset_mergedClean1997to2018 <- subset_mergedClean1997to2018[, c(
  "bodyScore",
  "uniqueName",
  "yearOnlyScale",
  "monthOnly",
  "monthOnlyScale",
  "yearOnly",
  "scorerCode",
  "sex",
  "age",
  "angle",
  "quality",
  "source",
  "nEles",
  "event"
)]

subset_mergedClean1997to2018 <-
  subset_mergedClean1997to2018[complete.cases(subset_mergedClean1997to2018), ]
subset_mergedClean1997to2018 <-
  droplevels(subset_mergedClean1997to2018)
subset_mergedClean1997to2018$monthOnly <-
  as.factor(subset_mergedClean1997to2018$monthOnly)
summary(subset_mergedClean1997to2018)

#### REVISION 1 #### Keep only females
subset_mergedClean1997to2018Females <- subset_mergedClean1997to2018[subset_mergedClean1997to2018$sex == "female",]

# Some diagnostics of effort
table(subset_mergedClean1997to2018Females$event,
      subset_mergedClean1997to2018Females$scorerCode)
table(
  subset_mergedClean1997to2018Females$uniqueName,
  subset_mergedClean1997to2018Females$scorerCode
)
table(subset_mergedClean1997to2018Females$yearOnly)
table(subset_mergedClean1997to2018Females$monthOnly)
table(subset_mergedClean1997to2018Females$monthOnly,
      subset_mergedClean1997to2018Females$yearOnly)
table(subset_mergedClean1997to2018Females$yearOnly, subset_mergedClean1997to2018Females$monthOnly)

#### Regression analysis ####

# Check for multicollinearity
source(paste0(dirname(dirname(getwd(
))), "/testScoring/R/functions/pair_plot.R"))

names(subset_mergedClean1997to2018Females)

pairs(
  subset_mergedClean1997to2018Females[, c(1, 3, 5, 13)],
  panel = panel.lm,
  cex = 1,
  pch = 19,
  col = adjustcolor(4, .4),
  cex.labels = 1.5,
  font.labels = 4,
  lower.panel = panel.cor
)

#### Temporal change in body condition ####
# Unique 'events' (i.e. series of images that are close together in time, mostly of same animal but not always) were created to deal with pseudoreplication, but still some question if correct to include these as a random effect in a LMM. Try multiple approachs to cross-validate results.

# 1. Average bodyScore for each 'event', ignoring scorer effects, simple GLM
subset_mergedClean1997to2018Females$row <-
  1:nrow(subset_mergedClean1997to2018Females)

subset_mergedClean1997to2018FemalesavEvent <-
  aggregate(bodyScore ~ event, FUN = mean, data = subset_mergedClean1997to2018Females)
hist(subset_mergedClean1997to2018FemalesavEvent$bodyScore)
summary(subset_mergedClean1997to2018FemalesavEvent)

avEventDat <-
  merge(x = subset_mergedClean1997to2018FemalesavEvent,
        y = subset_mergedClean1997to2018Females[, c("yearOnlyScale", "monthOnly", "event")],
        by = "event",
        all.y = F)
avEventDat <- unique(avEventDat)
summary(avEventDat)

# Get most frequent elephant age
pivotCounts <-
  count(subset_mergedClean1997to2018Females, event, age) %>% pivot_wider(values_from = n, names_from = age)
pivotCounts$newAge <-
  as.numeric(apply(pivotCounts[, c(2:4)], 1, function(x)
    which.max(x)))
data.frame(pivotCounts[1:100, ]) # Choose most common age class

avEventDatNewAge <-
  merge(x = avEventDat,
        y = pivotCounts[, c(1, 5)],
        by = "event",
        all.y = F)
avEventDatNewAge$age <-
  ifelse(avEventDatNewAge$newAge == 1, "adult", avEventDatNewAge$newAge)
avEventDatNewAge$age <-
  ifelse(avEventDatNewAge$newAge == 2, "immature", avEventDatNewAge$age)
avEventDatNewAge$age <-
  ifelse(avEventDatNewAge$newAge == 3, "infant", avEventDatNewAge$age)
head(avEventDatNewAge)
summary(factor(avEventDatNewAge$age))

# Not analysed further due to insufficient data
