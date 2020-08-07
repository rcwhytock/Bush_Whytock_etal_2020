library(psych)
library(tidyr)

# read in data
files <- list.files("../data/", pattern = "\\.csv$", full = T)

dat <- vector("list", length = length(files))

for (i in 1:length(dat)) {
  
  dat[[i]] <- read.csv(files[i], stringsAsFactors = F)
  
}

dat <- do.call("rbind", dat)
str(dat)
summary(factor(dat$initials))

# Make NAs have '0' bodyscore to allow easier comparison
# dat$bodyScore <- ifelse(is.na(dat$bodyScore), 0, dat$bodyScore)

# pivot the data
wideDatBody <-
  data.frame(pivot_wider(
    dat,
    id_cols = "uniqueName",
    names_from = "initials",
    values_from = c("bodyScore")
  ))

head(wideDatBody)
summary(wideDatBody)

# Pair plots
source("functions/pair_plot.R")

pairs(wideDatBody[complete.cases(wideDatBody), c(2:9)], panel = panel.lm,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
      font.labels = 2, lower.panel = panel.cor)

wideDatBodyComplete <- wideDatBody[complete.cases(wideDatBody), ]
cor(wideDatBodyComplete[, c(2:9)])

# look at counts
datCount <- dat
datCount[datCount$neles == 0, "neles"] <- NA
wideDatCount <-
  data.frame(
    pivot_wider(
      datCount,
      id_cols = "uniqueName",
      names_from = "initials",
      values_from = c("neles")
    )
  )

head(wideDatCount)
summary(wideDatCount)
wideDatCountComplete <- wideDatCount[complete.cases(wideDatCount), ]
cor(wideDatCountComplete[, c(2:7)])

# quality
table(dat$quality, dat$initials)

# sex
table(dat$sex, dat$initials)

# age
table(dat$age, dat$initials, dat$quality)

# angle
table(dat$angle, dat$initials)

# Intraclass correlation coefficient of bodyScore with scorers D,E,G,I,J,L (see bodyCondition_loadData_ v2019.R for explanation of why these scorers)
ICC(wideDatBody[,3:8]) # Average of fixed k = 6 raters ICC3k: ICC = 0.89, F = 9.4, df1 = 199, df2 = 995, p = < 0.001, lower = 0.87, upper = 0.91, this is a two-way mixed effects model

