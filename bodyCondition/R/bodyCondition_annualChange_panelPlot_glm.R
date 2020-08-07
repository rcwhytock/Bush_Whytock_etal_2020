library(ggplot2)
library(cowplot)

# Get a list of all the .RData files
# Read all the tables into a list
RData <- list.files("../results/",
  recursive = TRUE,
  full = TRUE,
  pattern = "\\.Rdata$"
)

RData

# Create new environment for each time period
env_1997to2007 <- new.env()
load(RData[[1]], envir = env_1997to2007)

env_1997to2018 <- new.env()
load(RData[[2]], envir = env_1997to2018)

env_2008to2018 <- new.env()
load(RData[[3]], envir = env_2008to2018)

#### 1997 to 2018 ####
# Months
summary(env_1997to2018$topModList1997to2018[[1]])

glm_pred1997to2018_months <- data.frame(monthOnly = as.factor(rep(1:12)),
                                        yearOnlyScale = 0)

pred_1997to2018 <-  predict(env_1997to2018$topModList1997to2018[[1]], 
                              newdata = glm_pred1997to2018_months, se.fit = TRUE)

glm_pred1997to2018_months$fit <- pred_1997to2018$fit
glm_pred1997to2018_months$upper <- pred_1997to2018$fit + 1.96 * pred_1997to2018$se.fit
glm_pred1997to2018_months$lower <- pred_1997to2018$fit - 1.96 * pred_1997to2018$se.fit 

# Annual change
summary(env_1997to2018$topModList1997to2018[[1]]) # no year in top model, not plotted


#### 1997 to 2007 ####
# Months
summary(env_1997to2007$topModList1997to2007[[1]])

glm_pred1997to2007_months <- data.frame(monthOnly = as.factor(rep(2:11, length = 10)),
                                        yearOnlyScale = 0)

pred_1997to2007 <-  predict(env_1997to2007$topModList1997to2007[[1]], 
                            newdata = glm_pred1997to2007_months, se.fit = TRUE)

glm_pred1997to2007_months$fit <- pred_1997to2007$fit
glm_pred1997to2007_months$upper <- pred_1997to2007$fit + 1.96 * pred_1997to2007$se.fit 
glm_pred1997to2007_months$lower <- pred_1997to2007$fit - 1.96 * pred_1997to2007$se.fit 

plot(fit ~ monthOnly, glm_pred1997to2007_months, ylim = c(1,10))

# Annual change
summary(env_1997to2007$topModList1997to2007[[1]])

# Use month 9 September as closest to average
glm_pred1997to2007_years <- data.frame(monthOnly = factor(rep(9, length = 1000), levels = c(1:12)),
                                             yearOnlyScale = seq(
                                               min(env_1997to2007$avEventDatNewAge$yearOnlyScale),
                                               max(env_1997to2007$avEventDatNewAge$yearOnlyScale), 
                                               length.out = 1000))


pred_years_1997to2007 <-  predict(env_1997to2007$topModList1997to2007[[1]], 
                                        newdata = glm_pred1997to2007_years, se.fit = TRUE)

glm_pred1997to2007_years$fit <- pred_years_1997to2007$fit
glm_pred1997to2007_years$upper <- pred_years_1997to2007$fit + 1.96 * pred_years_1997to2007$se.fit
glm_pred1997to2007_years$lower <- pred_years_1997to2007$fit - 1.96 * pred_years_1997to2007$se.fit

# New year labels
newYear1997to2007 <- c(2000, 2005)
newYear1997to2007Scaled <- (newYear1997to2007 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
minYear <- (1997 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
maxYear <- (2007 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
env_1997to2007$newDatAllAge <- rbind(env_1997to2007$newDatInf,env_1997to2007$newDatAdult,env_1997to2007$newDatImm)

yearPlot_1997to2007 <- ggplot(env_1997to2007$avEventDatNewAge, aes(x = yearOnlyScale, y = bodyScore)) +
  scale_x_continuous(breaks = newYear1997to2007Scaled,
                     labels = newYear1997to2007,
                     limits = c(minYear, maxYear),
                     expand = c(0.05,0)) +
  theme_classic() +
  xlab("Year") +
  ylab("Elephant Body Condition") +
  geom_line(data = glm_pred1997to2007_years, aes(x = yearOnlyScale, y = fit), col = "coral", size = 1.5) +
  geom_line(data = glm_pred1997to2007_years, aes(x = yearOnlyScale, y = upper), col = "coral", size = 1, linetype = "dashed") +
  geom_line(data = glm_pred1997to2007_years, aes(x = yearOnlyScale, y = lower), col = "coral", size = 1, linetype = "dashed") +
  ylim(2, 8)

yearPlot_1997to2007

#### 2008 to 2018 ####
# Months
summary(env_2008to2018$topModList2008to2018[[1]])

glm_pred2008to2018_months <- data.frame(monthOnly = as.factor(rep(1:12, length = 12)),
                                        yearOnlyScale = 0)

pred_2008to2018 <-  predict(env_2008to2018$topModList2008to2018[[1]], 
                            newdata = glm_pred2008to2018_months, se.fit = TRUE)

glm_pred2008to2018_months$fit <- pred_2008to2018$fit
glm_pred2008to2018_months$upper <- pred_2008to2018$fit + 1.96 * pred_2008to2018$se.fit 
glm_pred2008to2018_months$lower <- pred_2008to2018$fit - 1.96 * pred_2008to2018$se.fit 

# Annual change
summary(env_2008to2018$topModList2008to2018[[1]])

# Use month 9 September as closest to average
glm_pred2008to2018_years <- data.frame(monthOnly = factor(rep(9, length = 1000), levels = c(1:12)),
                                             yearOnlyScale = seq(
                                               min(env_2008to2018$avEventDatNewAge$yearOnlyScale),
                                               max(env_2008to2018$avEventDatNewAge$yearOnlyScale), 
                                               length.out = 1000))

pred_years_2008to2018 <-  predict(env_2008to2018$topModList2008to2018[[1]], 
                                        newdata = glm_pred2008to2018_years, se.fit = TRUE)


glm_pred2008to2018_years$fit <- pred_years_2008to2018$fit
glm_pred2008to2018_years$upper <- pred_years_2008to2018$fit + 1.96 * pred_years_2008to2018$se.fit
glm_pred2008to2018_years$lower <- pred_years_2008to2018$fit - 1.96 * pred_years_2008to2018$se.fit

# New year labels
newYear2008to2018 <- c(2010, 2015)
newYear2008to2018Scaled <- (newYear2008to2018 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
minYear <- (2008 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
maxYear <- (2018 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
env_2008to2018$newDatAllAge <- rbind(env_2008to2018$newDatInf,env_2008to2018$newDatAdult,env_2008to2018$newDatImm)

yearPlot_2008to2018 <- ggplot(env_2008to2018$avEventDatNewAge, aes(x = yearOnlyScale, y = bodyScore)) +
  scale_x_continuous(breaks = newYear2008to2018Scaled,
                     labels = newYear2008to2018,
                     limits = c(minYear, maxYear),
                     expand = c(0.05,0)) +
  theme_classic() +
  xlab("Year") +
  ylab("Elephant Body Condition") +
  geom_line(data = glm_pred2008to2018_years, aes(x = yearOnlyScale, y = fit), col = "coral", size = 1.5) +
  geom_line(data = glm_pred2008to2018_years, aes(x = yearOnlyScale, y = upper), col = "coral", size = 1, linetype = "dashed") +
  geom_line(data = glm_pred2008to2018_years, aes(x = yearOnlyScale, y = lower), col = "coral", size = 1, linetype = "dashed") +
  ylim(2, 8)
yearPlot_2008to2018

# Output
yearPlot_1997to2007
yearPlot_2008to2018

# Save as pdf (single row)


pdf("../figures/allYears_panelPlot/annualChange_TwoPanelPlot_glm.pdf", width = 8, height = 3.5)

plot_grid(yearPlot_1997to2007 + 
            theme(legend.position = "none"),
          yearPlot_2008to2018 +
            theme(legend.position = "none"), 
          nrow = 1,
          ncol = 2,
          rel_widths = c(1,1),
          labels = "AUTO")

dev.off()

jpeg("../figures/allYears_panelPlot/annualChange_TwoPanelPlot_glm.jpg", res = 300, width = 8, height = 3.5, units = "in")

plot_grid(yearPlot_1997to2007 + 
            theme(legend.position = "none"),
          yearPlot_2008to2018 +
            theme(legend.position = "none"), 
          nrow = 1,
          ncol = 2,
          rel_widths = c(1,1),
          labels = "AUTO")

dev.off()

# Plot months
glm_pred1997to2007_months$year <- "1997 to 2007"
glm_pred2008to2018_months$year <- "2008 to 2018"

allMonths <- rbind(glm_pred1997to2007_months, glm_pred2008to2018_months)
str(allMonths)

allMonths$monthOnly <- as.integer(as.character(allMonths$monthOnly))

# Plot
pd <- position_dodge(0.5) # dodge points .5 to the left and right

monthPlot <- ggplot(allMonths, aes(x=monthOnly, y=fit, colour=year, group=year)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, colour = year, group = year), width=0, position=pd, lty = 2, lwd = 0.5) +
  scale_color_manual(values=c('coral','darkslategrey'), name = NULL)+
  geom_line(aes(x=monthOnly, y=fit, colour = year, group=year),position=pd, lty = 1, lwd =0.5) +
  geom_point(aes(x=monthOnly, y=fit, colour = year, group=year, shape = year), position=pd, size =1.5) +
  scale_shape_manual(values=c(21, 16), name = NULL)+
  xlab("Month") +
  ylab("Elephant Body Condition") +
  ylim(2,8) +
  theme_classic() + 
  scale_x_continuous(breaks = 1:12, labels = c(1:12))  +
  theme(legend.position= c(0.2,0.9))
monthPlot

pdf("../figures/allYears_panelPlot/monthPlot_glm.pdf", width = 4, height = 3.5)

monthPlot

dev.off()

jpeg("../figures/allYears_panelPlot/monthPlot_glm.jpg", width = 4, height = 3.5, res = 300, units = "in")

monthPlot

dev.off()

