library(ggplot2)
library(cowplot)
library(lme4)

# Set sead for reproducibility
set.seed(827)

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

#### 1997 to 2007 ####
# Months
summary(env_1997to2007$topModList1997to2007[[2]])

# For plot
nSim <- 1000

# Most common month
table(env_1997to2007$subset_mergedClean1997to2007$monthOnly) # july

# adults 
newDatAdult <- data.frame(
  yearOnlyScale = seq(
    min(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    max(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    length = nrow(env_1997to2007$subset_mergedClean1997to2007)
  ),
  age = factor("adult", levels = levels(env_1997to2007$subset_mergedClean1997to2007$age)),
  event = factor(
    env_1997to2007$subset_mergedClean1997to2007$event[1],
    levels = levels(factor(env_1997to2007$subset_mergedClean1997to2007$event))
  ),
  scorerCode = factor(
    env_1997to2007$subset_mergedClean1997to2007$scorerCode[1],
    levels = levels(env_1997to2007$subset_mergedClean1997to2007$scorerCode)
  ),
  monthOnly = factor("7",
                     levels = levels(
                       factor(env_1997to2007$subset_mergedClean1997to2007$monthOnly)
                     ))
  
)

str(newDatAdult)
mm <- model.matrix(~ age * yearOnlyScale + monthOnly, data = newDatAdult)

predFun <- function(.) mm%*%fixef(.)

bb <- bootMer(env_1997to2007$topModList1997to2007[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
newDatAdult$LC <- bb_se[1,]
newDatAdult$UC <- bb_se[2,] 
newDatAdult$pred <- predict(env_1997to2007$topModList1997to2007[[2]], 
                            newdata = newDatAdult, 
                            re.form = ~0)

# Immature
newDatImmature <- data.frame(
  yearOnlyScale = seq(
    min(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    max(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    length = nrow(env_1997to2007$subset_mergedClean1997to2007)
  ),
  age = factor("immature", levels = levels(env_1997to2007$subset_mergedClean1997to2007$age)),
  event = factor(
    env_1997to2007$subset_mergedClean1997to2007$event[1],
    levels = levels(factor(env_1997to2007$subset_mergedClean1997to2007$event))
  ),
  scorerCode = factor(
    env_1997to2007$subset_mergedClean1997to2007$scorerCode[1],
    levels = levels(env_1997to2007$subset_mergedClean1997to2007$scorerCode)
  ),
  monthOnly = factor("7",
                     levels = levels(
                       factor(env_1997to2007$subset_mergedClean1997to2007$monthOnly)
                     ))
  
)

mm <- model.matrix(~ age * yearOnlyScale + monthOnly, data = newDatImmature)

bb <- bootMer(env_1997to2007$topModList1997to2007[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
newDatImmature$LC <- bb_se[1,]
newDatImmature$UC <- bb_se[2,] 
newDatImmature$pred <- predict(env_1997to2007$topModList1997to2007[[2]], 
                               newdata = newDatImmature, 
                               re.form = ~0)

# Infant
newDatInfant <- data.frame(
  yearOnlyScale = seq(
    min(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    max(env_1997to2007$subset_mergedClean1997to2007$yearOnlyScale),
    length = nrow(env_1997to2007$subset_mergedClean1997to2007)
  ),
  age = factor("infant", levels = levels(env_1997to2007$subset_mergedClean1997to2007$age)),
  event = factor(
    env_1997to2007$subset_mergedClean1997to2007$event[1],
    levels = levels(factor(env_1997to2007$subset_mergedClean1997to2007$event))
  ),
  scorerCode = factor(
    env_1997to2007$subset_mergedClean1997to2007$scorerCode[1],
    levels = levels(env_1997to2007$subset_mergedClean1997to2007$scorerCode)
  ),
  monthOnly = factor("7",
                     levels = levels(
                       factor(env_1997to2007$subset_mergedClean1997to2007$monthOnly)
                     ))
  
)


mm <- model.matrix(~ age * yearOnlyScale + monthOnly, data = newDatInfant)

bb <- bootMer(env_1997to2007$topModList1997to2007[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
newDatInfant$LC <- bb_se[1,]
newDatInfant$UC <- bb_se[2,] 
newDatInfant$pred <- predict(env_1997to2007$topModList1997to2007[[2]], 
                               newdata = newDatInfant, 
                               re.form = ~0)

# Remove infant years before 2001
scaled2001 <- (2001 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
newDatInfant <- newDatInfant[newDatInfant$yearOnlyScale > scaled2001,]
lmm_pred1997to2007_years <- rbind(newDatAdult, newDatImmature, newDatInfant)

# New year labels
newYear1997to2007 <- c(2000, 2005)
newYear1997to2007Scaled <- (newYear1997to2007 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
minYear <- (1997 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)
maxYear <- (2007 - mean(env_1997to2007$subset_mergedClean1997to2007$yearOnly)) / sd(env_1997to2007$subset_mergedClean1997to2007$yearOnly)

yearPlot_1997to2007 <- ggplot(env_1997to2007$avEventDatNewAge, aes(x = yearOnlyScale, y = bodyScore, group = age, col = age)) +
  scale_x_continuous(breaks = newYear1997to2007Scaled,
                     labels = newYear1997to2007,
                     limits = c(minYear, maxYear),
                     expand = c(0.05,0)) +
  theme_classic() +
  xlab("Year") +
  ylab("Elephant Body Condition") +
  geom_line(data = lmm_pred1997to2007_years, aes(x = yearOnlyScale, y = pred, group = age, col = age), size = 1.5) +
  geom_line(data = lmm_pred1997to2007_years, aes(x = yearOnlyScale, y = UC, group = age, col = age), size = 1, linetype = "dashed") +
  geom_line(data = lmm_pred1997to2007_years, aes(x = yearOnlyScale, y = LC, group = age, col = age), size = 1, linetype = "dashed") +
  scale_color_manual(values=c("coral", "cadetblue", "darkslategrey"), name = "Age", labels = c("Adult", "Immature", "Infant")) +
  ylim(2, 8)

yearPlot_1997to2007

#### Plot 2008 to 2018 ####

# Most common month
table(env_2008to2018$subset_mergedClean2008to2018$monthOnly) # October but use July to be consistent with 1997 to 2007

# adults 
newDat <- data.frame(
  yearOnlyScale = seq(
    min(env_2008to2018$subset_mergedClean2008to2018$yearOnlyScale),
    max(env_2008to2018$subset_mergedClean2008to2018$yearOnlyScale),
    length = nrow(env_2008to2018$subset_mergedClean2008to2018)
  ),
  scorerCode = factor(
    env_2008to2018$subset_mergedClean2008to2018$scorerCode[1],
    levels = levels(env_2008to2018$subset_mergedClean2008to2018$scorerCode)
  ),
  event = factor(
    env_2008to2018$subset_mergedClean2008to2018$event[1],
    levels = levels(factor(env_2008to2018$subset_mergedClean2008to2018$event))
  ),
  monthOnly = factor("7",
                     levels = levels(
                       factor(env_2008to2018$subset_mergedClean2008to2018$monthOnly)
                     ))
  
)

str(newDat)
mm <- model.matrix(~ yearOnlyScale + monthOnly, data = newDat)

bb <- bootMer(env_2008to2018$topModList2008to2018[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
newDat$LC <- bb_se[1,]
newDat$UC <- bb_se[2,] 
newDat$pred <- predict(env_2008to2018$topModList2008to2018[[2]], 
                            newdata = newDat, 
                            re.form = ~0)

lmm_pred2008to2018_years <- newDat

# New year labels
newYear2008to2018 <- c(2010, 2015)
newYear2008to2018Scaled <- (newYear2008to2018 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
minYear <- (2008 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
maxYear <- (2018 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)

yearPlot_2008to2018 <- ggplot(env_2008to2018$avEventDatNewAge, aes(x = yearOnlyScale, y = bodyScore)) +
  scale_x_continuous(breaks = newYear2008to2018Scaled,
                     labels = newYear2008to2018,
                     limits = c(minYear, maxYear),
                     expand = c(0.05,0)) +
  theme_classic() +
  xlab("Year") +
  ylab("Elephant Body Condition") +
  geom_line(data = lmm_pred2008to2018_years, aes(x = yearOnlyScale, y = pred), col = "darkslategrey", size = 1.5) +
  geom_line(data = lmm_pred2008to2018_years, aes(x = yearOnlyScale, y = UC), col = "darkslategrey", size = 1, linetype = "dashed") +
  geom_line(data = lmm_pred2008to2018_years, aes(x = yearOnlyScale, y = LC), col = "darkslategrey", size = 1, linetype = "dashed") +
  ylim(2, 8)

yearPlot_2008to2018

# Output
yearPlot_1997to2007
yearPlot_2008to2018

legend <- get_legend(yearPlot_1997to2007 + 
                       theme(legend.box.margin = margin(0, 0, 0, 5), 
                             legend.justification = "top"))

#### Save as pdf (single row) ####
mainRow <- plot_grid(yearPlot_1997to2007 + 
                       theme(legend.position = "none"),
                     yearPlot_2008to2018 +
                       theme(legend.position = "none"), 
                     nrow = 1,
                     ncol = 2,
                     rel_widths = c(1,1),
                     labels = "AUTO")

pdf("../figures/allYears_panelPlot/annualChange_TwoPanelPlot_lmm.pdf", width = 8, height = 3.5)

plot_grid(
  mainRow, legend, rel_widths = c(3.5, .6)
)

dev.off()

jpeg("../figures/allYears_panelPlot/annualChange_TwoPanelPlot_lmm.jpg", res = 300, width = 8, height = 3.5, units = "in")

plot_grid(
  mainRow, legend, rel_widths = c(3.5, .6)
)

dev.off()

# Plot months
# 1997 to 2007
lmm_pred1997to2007_months <- data.frame(monthOnly = as.factor(2:11),
                                        yearOnlyScale = 0,
                                        age = factor("adult", levels = c("adult", "immature", "infant")),
                                        event = factor(
                                          env_1997to2007$subset_mergedClean1997to2007$event[1],
                                          levels = levels(factor(env_1997to2007$subset_mergedClean1997to2007$event))
                                        ),
                                        scorerCode = factor(
                                          env_1997to2007$subset_mergedClean1997to2007$scorerCode[1],
                                          levels = levels(env_1997to2007$subset_mergedClean1997to2007$scorerCode)
                                        ))
mm <- model.matrix(~ age * yearOnlyScale + monthOnly, data = lmm_pred1997to2007_months)

bb <- bootMer(env_1997to2007$topModList1997to2007[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
lmm_pred1997to2007_months$LC <- bb_se[1,]
lmm_pred1997to2007_months$UC <- bb_se[2,] 
lmm_pred1997to2007_months$pred <- predict(env_1997to2007$topModList1997to2007[[2]], 
                            newdata = lmm_pred1997to2007_months, 
                            re.form = ~0)

# 2008 to 2018
lmm_pred2008to2018_months <- data.frame(monthOnly = as.factor(1:12),
                                        yearOnlyScale = 0,
                                        event = factor(
                                          env_2008to2018$subset_mergedClean2008to2018$event[1],
                                          levels = levels(factor(env_2008to2018$subset_mergedClean2008to2018$event))
                                        ),
                                        scorerCode = factor(
                                          env_2008to2018$subset_mergedClean2008to2018$scorerCode[1],
                                          levels = levels(env_2008to2018$subset_mergedClean2008to2018$scorerCode)
                                        ))
mm <- model.matrix(~ yearOnlyScale + monthOnly, data = lmm_pred2008to2018_months)

bb <- bootMer(env_2008to2018$topModList2008to2018[[2]], FUN = predFun, nsim = nSim)

bb_se <- apply(bb$t, 2, function(x) x[order(x)][c(25,975)])
lmm_pred2008to2018_months$LC <- bb_se[1,]
lmm_pred2008to2018_months$UC <- bb_se[2,] 
lmm_pred2008to2018_months$pred <- predict(env_2008to2018$topModList2008to2018[[2]], 
                                          newdata = lmm_pred2008to2018_months, 
                                          re.form = ~0)



lmm_pred1997to2007_months$year <- "1997 to 2007"
lmm_pred2008to2018_months$year <- "2008 to 2018"

allMonths <- rbind(lmm_pred1997to2007_months[,-3], lmm_pred2008to2018_months)
str(allMonths)

allMonths$monthOnly <- as.integer(as.character(allMonths$monthOnly))

# Plot
pd <- position_dodge(0.5) # dodge points .5 to the left and right

monthPlot <- ggplot(allMonths, aes(x=monthOnly, y=pred, colour=year, group=year)) + 
  geom_errorbar(aes(ymin=LC, ymax=UC, colour = year, group = year), width=0, position=pd, lty = 2, lwd = 0.5) +
  scale_color_manual(values=c('coral','darkslategrey'), name = NULL)+
  geom_line(aes(x=monthOnly, y=pred, colour = year, group=year),position=pd, lty = 1, lwd =0.5) +
  geom_point(aes(x=monthOnly, y=pred, colour = year, group=year, shape = year), position=pd, size =1.5) +
  scale_shape_manual(values=c(21, 16), name = NULL)+
  xlab("Month") +
  ylab("Elephant Body Condition") +
  ylim(2,8) +
  theme_classic() + 
  scale_x_continuous(breaks = 1:12, labels = c(1:12))  +
  theme(legend.position= c(0.2,0.9))
monthPlot

pdf("../figures/allYears_panelPlot/monthPlot_lmm.pdf", width = 4, height = 3.5)

monthPlot

dev.off()

jpeg("../figures/allYears_panelPlot/monthPlot_lmm.jpg", width = 4, height = 3.5, res = 300, units = "in")

monthPlot

dev.off()

#### Plot 2008 to 2018 annual change alongside month change ####
#### Save as pdf (single row) ####

pdf("../figures/allYears_panelPlot/annualChange_month_TwoPanelPlot_lmm.pdf", width = 8, height = 3.5)
plot_grid(
  yearPlot_2008to2018,
  monthPlot,
  nrow = 1,
  ncol = 2,
  rel_widths = c(1, 1),
  labels = "AUTO"
)
dev.off()

# Calculate percentage change in body condtion for 2008 to 2018
((lmm_pred2008to2018_years$pred[nrow(lmm_pred2008to2018_years)] - lmm_pred2008to2018_years$pred[1]) / lmm_pred2008to2018_years$pred[1]) * 100 # -11.07% average change

# Manually calculate worst case scenario using lower and upper slope CI from top model
summary(env_2008to2018$topModList2008to2018[[2]])
intercept2008to2018 <- coefTable(env_2008to2018$topModList2008to2018[[2]])[1,1]
yearSlopeWorst2008to2018 <- -0.27 # lower 95% confint of slope (see Table S3)
yearSlopeBest2008to2018 <- -0.07 # upper 95% confint of slope (see Table S3)
monthOnly7_2008to2018 <- -0.23 # July 

# Scale first and last year
scaledYear2008 <- (2008 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)
scaledYear2018 <- (2018 - mean(env_2008to2018$subset_mergedClean2008to2018$yearOnly)) / sd(env_2008to2018$subset_mergedClean2008to2018$yearOnly)

# Calculate worst change
worst2008 <- intercept2008to2018 + scaledYear2008 * yearSlopeWorst2008to2018 - monthOnly7_2008to2018
worst2018 <- intercept2008to2018 + scaledYear2018 * yearSlopeWorst2008to2018 - monthOnly7_2008to2018

((worst2018 - worst2008) / worst2008) * 100 # -15.59% worst change 2008 to 2018

# Calculate Best change
Best2008 <- intercept2008to2018 + scaledYear2008 * yearSlopeBest2008to2018 - monthOnly7_2008to2018
Best2018 <- intercept2008to2018 + scaledYear2018 * yearSlopeBest2008to2018 - monthOnly7_2008to2018

((Best2018 - Best2008) / Best2008) * 100 # -4.30% best change 2008 to 2018

# Manually calculate worst case scenario for 1997 to 2018 using lower and upper slope CI from top model
summary(env_1997to2018$topModList1997to2018[[2]])
intercept1997to2018 <- coefTable(env_1997to2018$topModList1997to2018[[2]])[1,1]
yearSlopeMean1997to2018 <- -0.06 # estimate of slope (see Table S3)
yearSlopeWorst1997to2018 <- -0.14 # lower 95% confint of slope (see Table S3)
yearSlopeBest1997to2018 <- 0.01 # upper 95% confint of slope (see Table S3)
monthOnly7_1997to2018 <- -0.26 # July 

# Scale first and last year
scaledYear1997 <- (1997 - mean(env_1997to2018$subset_mergedClean1997to2018$yearOnly)) / sd(env_1997to2018$subset_mergedClean1997to2018$yearOnly)
scaledYear2018 <- (2018 - mean(env_1997to2018$subset_mergedClean1997to2018$yearOnly)) / sd(env_1997to2018$subset_mergedClean1997to2018$yearOnly)

# Calculate worst change
worst1997 <- intercept1997to2018 + scaledYear1997 * yearSlopeWorst1997to2018 - monthOnly7_1997to2018
worst2018_from1997to2018 <- intercept1997to2018 + scaledYear2018 * yearSlopeWorst1997to2018 - monthOnly7_1997to2018

((worst2018_from1997to2018 - worst1997) / worst1997) * 100 # -11.20% worst change 1997 to 2018

# Calculate Best change
Best1997 <- intercept1997to2018 + scaledYear1997 * yearSlopeBest1997to2018 - monthOnly7_1997to2018
Best2018_from1997to2018 <- intercept1997to2018 + scaledYear2018 * yearSlopeBest1997to2018 - monthOnly7_1997to2018

((Best2018_from1997to2018 - Best1997) / Best1997) * 100 # + 0.86% best change 1997 to 2018

# calculate mean change
Mean1997 <- intercept1997to2018 + scaledYear1997 * yearSlopeMean1997to2018 - monthOnly7_1997to2018
Mean2018_from1997to2018 <- intercept1997to2018 + scaledYear2018 * yearSlopeMean1997to2018 - monthOnly7_1997to2018

((Mean2018_from1997to2018 - Mean1997) / Mean1997) * 100 # - 4.99% Mean change 1997 to 2018

# save predictions
write.csv(allMonths, "../results/allMonths_pred_lmm.csv")
write.csv(lmm_pred2008to2018_years, "../results/years_2008to2018_pred_lmm.csv")
write.csv(lmm_pred1997to2007_years, "../results/years_1997to2007_pred_lmm.csv")

# Save everything
save(list = ls(.GlobalEnv),
     file = "../results/lmm_plotDat.Rdata")
