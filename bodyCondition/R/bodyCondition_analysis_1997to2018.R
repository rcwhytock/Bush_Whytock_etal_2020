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

# Some diagnostics of effort
table(subset_mergedClean1997to2018$event,
      subset_mergedClean1997to2018$scorerCode)
table(
  subset_mergedClean1997to2018$uniqueName,
  subset_mergedClean1997to2018$scorerCode
)

#### Regression analysis ####

# Check for multicollinearity
source(paste0(dirname(dirname(getwd(
))), "/testScoring/R/functions/pair_plot.R"))

names(subset_mergedClean1997to2018)

pairs(
  subset_mergedClean1997to2018[, c(1, 3, 5, 13)],
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
subset_mergedClean1997to2018$row <-
  1:nrow(subset_mergedClean1997to2018)

subset_mergedClean1997to2018avEvent <-
  aggregate(bodyScore ~ event, FUN = mean, data = subset_mergedClean1997to2018)
hist(subset_mergedClean1997to2018avEvent$bodyScore)
summary(subset_mergedClean1997to2018avEvent)

avEventDat <-
  merge(x = subset_mergedClean1997to2018avEvent,
        y = subset_mergedClean1997to2018[, c("yearOnlyScale", "monthOnly", "event")],
        by = "event",
        all.y = F)
avEventDat <- unique(avEventDat)
summary(avEventDat)

# Get most frequent elephant age
pivotCounts <-
  count(subset_mergedClean1997to2018, event, age) %>% pivot_wider(values_from = n, names_from = age)
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

# 1.1 GLM of average bodyScore per event
glmList <- list(

  mod1_glm <-
    glm(bodyScore ~ monthOnly, family = gaussian, data = avEventDatNewAge),
  mod2_glm <-
    glm(
      bodyScore ~ yearOnlyScale +  monthOnly,
      family = gaussian,
      data = avEventDatNewAge
    ),
  mod3_glm <-
    glm(
      bodyScore ~ age + yearOnlyScale + monthOnly + age,
      family = gaussian,
      data = avEventDatNewAge
    ),
  mod4_glm <-
    glm(
      bodyScore ~ age * yearOnlyScale + monthOnly,
      family = gaussian,
      data = avEventDatNewAge
    )
  
)

glmAICc <- AICc(glmList[[1]], glmList[[2]], glmList[[3]], glmList[[4]])
glmAICc$model <- c(
  "Month",
  "Year + Month",
  "Age + Year + Month",
  "Age + Year + Age * Year + Month"
)
glmAICc$timePeriod = "1997 to 2018"
glmAICc$dAICc <- glmAICc$AICc - glmAICc[which.min(glmAICc$AICc),"AICc"]

glmAICc <- glmAICc[order(glmAICc$dAICc), c(4,3,1,2,5)]
write.csv(glmAICc, "../results/1997to2018/AICc_glm_1997to2018.csv", row.names = FALSE)

# Residual diagnostics
topGLM <- 1

par(mfrow = c(2, 2))
plot(glmList[[topGLM]]) # OK

par(mfrow = c(1, 1))
plot(resid(glmList[[topGLM]]) ~ avEventDatNewAge$yearOnlyScale)
abline(h = 0, col = "red", lty = 2)
plot(resid(glmList[[topGLM]]) ~ avEventDatNewAge$monthOnly)
abline(h = 0, col = "red", lty = 2)
plot(resid(glmList[[topGLM]]) ~ factor(avEventDatNewAge$age))
abline(h = 0, col = "red", lty = 2)

# Results
summary(glmList[[topGLM]])

topGLM_mod <- glmList[[topGLM]]

# 2 Run as mixed effects model with random intercept of event, random intercept for scorerCode
lmerList <- list(

  mod1_lmer <-
    lmer(
      bodyScore ~ monthOnly + (1|event) + (1|scorerCode),
      REML = FALSE,
      data = subset_mergedClean1997to2018,
      control=lmerControl(optimizer="Nelder_Mead")
    ),
  mod2_lmer <-
    lmer(
      bodyScore ~ yearOnlyScale + monthOnly + (1|event) + (1|scorerCode),
      REML = FALSE,
      data = subset_mergedClean1997to2018,
      control=lmerControl(optimizer="Nelder_Mead")
    ),
  mod3_lmer <-
    lmer(
      bodyScore ~ age + yearOnlyScale + monthOnly + (1|event) + (1|scorerCode),
      REML = FALSE,
      data = subset_mergedClean1997to2018,
      control=lmerControl(optimizer="Nelder_Mead")
    ),
  mod4_lmer <-
    lmer(
      bodyScore ~ age * yearOnlyScale + monthOnly + (1|event) + (1|scorerCode),
      REML = FALSE,
      data = subset_mergedClean1997to2018,
      control=lmerControl(optimizer="Nelder_Mead")
    )
)

lmerAICc <- AICc(lmerList[[1]], lmerList[[2]], lmerList[[3]], lmerList[[4]])
lmerAICc$model <- c(
  "Month",
  "Year + Month",
  "Age + Year + Month",
  "Age + Year + Age * Year + Month"
)
lmerAICc$timePeriod = "1997 to 2018"
lmerAICc$dAICc <- lmerAICc$AICc - lmerAICc[which.min(lmerAICc$AICc),"AICc"]

lmerAICc <- lmerAICc[order(lmerAICc$dAICc), c(4,3,1,2,5)]
write.csv(lmerAICc, "../results/1997to2018/AICc_lmer_1997to2018.csv", row.names = FALSE)

# Residual diagnostics
topLmer <- 2
plot(lmerList[[topLmer]])

# code from here https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
ggplot(data.frame(
  lev = hatvalues(lmerList[[topLmer]]),
  pearson = residuals(mod3_lmer, type = "pearson")
),
aes(x = lev, y = pearson)) +
  geom_point() +
  theme_bw()

par(mfrow = c(2, 3))
plot(resid(lmerList[[topLmer]]) ~ subset_mergedClean1997to2018$yearOnlyScale)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmerList[[topLmer]]) ~ subset_mergedClean1997to2018$monthOnly)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmerList[[topLmer]]) ~ as.factor(subset_mergedClean1997to2018$age))
abline(h = 0, col = "red", lty = 2)
plot(resid(lmerList[[topLmer]]) ~ subset_mergedClean1997to2018$scorerCode)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmerList[[topLmer]]) ~ as.factor(subset_mergedClean1997to2018$event))
abline(h = 0, col = "red", lty = 2)

# Results
top_lmer_REML <- update(lmerList[[topLmer]], REML = TRUE)

# 3 Run as mixed effects model without random intercept of event
lmer1List <-
  list(

    mod1_lmer1 <-
      lmer(
        bodyScore ~ monthOnly + (1|scorerCode),
        REML = FALSE,
        data = subset_mergedClean1997to2018,
        control=lmerControl(optimizer="Nelder_Mead")
      ),
    mod2_lmer1 <-
      lmer(
        bodyScore ~ yearOnlyScale + monthOnly + (1|scorerCode),
        REML = FALSE,
        data = subset_mergedClean1997to2018,
        control=lmerControl(optimizer="Nelder_Mead")
      ),
    mod3_lmer1 <-
      lmer(
        bodyScore ~ age + yearOnlyScale + monthOnly + (1|scorerCode),
        REML = FALSE,
        data = subset_mergedClean1997to2018,
        control=lmerControl(optimizer="Nelder_Mead")
      ),
    mod4_lmer1 <-
      lmer(
        bodyScore ~ age * yearOnlyScale + monthOnly + (1|scorerCode),
        REML = FALSE,
        data = subset_mergedClean1997to2018,
        control=lmerControl(optimizer="Nelder_Mead")
      )
  )

lmer1AICc <- AICc(lmer1List[[1]], lmer1List[[2]], lmer1List[[3]], lmer1List[[4]])
lmer1AICc$model <- c(
  "Month",
  "Year + Month",
  "Age + Year + Month",
  "Age + Year + Age * Year + Month"
)
lmer1AICc$timePeriod = "1997 to 2018"
lmer1AICc$dAICc <- lmer1AICc$AICc - lmer1AICc[which.min(lmer1AICc$AICc),"AICc"]

lmer1AICc <- lmer1AICc[order(lmer1AICc$dAICc), c(4,3,1,2,5)]
write.csv(lmer1AICc, "../results/1997to2018/AICc_lmer1_1997to2018.csv", row.names = FALSE)

# Residual diagnostics
topLmer1 <- 3

plot(lmer1List[[topLmer1]])

ggplot(data.frame(
  lev = hatvalues(lmer1List[[topLmer1]]),
  pearson = residuals(mod2_lmer1, type = "pearson")
),
aes(x = lev, y = pearson)) +
  geom_point() +
  theme_bw() # OK

par(mfrow = c(2, 2))
plot(resid(lmer1List[[topLmer1]]) ~ subset_mergedClean1997to2018$yearOnlyScale)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmer1List[[topLmer1]]) ~ subset_mergedClean1997to2018$age)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmer1List[[topLmer1]]) ~ subset_mergedClean1997to2018$monthOnly)
abline(h = 0, col = "red", lty = 2)
plot(resid(lmer1List[[topLmer1]]) ~ subset_mergedClean1997to2018$scorerCode)
abline(h = 0, col = "red", lty = 2)

# Results
top_lmer1_REML <- update(lmer1List[[topLmer1]], REML = TRUE)

# Top model list
topModList1997to2018 <-
  list(topGLM_mod, top_lmer_REML, top_lmer1_REML)

# Bootstrap confidence ints for summary table
nSim <- 1000

# Top glm
annualChange_confint_top_glm <- confint(topGLM_mod,
                                        level = 0.95,
                                        method = "boot",
                                        nsim = nSim)

annualCoefs_top_glm <- data.frame(coefTable(topGLM_mod))
annualCoefs_top_glm$lowerCI <-
  as.numeric(annualChange_confint_top_glm [1:2, ][, 1])
annualCoefs_top_glm$upperCI <-
  as.numeric(annualChange_confint_top_glm [1:2, ][, 2])
write.csv(
  annualCoefs_top_glm,
  "../results/1997to2018/coefs_annualChange_top_glm_1997to2018.csv"
)

#  Top lmer
annualChange_confint_top_lmer <-
  confint.merMod(
    topModList1997to2018[[2]],
    level = 0.95,
    method = "boot",
    nsim = nSim
  )

annualCoefs_top_lmer <-
  data.frame(coefTable(topModList1997to2018[[2]]))
annualCoefs_top_lmer$lowerCI <-
  as.numeric(annualChange_confint_top_lmer [4:16, ][, 1])
annualCoefs_top_lmer$upperCI <-
  as.numeric(annualChange_confint_top_lmer [4:16, ][, 2])
write.csv(
  annualCoefs_top_lmer,
  "../results/1997to2018/coefs_annualChange_top_lmer_1997to2018.csv"
)

#  Top lmer1
annualChange_confint_top_lmer1 <-
  confint.merMod(
    topModList1997to2018[[3]],
    level = 0.95,
    method = "boot",
    nsim = nSim
  )

annualCoefs_top_lmer1 <-
  data.frame(coefTable(topModList1997to2018[[3]]))
annualCoefs_top_lmer1$lowerCI <-
  as.numeric(annualChange_confint_top_lmer1 [3:17, ][, 1])
annualCoefs_top_lmer1$upperCI <-
  as.numeric(annualChange_confint_top_lmer1 [3:17, ][, 2])
write.csv(
  annualCoefs_top_lmer1,
  "../results/1997to2018/coefs_annualChange_top_lmer1_1997to2018.csv"
)

# Save everything
save(list = ls(.GlobalEnv),
     file = "../results/1997to2018/bodyCondition_1997to2018.Rdata")

