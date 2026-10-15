
# Data cleaning and individual effort slopes


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)


###############################################################################
# Libraries

library(lme4)


###############################################################################
# Import Data

glm      <- read.csv("Datos/datos_long_glmm_full.csv", header = T)
df_final <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

## Data Cleaning: filter subjects with omitions (>25%) OTHER nor SELF
glm_filtred <- glm[glm$zeros_OTHER <= 25, ]
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <= 25, ]


###############################################################################
# Transform variables (columns of data_glmm_filtered.csv)

alldata <- data.frame(sub      = as.factor(glm_filtred$sub),
                      decision = as.factor(glm_filtred$decision),
                      grupo    = as.factor(glm_filtred$grupo),
                      agent    = as.factor(glm_filtred$agent),
                      success  = as.numeric(as.character(glm_filtred$success)),
                      c.reward = as.numeric(as.character(glm_filtred$reward)),
                      c.effort = as.numeric(as.character(glm_filtred$effort)))

## Estandarize (z-score) every column with prefix c.
numcols <- grep("^c\\.", names(alldata))
alldata.sc <- alldata
alldata.sc[, numcols] <- scale(alldata.sc[, numcols])

## Exclude omited trials
alldata.sc <- subset(alldata.sc, decision != 2)

## Add each subject's Fatigue_diff from dataset_final.csv
alldata.sc$Fatigue_diff <- df_final$Fatigue_diff[match(as.character(alldata.sc$sub), as.character(df_final$sub))]

## Save Data
write.csv(alldata.sc, "data_glmm_filtered.csv", row.names = FALSE)


###############################################################################
# Extract individual effort slopes

## Split data by agent
data.self  <- subset(alldata.sc, agent != "1")
data.other <- subset(alldata.sc, agent != "0")

## Rescale numeric variables within each subset
numcols <- grep("^c\\.", names(data.other))
data.other.sc <- data.other
data.other.sc[, numcols] <- scale(data.other.sc[, numcols])

numcols <- grep("^c\\.", names(data.self))
data.self.sc <- data.self
data.self.sc[, numcols] <- scale(data.self.sc[, numcols])

## GLMM by agent to estimate individual slopes
lmself_rs.sc <- glmer(decision ~ c.reward + c.effort + (1 + c.reward + c.effort|sub),
                      data = data.self.sc,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5)))

lmother_rs.sc <- glmer(decision ~ c.reward + c.effort + (1 + c.reward + c.effort|sub),
                       data = data.other.sc,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)))

## Extract individual coefficients by subject
indvself  <- coef(lmself_rs.sc)$sub
indvother <- coef(lmother_rs.sc)$sub


###############################################################################
# Save slopes to dataset_final.csv

## Match each subject in dataset_final.csv to its slopes
idx_self  <- match(as.character(df_final$sub), rownames(indvself))
idx_other <- match(as.character(df_final$sub), rownames(indvother))
stopifnot(!anyNA(idx_self), !anyNA(idx_other))   # every subject must have a slope

## Replace effort slopes and their difference (other - self)
df_final$effort_self  <- indvself$c.effort[idx_self]
df_final$effort_other <- indvother$c.effort[idx_other]
df_final$diff_effort  <- df_final$effort_other - df_final$effort_self

## Save final dataset
write.csv(df_final, "dataset_final.csv", row.names = FALSE)





