
## Librerías

library(tidyverse)
library(readxl)
library(lme4)
library(car)
library(ggeffects)
library(ggcorrplot)
library(corrplot)
library(emmeans)
library(performance)
library(patchwork)
library(ggplot2)
library(broom)      # tidy() / glance()


######################################################
## Import Data

alldata.sc <- read.csv("data_glmm_filtered.csv", header = T)
alldata.sc <- subset(alldata.sc, select = -c(X))


###################################
### GLMM Decisión

## Model 3
m3 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

## Model 3
m3_nf <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
              data=alldata.sc,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

summary(m3)
summary(m3_nf)
anova(m3, m3_nf)


###################################
### GLMM Success

alldata.sc_a <- subset(alldata.sc, decision == 1)


## Model 2
m2 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + Fatigue_diff + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 2
m2_nf <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

summary(m2)
summary(m2_nf)
anova(m2, m2_nf)



#########################
## Data

# Cargar datos
df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo == "1")


# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo", "diff_effort", "effort_other",
                 "MAIA_DIRt", "SASS_DIRt", "IRI_DIRt",
                 "Fatigue_diff")

df_mod <- df %>%
  dplyr::select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))




###################################
## Modelo diff_effort sin interacción de grupo

## MAIA total
m11 <- lm(diff_effort ~ MAIA_DIRt, data = df_mod)
print(summary(m11))

## MAIA total con grupo
m12 <- lm(diff_effort ~ MAIA_DIRt * grupo, data = df_mod)
print(summary(m12))


## MAIA total + Fatigue
m112 <- lm(diff_effort ~ MAIA_DIRt + Fatigue_diff, data = df_mod)
print(summary(m112))


## MAIA total con grupo + Fatigue
m122 <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m122))




#################################
## Modelos SASS

## SASS total
m21 <- lm(diff_effort ~ SASS_DIRt, data = df_mod)
print(summary(m21))


## SASS total * grupo
m22 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m22))



## SASS total + Fatigue
m212 <- lm(diff_effort ~ SASS_DIRt + Fatigue_diff, data = df_mod)
print(summary(m212))


## SASS total * grupo + Fatigue
m222 <- lm(diff_effort ~ SASS_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m222))




###################################
## Modelo IRI

## IRI total
m31 <- lm(diff_effort ~ IRI_DIRt, data = df_mod)
print(summary(m31))

## IRI total * grupo
m32 <- lm(diff_effort ~ IRI_DIRt * grupo, data = df_mod)
print(summary(m32))


## IRI total + Fatigue
m312 <- lm(diff_effort ~ IRI_DIRt + Fatigue_diff, data = df_mod)
print(summary(m312))


## IRI total * grupo + Fatigue
m322 <- lm(diff_effort ~ IRI_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m322))









###################################
## Comparación: slopes estimados (BLUP) vs empíricos — MAIA, SASS, IRI

# Slopes empíricos por sujeto y agente (OLS de decision ~ effort_level)
long <- read.csv("data_glmm_filtered.csv", stringsAsFactors = FALSE)
long$effort_level <- as.integer(as.factor(long$c.effort))

slopes_emp <- long %>%
  group_by(sub, agent) %>%
  summarise(slope = coef(lm(decision ~ effort_level))[["effort_level"]], .groups = "drop") %>%
  pivot_wider(names_from = agent, values_from = slope, names_prefix = "slope_emp_") %>%
  rename(slope_emp_self = slope_emp_0, slope_emp_other = slope_emp_1) %>%
  mutate(diff_effort_emp = slope_emp_other - slope_emp_self)

# df común: BLUP (diff_effort) + empírico (diff_effort_emp) + escalas + fatiga (mismo N)
df_cmp <- read.csv("dataset_final.csv", stringsAsFactors = FALSE) %>%
  select(sub, grupo, Fatigue_diff, diff_effort, MAIA_DIRt, SASS_DIRt, IRI_DIRt) %>%
  left_join(slopes_emp, by = "sub") %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()


## MAIA — estimado (BLUP)
m_maia_blup <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_maia_blup))
print(summary(emtrends(m_maia_blup, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ MAIA_DIRt + diff_effort, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ MAIA_DIRt + diff_effort, data = subset(df_cmp, grupo == 1)))

## MAIA — empírico
m_maia_emp <- lm(diff_effort_emp ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_maia_emp))
print(summary(emtrends(m_maia_emp, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 1)))


## SASS — estimado (BLUP)
m_sass_blup <- lm(diff_effort ~ SASS_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_sass_blup))
print(summary(emtrends(m_sass_blup, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ SASS_DIRt + diff_effort, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ SASS_DIRt + diff_effort, data = subset(df_cmp, grupo == 1)))

## SASS — empírico
m_sass_emp <- lm(diff_effort_emp ~ SASS_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_sass_emp))
print(summary(emtrends(m_sass_emp, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 1)))


## IRI — estimado (BLUP)
m_iri_blup <- lm(diff_effort ~ IRI_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_iri_blup))
print(summary(emtrends(m_iri_blup, ~ grupo, var = "IRI_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ IRI_DIRt + diff_effort, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ IRI_DIRt + diff_effort, data = subset(df_cmp, grupo == 1)))

## IRI — empírico
m_iri_emp <- lm(diff_effort_emp ~ IRI_DIRt * grupo + Fatigue_diff, data = df_cmp)
print(summary(m_iri_emp))
print(summary(emtrends(m_iri_emp, ~ grupo, var = "IRI_DIRt", at = list(grupo = c(0, 1))), infer = c(TRUE, TRUE)))
print(cor.test(~ IRI_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 0)))
print(cor.test(~ IRI_DIRt + diff_effort_emp, data = subset(df_cmp, grupo == 1)))



