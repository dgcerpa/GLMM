

## GLMM: General Linear Mixed Models
# Diego Garrido Cerpa - Viña del Mar 2026


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


######################################
## Import Data

alldata.sc <- read.csv("data_glmm_filtered.csv", header = T)
alldata.sc <- subset(alldata.sc, select = -c(X))



###################################
### GLMM

## Model 1
m1 <- glmer(decision ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


## Model 2
m2 <- glmer(decision ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1|sub),
            data = alldata.sc,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))


## Model 3
m3 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


## Model 4
m4 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Summary of models

summary(m3)
car::Anova(m3, type = "II")
isSingular(m3)
performance::r2_nakagawa(m3)
anova(m1, m2, m3, m4)


# (1) Slopes por grupo, dentro de cada agent
z_grupo_en_agent <- emtrends(m3, ~ grupo | agent, var = "c.effort")
summary(z_grupo_en_agent, infer = c(TRUE, TRUE))
pairs(z_grupo_en_agent, adjust = "fdr")

# (2) Slopes por agent, dentro de cada grupo
z_agent_en_grupo <- emtrends(m3, ~ agent | grupo, var = "c.effort")
summary(z_agent_en_grupo, infer = c(TRUE, TRUE))
pairs(z_agent_en_grupo, adjust = "fdr")




###################
# Gráfico Post-Hoc

## Gráfico 3: Curvas predichas de decisión
pred_eff <- ggpredict(m3, terms = c("c.effort [all]", "agent", "grupo"))

g3 <- plot(pred_eff) +
  scale_colour_discrete(labels = lab_agent, name = "Agente") +
  scale_fill_discrete(labels   = lab_agent, name = "Agente") +
  facet_wrap(~ facet, labeller = labeller(facet = lab_grupo)) +
  labs(title = "Probabilidad predicha de aceptar vs. esfuerzo",
       subtitle = "Curvas que ilustran las pendientes estimadas en z y z2",
       x = "Esfuerzo (z-score)", y = "P(decisión = aceptar)") +
  theme_minimal(base_size = 13)

print(g3)


