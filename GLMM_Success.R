

## GLMM: General Linear Mixed Models of Success on Experimental Task
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

alldata.sc <- read.csv("datos_long_glmm_filtrados.csv", header = T)
alldata.sc <- subset(alldata.sc, select = -c(X))



###################################
### GLMM success ####

alldata.sc_a <- subset(alldata.sc, decision == 1)

## Model 1
m1 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1 + c.effort + c.reward|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 2
m2 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 3
m3 <- glmer(success ~ c.reward*agent*c.effort*grupo + (1 + c.reward + c.effort|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 4
m4 <- glmer(success ~ c.reward*agent*c.effort*grupo + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

# Summary of models
summary(m2)
car::Anova(m2, type = "II")
isSingular(m2)
performance::r2_nakagawa(m2)
anova(m1, m2, m3, m4)



######################
# POST HOC modelo succes

em_grupo_en_agent <- emmeans(m2, ~ grupo | agent, type = "response", at = list(c.reward = 0, c.effort = 0))
pairs(em_grupo_en_agent)
summary(m2)


slopes_success_m2 <- emtrends(m2,~ grupo*agent, var = "c.reward")
pairs(slopes_success_m2)


# Grilla completa grupo x agente para los gráficos (covariables fijadas en su media)
df_success_m2 <- as.data.frame(
  emmeans(m2, ~ grupo * agent,
          type = "response",
          at = list(c.reward = 0, c.effort = 0))
)

# 'grupo' y 'agent' vienen como numéricos (0/1) al leer el CSV; los pasamos a factor
# para que las escalas discretas y de color de ggplot funcionen correctamente.
df_success_m2$grupo <- factor(df_success_m2$grupo)
df_success_m2$agent <- factor(df_success_m2$agent)


# 
ggplot(df_success_m2, aes(x = grupo, y = prob, fill = agent)) +
  geom_col(position = position_dodge(width = 0.6),
           width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, color = "black",
                position = position_dodge(width = 0.6)) +
  scale_x_discrete(labels = c("0" = "Control", "1" = "Vulnerable")) +
  scale_fill_manual(values = c("0" = "#1F77B4", "1" = "#D62728"),
                    labels = c("0" = "Self", "1" = "Other"),
                    name = "Agente") +
  labs(x = "Grupo",
       y = "Probabilidad de éxito") +
  theme_classic(base_size = 14)

