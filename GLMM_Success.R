
## GLMM: Success (fallo de completacion) en trials aceptados
# Diego Garrido Cerpa - Viña del Mar 2026
# 'success' esta codificada 1 = fallo de completacion.

## Librerías
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(performance)


######################################
## Import Data
alldata.sc <- read.csv("data_glmm_filtered.csv", header = T)
alldata.sc <- subset(alldata.sc, select = -c(X))

# Solo trials aceptados
alldata.sc_a <- subset(alldata.sc, decision == 1)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


###################################
### GLMM success ####

## Model 1: 4-vias + slopes
m1 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model 2: 4-vias + intercept
m2 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model 3: mirrored (reward*agent*grupo + effort*agent*grupo) + slopes  <- reportado
m3 <- glmer(success ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model 4: mirrored + intercept
m4 <- glmer(success ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

# Comparacion de modelos
anova(m1, m2, m3, m4)                 # AIC/BIC: m3 mejor
sapply(list(m1=m1,m2=m2,m3=m3,m4=m4), isSingular)   # m1 singular; m3 no
anova(m4, m3)                         # slopes vs intercept (mirrored): chi2(5)=17.74, p=.003


###################################
## Modelo reportado: m3

summary(m3)
car::Anova(m3, type = "II")
isSingular(m3)                        # FALSE
r2_nakagawa(m3)                       # marginal .119, conditional .354


######################
## Post-hoc (descriptivo; la interaccion beneficiary x group NO es significativa)

# grupo dentro de cada beneficiario (odds ratio)
em_grupo_en_agent <- emmeans(m3, ~ grupo | agent, type = "response",
                             at = list(c.reward = 0, c.effort = 0))
pairs(em_grupo_en_agent)


###################
## Figura 5: probabilidad de fallo por grupo y beneficiario (sin corchetes)
###################

LAB_NONVUL <- "Non-vulnerable"        # etiqueta grupo 0 (antes Control)
LAB_VUL    <- "Vulnerable"

# Grilla grupo x beneficiario (covariables en su media)
df_success <- as.data.frame(
  emmeans(m3, ~ grupo * agent, type = "response",
          at = list(c.reward = 0, c.effort = 0)))
df_success$grupo <- factor(df_success$grupo)
df_success$agent <- factor(df_success$agent)

# Puntos por sujeto: tasa empirica de fallo por beneficiario
puntos_fallo <- alldata.sc_a %>%
  group_by(sub, grupo, agent) %>%
  summarise(prob_indiv = mean(success), .groups = "drop") %>%
  mutate(grupo = factor(grupo), agent = factor(agent))

p1 <- ggplot(df_success, aes(x = grupo, y = prob, fill = agent)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, color = "black", position = position_dodge(width = 0.6)) +
  geom_point(data = puntos_fallo, inherit.aes = FALSE,
             aes(x = grupo, y = prob_indiv, color = agent),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.55, size = 1.8, show.legend = FALSE) +
  scale_x_discrete(labels = c("0" = LAB_NONVUL, "1" = LAB_VUL)) +
  scale_fill_manual(values = c("0" = "#1F77B4", "1" = "#D62728"),
                    labels = c("0" = "Self", "1" = "Other"), name = "Beneficiary") +
  scale_color_manual(values = c("0" = "#1F77B4", "1" = "#D62728"), guide = "none") +
  labs(x = "Group", y = "Probability of failure") +
  theme_classic(base_size = 14)

print(p1)
ggsave("figure5.png", p1, width = 7, height = 5, dpi = 300, bg = "white")
