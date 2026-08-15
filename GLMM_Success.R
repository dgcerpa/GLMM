
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

alldata.sc <- read.csv("data_glmm_filtered.csv", header = T)
alldata.sc <- subset(alldata.sc, select = -c(X))


###################################
### GLMM success ####

alldata.sc_a <- subset(alldata.sc, decision == 1)

## Model 1
m1 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 2
m2 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + Fatigue_diff + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 3
m3 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1 + c.reward + c.effort|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

## Model 4
m4 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))


# Summary of models
summary(m2)
car::Anova(m2, type = "II")
isSingular(m2)
performance::r2_nakagawa(m2)
# anova(m1, m2, m3, m4)



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


###################
## Contrastes post-hoc para los corchetes

p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05)  "*"  else if (p < 0.10) "."  else "ns"
}

# (1) Self vs Other dentro de cada grupo
em_agent_en_grupo <- emmeans(m2, ~ agent | grupo, type = "response",
                             at = list(c.reward = 0, c.effort = 0))

# (2) Control vs Vulnerable dentro de cada agente
em_grupo_en_agent <- emmeans(m2, ~ grupo | agent, type = "response",
                             at = list(c.reward = 0, c.effort = 0))

# Puntos por sujeto: tasa empírica de fallo por agente
puntos_fallo <- alldata.sc_a %>%
  group_by(sub, grupo, agent) %>%
  summarise(prob_indiv = 1 - mean(success), .groups = "drop") %>%
  mutate(grupo = factor(grupo),
         agent = factor(agent))

dodge_off  <- 0.6 / 2 / 2
y_top_all  <- max(c(df_success_m2$asymp.UCL, puntos_fallo$prob_indiv), na.rm = TRUE)

# Corchetes cortos (dentro del grupo): agent0 - agent1 dentro de cada grupo
sig_within <- as.data.frame(pairs(em_agent_en_grupo)) %>%
  mutate(x_left   = as.numeric(as.factor(grupo)) - dodge_off,
         x_right  = as.numeric(as.factor(grupo)) + dodge_off,
         y_bracket = y_top_all * 1.05,
         y_label   = y_top_all * 1.08,
         etiqueta  = vapply(p.value, p_a_estrellas, character(1)))

# Corchetes largos (entre grupos): grupo0 - grupo1 dentro de cada agente
sig_between <- as.data.frame(pairs(em_grupo_en_agent)) %>%
  mutate(x_left   = 1 + ifelse(agent == 0, -dodge_off, dodge_off),
         x_right  = 2 + ifelse(agent == 0, -dodge_off, dodge_off),
         y_bracket = ifelse(agent == 0, y_top_all * 1.15, y_top_all * 1.28),
         y_label   = y_bracket + y_top_all * 0.03,
         etiqueta  = vapply(p.value, p_a_estrellas, character(1)))






###################
## Gráfico probabilidad de fallo con puntos individuales por sujeto

# Puntos por sujeto: tasa empírica de fallo (mean(success)) por agente
puntos_fallo <- alldata.sc_a %>%
  group_by(sub, grupo, agent) %>%
  summarise(prob_indiv = mean(success), .groups = "drop") %>%
  mutate(grupo = factor(grupo),
         agent = factor(agent))

p1 <- ggplot(df_success_m2, aes(x = grupo, y = prob, fill = agent)) +
  geom_col(position = position_dodge(width = 0.6),
           width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, color = "black",
                position = position_dodge(width = 0.6)) +
  geom_point(data = puntos_fallo, inherit.aes = FALSE,
             aes(x = grupo, y = prob_indiv, color = agent),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
             alpha = 0.55, size = 1.8, show.legend = FALSE) +
  # Corchetes cortos: Self vs Other dentro de cada grupo
  geom_segment(data = sig_within, inherit.aes = FALSE,
               aes(x = x_left, xend = x_right, y = y_bracket, yend = y_bracket)) +
  geom_segment(data = sig_within, inherit.aes = FALSE,
               aes(x = x_left, xend = x_left,
                   y = y_bracket, yend = y_bracket - y_top_all * 0.015)) +
  geom_segment(data = sig_within, inherit.aes = FALSE,
               aes(x = x_right, xend = x_right,
                   y = y_bracket, yend = y_bracket - y_top_all * 0.015)) +
  geom_text(data = sig_within, inherit.aes = FALSE,
            aes(x = (x_left + x_right)/2, y = y_label, label = etiqueta), size = 5) +
  # Corchetes largos: Control vs Vulnerable dentro de cada agente
  geom_segment(data = sig_between, inherit.aes = FALSE,
               aes(x = x_left, xend = x_right, y = y_bracket, yend = y_bracket)) +
  geom_segment(data = sig_between, inherit.aes = FALSE,
               aes(x = x_left, xend = x_left,
                   y = y_bracket, yend = y_bracket - y_top_all * 0.015)) +
  geom_segment(data = sig_between, inherit.aes = FALSE,
               aes(x = x_right, xend = x_right,
                   y = y_bracket, yend = y_bracket - y_top_all * 0.015)) +
  geom_text(data = sig_between, inherit.aes = FALSE,
            aes(x = (x_left + x_right)/2, y = y_label, label = etiqueta), size = 5) +
  scale_x_discrete(labels = c("0" = "Control", "1" = "Vulnerable")) +
                    labels = c("0" = "Self", "1" = "Other",
                    name = "Agente") +
  scale_color_manual(values = c("0" = "#1F77B4", "1" = "#D62728"),
                     guide = "none") +
  labs(x = "Grupo", y = "Probabilidad de fallo") +
  theme_classic(base_size = 14)

print(p1)

