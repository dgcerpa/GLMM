
## GLMM: Success (fallo de completacion) en trials aceptados
# Diego Garrido Cerpa - Viña del Mar 2026
# 'success' esta codificada 1 = fallo de completacion.

## Librerías
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(performance)
library(rsvg)


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
anova(m1, m2, m3, m4)
anova(m4, m3)


###################################
## Modelo reportado: m4

isSingular(m3) 
print(summary(m3)$coefficients, digits = 6)
print(car::Anova(m3, type = "II"), digits = 6)
r2_nakagawa(m3)


isSingular(m4) 
print(summary(m4)$coefficients, digits = 6)
print(car::Anova(m4, type = "II"), digits = 6)
r2_nakagawa(m4)


# Tasas de fallo empíricas (ensayos aceptados) por grupo
alldata.sc_a %>% group_by(grupo) %>%
  summarise(n_trials = n(), fail_rate = mean(success), .groups = "drop")


###################
## Figura 5: efecto principal de grupo (promediado sobre beneficiario)
###################

LAB_NONVUL <- "Non-vulnerable"; LAB_VUL <- "Vulnerable"
COL_NONVUL <- "#E76F51";        COL_VUL <- "#2A9D8F"   # misma paleta que Fig. 2 y 3

p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.10) "." else "ns"
}

# Probabilidad estimada por grupo (promedio sobre agent; effort y reward en su media)
df_grp <- as.data.frame(
  emmeans(m4, ~ grupo, type = "response", at = list(c.reward = 0, c.effort = 0)))
df_grp$Group <- factor(df_grp$grupo, levels = c(0, 1), labels = c(LAB_NONVUL, LAB_VUL))

# Tasa empírica de fallo por sujeto (todos los ensayos aceptados)
pts <- alldata.sc_a %>%
  group_by(sub, grupo) %>%
  summarise(prob_indiv = mean(success), .groups = "drop") %>%
  mutate(Group = factor(grupo, levels = c(0, 1), labels = c(LAB_NONVUL, LAB_VUL)))

# Corchete: p del efecto principal de grupo (Type II), el mismo que se reporta en el texto
p_grupo <- car::Anova(m4, type = "II")["grupo", "Pr(>Chisq)"]
y_top   <- max(pts$prob_indiv, na.rm = TRUE)
sig <- data.frame(x1 = 1, x2 = 2, y = y_top * 1.08, yl = y_top * 1.12,
                  lab = p_a_estrellas(p_grupo))

p5 <- ggplot(df_grp, aes(x = Group, y = prob, fill = Group)) +
  geom_col(width = 0.5, color = "black", alpha = 0.85) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  geom_jitter(data = pts, inherit.aes = FALSE,
              aes(x = Group, y = prob_indiv, color = Group),
              width = 0.08, alpha = 0.6, size = 1.8) +
  geom_segment(data = sig, inherit.aes = FALSE, aes(x = x1, xend = x2, y = y, yend = y)) +
  geom_segment(data = sig, inherit.aes = FALSE, aes(x = x1, xend = x1, y = y, yend = y - y_top * 0.02)) +
  geom_segment(data = sig, inherit.aes = FALSE, aes(x = x2, xend = x2, y = y, yend = y - y_top * 0.02)) +
  geom_text(data = sig, inherit.aes = FALSE, aes(x = 1.5, y = yl, label = lab), size = 6) +
  scale_fill_manual(values  = c(COL_NONVUL, COL_VUL), guide = "none") +
  scale_color_manual(values = c(COL_NONVUL, COL_VUL), guide = "none") +
  labs(x = NULL, y = "Probability of failure") +
  theme_classic(base_size = 14)

print(p5)
ggsave("figure5.svg", p5, width = 5, height = 5, dpi = 300, bg = "white")




