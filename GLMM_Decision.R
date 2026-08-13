

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
library(patchwork)


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

## Model 3
m3.5 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
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
# anova(m1, m2, m3, m4)


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
###################################
## Figura 2: predicted probabilities of decision (Panel A) + raincloud (Panel B)

# Paleta (igual a la de figuras_python.py)
COL_SELF       <- "#8AA624"
COL_OTHER      <- "#6A4C93"
COL_CONTROL    <- "#E76F51"
COL_VULNERABLE <- "#2A9D8F"


## Panel A: curvas predichas de decisión (de m3) por grupo y agente

# Ticks del eje X: se muestran los niveles 2, 3, 4 (recuperados desde los z-scores)
z_levels <- sort(unique(alldata.sc$c.effort))[c(2, 3, 4)]

pred_grid <- as.data.frame(
  ggpredict(m3, terms = c("c.effort [n=100]", "agent", "grupo"))
)
pred_grid$Agent <- factor(as.character(pred_grid$group),
                          levels = c("0", "1"), labels = c("Self", "Other"))
pred_grid$Group <- factor(as.character(pred_grid$facet),
                          levels = c("0", "1"), labels = c("Control", "Vulnerable"))

p_A <- ggplot(pred_grid, aes(x = x, y = predicted, color = Agent, fill = Agent)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1.4) +
  facet_wrap(~ Group) +
  scale_color_manual(values = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_fill_manual(values  = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_x_continuous(breaks = z_levels, labels = c("2", "3", "4")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0.70, 1.00)) +
  labs(x = NULL, y = "P(accept work offer)",
       title = "Predicted probabilities of decision") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        plot.title = element_text(face = "bold", hjust = 0.5))



## Panel B: raincloud de diff_effort por grupo
# Usa los slopes individuales de post_hoc_v2.csv (generado por Limpieza_Datos.R)

df_ind <- read.csv("dataset_final.csv", stringsAsFactors = FALSE) %>%
  select(sub, grupo, diff_effort) %>%
  filter(!is.na(diff_effort)) %>%
  mutate(Group = factor(grupo, levels = c(0, 1),
                        labels = c("Control", "Vulnerable")))


# Test entre grupos (Welch t-test) para el corchete de significancia
p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05)  "*"  else if (p < 0.10) "."  else "ns"
}

diff_pval <- wilcox.test(diff_effort ~ Group, data = df_ind)$p.value
diff_y_top <- max(df_ind$diff_effort, na.rm = TRUE)

diff_sig <- data.frame(
  x_start   = 1,
  x_end     = 2,
  y_bracket = diff_y_top * 1.9,
  y_label   = diff_y_top * 1.9,
  y_top     = diff_y_top,
  etiqueta  = p_a_estrellas(diff_pval)
)

cat("diff_effort Control vs Vulnerable:  p =",
    format.pval(diff_pval, digits = 3),
    " (", p_a_estrellas(diff_pval), ")\n", sep = "")



p_B <- ggplot(df_ind, aes(x = Group, y = diff_effort, color = Group, fill = Group)) +
  geom_violin(alpha = 0.35, trim = FALSE, width = 0.7, linewidth = 0.4) +
  geom_boxplot(width = 0.12, alpha = 0.85, outlier.shape = NA,
               color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.06, alpha = 0.7, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_end,
                   y = y_bracket, yend = y_bracket)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_start,
                   y = y_bracket, yend = y_bracket - y_top * 0.03)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_end, xend = x_end,
                   y = y_bracket, yend = y_bracket - y_top * 0.03)) +
  geom_text(data = diff_sig, inherit.aes = FALSE,
            aes(x = (x_start + x_end) / 2, y = y_label, label = etiqueta), size = 5) +
  scale_color_manual(values = c("Control" = COL_CONTROL, "Vulnerable" = COL_VULNERABLE)) +
  scale_fill_manual(values  = c("Control" = COL_CONTROL, "Vulnerable" = COL_VULNERABLE)) +
  labs(x = NULL, y = "Effort Difference (Other \u2212 Self)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")


## Composición: A + B (ratio 1.6:1 como en la versión Python)
fig2 <- (p_A + p_B) +
  plot_layout(widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

print(fig2)
ggsave("figure2.png", fig2, width = 9.5, height = 4.2, dpi = 600, bg = "white")

