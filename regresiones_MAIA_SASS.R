

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(car)        # vif()
library(broom)      # tidy() / glance()
library(performance) # check_model() opcional
library(ggplot2)
library(ggeffects)
library(emmeans)



#########################
## Data

# Cargar datos
df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo == "1")


# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "MAIA_DIRt", "SASS_DIRt",
                 "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
                 "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
                 "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd", 
                 "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd", "Fatigue_diff")

df_mod <- df %>%
  dplyr::select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))




###################################
## Modelo diff_effort sin interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd + MAIA_AusenciaDistraccion_DIRd +
           MAIA_AusenciaPreocupacion_DIRd + MAIA_RegulacionAtencion_DIRd +
           MAIA_ConcienciaEmocional_DIRd + MAIA_Autorregulacion_DIRd +
           MAIA_EscuchaCuerpo_DIRd + MAIA_Confianza_DIRd + Fatigue_diff,
         data = df_mod)
print(summary(m2))



###################################
## Modelo diff_effort con interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m1))

trends_maia <- emtrends(m1, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1))) # Post-hoc: slopes de MAIA por grupo
summary(trends_maia, infer = c(TRUE, TRUE)) # Slopes por grupo con IC 95% y test contra 0
pairs(trends_maia) # Contraste entre grupos (equivale al término de interacción de m1)
car::Anova(m1, type = "II")


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)
print(summary(m2))


## interaccion de 5 sub escalas (agrupar sub escalas)



#################################
## Modelos SASS

## Modelo 3: SASS total (diff_effort) sin interacción de grupo
m3 <- lm(diff_effort ~ SASS_DIRt, data = df_mod)
print(summary(m3))


## Modelo 4: SASS total (diff_effort) con interacción de grupo
m4 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m4))


trends_sass <- emtrends(m4, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1))) # Post-hoc: slopes de SASS por grupo
summary(trends_sass, infer = c(TRUE, TRUE)) # Slopes por grupo con IC 95% y test contra 0
pairs(trends_sass) # Contraste entre grupos (equivale al término de interacción)
Anova(m4, type = "II") # Efectos principales correctos (SS tipo II ignoran las interacciones al testear los términos de menor orden)





####################################
### MAIA

# ---- Robustez model-free: correlaciones por grupo + Fisher r-to-z ----
d_maia <- df_mod %>%
  dplyr::select(grupo, MAIA_DIRt, diff_effort) %>%
  tidyr::drop_na(MAIA_DIRt, diff_effort)

g_vul  <- dplyr::filter(d_maia, grupo == 1)   # vulnerable
g_nvul <- dplyr::filter(d_maia, grupo == 0)   # non-vulnerable

# Pearson + Spearman por grupo
r_vul_p  <- cor.test(g_vul$MAIA_DIRt,  g_vul$diff_effort,  method = "pearson")
r_nvul_p <- cor.test(g_nvul$MAIA_DIRt, g_nvul$diff_effort, method = "pearson")
r_vul_s  <- cor.test(g_vul$MAIA_DIRt,  g_vul$diff_effort,  method = "spearman")
r_nvul_s <- cor.test(g_nvul$MAIA_DIRt, g_nvul$diff_effort, method = "spearman")

# Fisher r-to-z para diferencia entre dos correlaciones independientes
fisher_z_diff <- function(r1, n1, r2, n2) {
  z1 <- atanh(r1); z2 <- atanh(r2)
  se <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  Z  <- (z1 - z2) / se
  data.frame(Z = Z, p = 2 * pnorm(-abs(Z)), SE = se)
}

fz <- fisher_z_diff(r1 = unname(r_vul_p$estimate),  n1 = nrow(g_vul),
                    r2 = unname(r_nvul_p$estimate), n2 = nrow(g_nvul))

cat(sprintf("Vulnerable:     Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_vul_p$estimate, r_vul_p$p.value, r_vul_s$estimate, r_vul_s$p.value, nrow(g_vul)))
cat(sprintf("Non-vulnerable: Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_nvul_p$estimate, r_nvul_p$p.value, r_nvul_s$estimate, r_nvul_s$p.value, nrow(g_nvul)))
cat(sprintf("Fisher r-to-z:  Z = %.3f, p = %.3f\n", fz$Z, fz$p))










#################################
### SASS

# ---- Robustez SASS: correlaciones por grupo + Fisher r-to-z ----
d_sass <- df_mod %>%
  dplyr::select(grupo, SASS_DIRt, diff_effort) %>%
  tidyr::drop_na(SASS_DIRt, diff_effort)

g_vul  <- dplyr::filter(d_sass, grupo == 1)   # vulnerable
g_nvul <- dplyr::filter(d_sass, grupo == 0)   # non-vulnerable

r_vul_p  <- cor.test(g_vul$SASS_DIRt,  g_vul$diff_effort,  method = "pearson")
r_nvul_p <- cor.test(g_nvul$SASS_DIRt, g_nvul$diff_effort, method = "pearson")
r_vul_s  <- cor.test(g_vul$SASS_DIRt,  g_vul$diff_effort,  method = "spearman")
r_nvul_s <- cor.test(g_nvul$SASS_DIRt, g_nvul$diff_effort, method = "spearman")

fz <- fisher_z_diff(r1 = unname(r_vul_p$estimate),  n1 = nrow(g_vul),
                    r2 = unname(r_nvul_p$estimate), n2 = nrow(g_nvul))

cat(sprintf("Vulnerable:     Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_vul_p$estimate, r_vul_p$p.value, r_vul_s$estimate, r_vul_s$p.value, nrow(g_vul)))
cat(sprintf("Non-vulnerable: Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_nvul_p$estimate, r_nvul_p$p.value, r_nvul_s$estimate, r_nvul_s$p.value, nrow(g_nvul)))
cat(sprintf("Fisher r-to-z:  Z = %.3f, p = %.3f\n", fz$Z, fz$p))













####################################
### Figura 3: MAIA y SASS × grupo (índice BLUP diff_effort)

library(patchwork)
COL_NONVUL <- "#E76F51"; COL_VUL <- "#2A9D8F"

# Modelos de interacción con fatiga como covariable (igual que en el texto)
m_maia <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
m_sass <- lm(diff_effort ~ SASS_DIRt * grupo + Fatigue_diff, data = df_mod)

panel <- function(model, xvar, xlab, title){
  pr <- as.data.frame(ggpredict(model, terms = c(paste0(xvar, " [all]"), "grupo")))
  pr$Group  <- factor(pr$group, levels = c("0","1"), labels = c("Non-vulnerable","Vulnerable"))
  pts <- df_mod; pts$Group <- factor(pts$grupo, levels = c(0,1), labels = c("Non-vulnerable","Vulnerable"))
  ggplot() +
    geom_point(data = pts, aes(x = .data[[xvar]], y = diff_effort, color = Group, fill = Group),
               shape = 21, alpha = 0.6, size = 2.3, stroke = 0.4) +
    geom_ribbon(data = pr, aes(x = x, ymin = conf.low, ymax = conf.high, fill = Group),
                alpha = 0.20, color = NA) +
    geom_line(data = pr, aes(x = x, y = predicted, color = Group), linewidth = 1.3) +
    scale_color_manual(values = c("Non-vulnerable" = COL_NONVUL, "Vulnerable" = COL_VUL), name = NULL) +
    scale_fill_manual(values  = c("Non-vulnerable" = COL_NONVUL, "Vulnerable" = COL_VUL), guide = "none") +
    labs(x = xlab, y = "Effort Difference", title = title) +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
}

pA <- panel(m_maia, "MAIA_DIRt", "MAIA", "Interoceptive Awareness by Group")
pB <- panel(m_sass, "SASS_DIRt", "SASS", "Social Adaptation by Group")

fig3 <- (pA + pB) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold", size = 14))

print(fig3)
ggsave("figure3.png", fig3, width = 9.5, height = 4.5, dpi = 600, bg = "white")







