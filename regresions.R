
# Multiple regressions: MAIA, SASS and IRI x group on the prosocial-effort index


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)


###############################################################################
# Libraries

library(tidyverse)
library(emmeans)
library(ggeffects)
library(patchwork)
library(car)


###############################################################################
# Import Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

## Filter by group
# df <- subset(df, grupo == "1")

## Build IRI composites
df <- df %>%
  mutate(IRI_Cognitivo = IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd,
         IRI_Afectivo  = IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd)

## Analytic subset (variables used in the models)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "MAIA_DIRt", "SASS_DIRt", "IRI_DIRt",
                 "IRI_Cognitivo", "IRI_Afectivo",
                 "Fatigue_diff")

df_mod <- df %>%
  dplyr::select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))


###############################################################################
# MAIA model

## MAIA total with group interaction
m1 <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
summary(m1)
car::Anova(m1, type = "II")

## Simple slopes of MAIA by group
trends_maia <- emtrends(m1, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1)))
summary(trends_maia, infer = c(TRUE, TRUE))
pairs(trends_maia)


###############################################################################
# SASS model

## SASS total with group interaction
m2 <- lm(diff_effort ~ SASS_DIRt * grupo + Fatigue_diff, data = df_mod)
summary(m2)
car::Anova(m2, type = "II")

## Simple slopes of SASS by group
trends_sass <- emtrends(m2, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1)))
summary(trends_sass, infer = c(TRUE, TRUE))
pairs(trends_sass)


###############################################################################
# IRI models

## IRI total with group interaction
m3 <- lm(diff_effort ~ IRI_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m3))
car::Anova(m3, type = "II")

## Simple slopes of IRI total by group
trends_iri <- emtrends(m3, ~ grupo, var = "IRI_DIRt", at = list(grupo = c(0, 1)))
summary(trends_iri, infer = c(TRUE, TRUE))
pairs(trends_iri)

## IRI components with group interaction
m4 <- lm(diff_effort ~ IRI_Cognitivo * grupo + IRI_Afectivo * grupo + Fatigue_diff, data = df_mod)
print(summary(m4))
car::Anova(m4, type = "II")

## Simple slopes of the cognitive component by group
trends_iri_cognitivo <- emtrends(m4, ~ grupo, var = "IRI_Cognitivo", at = list(grupo = c(0, 1)))
summary(trends_iri_cognitivo, infer = c(TRUE, TRUE))
pairs(trends_iri_cognitivo)


###############################################################################
# MAIA robustness: correlations by group + Fisher r-to-z

## MAIA and prosocial-effort data
d_maia <- df_mod %>%
  dplyr::select(grupo, MAIA_DIRt, diff_effort) %>%
  tidyr::drop_na(MAIA_DIRt, diff_effort)

## Split by group
g_vul  <- dplyr::filter(d_maia, grupo == 1)   # vulnerable
g_nvul <- dplyr::filter(d_maia, grupo == 0)   # non-vulnerable

## Pearson + Spearman by group
r_vul_p  <- cor.test(g_vul$MAIA_DIRt,  g_vul$diff_effort,  method = "pearson")
r_nvul_p <- cor.test(g_nvul$MAIA_DIRt, g_nvul$diff_effort, method = "pearson")
r_vul_s  <- cor.test(g_vul$MAIA_DIRt,  g_vul$diff_effort,  method = "spearman")
r_nvul_s <- cor.test(g_nvul$MAIA_DIRt, g_nvul$diff_effort, method = "spearman")

## Fisher r-to-z for the difference between two independent correlations
fisher_z_diff <- function(r1, n1, r2, n2) {
  z1 <- atanh(r1); z2 <- atanh(r2)
  se <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  Z  <- (z1 - z2) / se
  data.frame(Z = Z, p = 2 * pnorm(-abs(Z)), SE = se)
}

fz <- fisher_z_diff(r1 = unname(r_vul_p$estimate),  n1 = nrow(g_vul),
                    r2 = unname(r_nvul_p$estimate), n2 = nrow(g_nvul))

## Print results
cat(sprintf("Vulnerable:     Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_vul_p$estimate, r_vul_p$p.value, r_vul_s$estimate, r_vul_s$p.value, nrow(g_vul)))
cat(sprintf("Non-vulnerable: Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_nvul_p$estimate, r_nvul_p$p.value, r_nvul_s$estimate, r_nvul_s$p.value, nrow(g_nvul)))
cat(sprintf("Fisher r-to-z:  Z = %.3f, p = %.3f\n", fz$Z, fz$p))


###############################################################################
# SASS robustness: correlations by group + Fisher r-to-z

## SASS and prosocial-effort data
d_sass <- df_mod %>%
  dplyr::select(grupo, SASS_DIRt, diff_effort) %>%
  tidyr::drop_na(SASS_DIRt, diff_effort)

## Split by group
g_vul  <- dplyr::filter(d_sass, grupo == 1)   # vulnerable
g_nvul <- dplyr::filter(d_sass, grupo == 0)   # non-vulnerable

## Pearson + Spearman by group
r_vul_p  <- cor.test(g_vul$SASS_DIRt,  g_vul$diff_effort,  method = "pearson")
r_nvul_p <- cor.test(g_nvul$SASS_DIRt, g_nvul$diff_effort, method = "pearson")
r_vul_s  <- cor.test(g_vul$SASS_DIRt,  g_vul$diff_effort,  method = "spearman")
r_nvul_s <- cor.test(g_nvul$SASS_DIRt, g_nvul$diff_effort, method = "spearman")

## Fisher r-to-z
fz <- fisher_z_diff(r1 = unname(r_vul_p$estimate),  n1 = nrow(g_vul),
                    r2 = unname(r_nvul_p$estimate), n2 = nrow(g_nvul))

## Print results
cat(sprintf("Vulnerable:     Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_vul_p$estimate, r_vul_p$p.value, r_vul_s$estimate, r_vul_s$p.value, nrow(g_vul)))
cat(sprintf("Non-vulnerable: Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_nvul_p$estimate, r_nvul_p$p.value, r_nvul_s$estimate, r_nvul_s$p.value, nrow(g_nvul)))
cat(sprintf("Fisher r-to-z:  Z = %.3f, p = %.3f\n", fz$Z, fz$p))


###############################################################################
# IRI robustness: correlations by group + Fisher r-to-z

## IRI and prosocial-effort data
d_IRI <- df_mod %>%
  dplyr::select(grupo, IRI_DIRt, diff_effort) %>%
  tidyr::drop_na(IRI_DIRt, diff_effort)

## Split by group
g_vul  <- dplyr::filter(d_IRI, grupo == 1)   # vulnerable
g_nvul <- dplyr::filter(d_IRI, grupo == 0)   # non-vulnerable

## Pearson + Spearman by group
r_vul_p  <- cor.test(g_vul$IRI_DIRt,  g_vul$diff_effort,  method = "pearson")
r_nvul_p <- cor.test(g_nvul$IRI_DIRt, g_nvul$diff_effort, method = "pearson")
r_vul_s  <- cor.test(g_vul$IRI_DIRt,  g_vul$diff_effort,  method = "spearman")
r_nvul_s <- cor.test(g_nvul$IRI_DIRt, g_nvul$diff_effort, method = "spearman")

## Fisher r-to-z for the difference between two independent correlations
fisher_z_diff <- function(r1, n1, r2, n2) {
  z1 <- atanh(r1); z2 <- atanh(r2)
  se <- sqrt(1/(n1 - 3) + 1/(n2 - 3))
  Z  <- (z1 - z2) / se
  data.frame(Z = Z, p = 2 * pnorm(-abs(Z)), SE = se)
}

fz <- fisher_z_diff(r1 = unname(r_vul_p$estimate),  n1 = nrow(g_vul),
                    r2 = unname(r_nvul_p$estimate), n2 = nrow(g_nvul))

## Print results
cat(sprintf("Vulnerable:     Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_vul_p$estimate, r_vul_p$p.value, r_vul_s$estimate, r_vul_s$p.value, nrow(g_vul)))
cat(sprintf("Non-vulnerable: Pearson r = %.3f (p = %.3f), Spearman rho = %.3f (p = %.3f), n = %d\n",
            r_nvul_p$estimate, r_nvul_p$p.value, r_nvul_s$estimate, r_nvul_s$p.value, nrow(g_nvul)))
cat(sprintf("Fisher r-to-z:  Z = %.3f, p = %.3f\n", fz$Z, fz$p))


###############################################################################
# Figure 3: MAIA and SASS x group (BLUP index diff_effort)

## Group colors
COL_NONVUL <- "#E76F51"; COL_VUL <- "#2A9D8F"

## Interaction models with fatigue as covariate (as in the text)
m_maia <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
m_sass <- lm(diff_effort ~ SASS_DIRt * grupo + Fatigue_diff, data = df_mod)

## Panel function: observed points + predicted lines with 95% CI
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

## Panels A (MAIA) and B (SASS)
pA <- panel(m_maia, "MAIA_DIRt", "MAIA", "Interoceptive Awareness by Group")
pB <- panel(m_sass, "SASS_DIRt", "SASS", "Social Adaptation by Group")

## Composition: A + B
fig3 <- (pA + pB) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold", size = 14))

## Print and save figure
print(fig3)
ggsave("figure3.png", fig3, width = 9.5, height = 4.5, dpi = 600, bg = "white")
