
# Regresiones empíricas — datos reales de esfuerzo
## Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(emmeans)
library(car)
library(ggeffects)
library(patchwork)


#########################
# Data

## Slopes empíricos por sujeto y agente: pendiente OLS de decision ~ effort_level
long <- read.csv("data_glmm_filtered.csv", stringsAsFactors = FALSE)
long$effort_level <- as.integer(as.factor(long$c.effort))

slopes_emp <- long %>%
  group_by(sub, agent) %>%
  summarise(slope = coef(lm(decision ~ effort_level))[["effort_level"]],
            .groups = "drop") %>%
  pivot_wider(names_from = agent, values_from = slope,
              names_prefix = "slope_emp_") %>%
  rename(slope_emp_self  = slope_emp_0,
         slope_emp_other = slope_emp_1) %>%
  mutate(diff_effort_emp = slope_emp_other - slope_emp_self)

## Merge con cuestionarios
df_emp <- read.csv("dataset_final.csv", stringsAsFactors = FALSE) %>%
  select(sub, grupo, MAIA_DIRt, SASS_DIRt, IRI_PreocupacionEmpatica_DIRd) %>%
  left_join(slopes_emp, by = "sub") %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()



###################################
# Modelos diff_effort_emp con interacción de grupo

## Modelo 1: MAIA total
m_maia <- lm(diff_effort_emp ~ MAIA_DIRt * grupo, data = df_emp)
print(summary(m_maia))
print(Anova(m_maia, type = "II"))
trends <- emtrends(m_maia, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1)))
print(summary(trends, infer = c(TRUE, TRUE)))
print(pairs(trends))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 0)))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 1)))


## Modelo 2: SASS total
m_sass <- lm(diff_effort_emp ~ SASS_DIRt * grupo, data = df_emp)
print(summary(m_sass))
print(Anova(m_sass, type = "II"))
trends <- emtrends(m_sass, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1)))
print(summary(trends, infer = c(TRUE, TRUE)))
print(pairs(trends))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 0)))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 1)))


## Modelo 3: IRI Preocupación Empática
m_iri <- lm(diff_effort_emp ~ IRI_PreocupacionEmpatica_DIRd * grupo, data = df_emp)
print(summary(m_iri))
print(Anova(m_iri, type = "II"))
trends <- emtrends(m_iri, ~ grupo, var = "IRI_PreocupacionEmpatica_DIRd",
                   at = list(grupo = c(0, 1)))
print(summary(trends, infer = c(TRUE, TRUE)))
print(pairs(trends))
print(cor.test(~ IRI_PreocupacionEmpatica_DIRd + diff_effort_emp,
               data = subset(df_emp, grupo == 0)))
print(cor.test(~ IRI_PreocupacionEmpatica_DIRd + diff_effort_emp,
               data = subset(df_emp, grupo == 1)))



###################################
# Figuras: pendientes Control vs Vulnerable

COL_CONTROL    <- "#E76F51"
COL_VULNERABLE <- "#2A9D8F"

plot_slope <- function(model, xvar, xlab, title) {
  pred <- as.data.frame(ggpredict(model,
                                  terms = c(paste0(xvar, " [all]"),
                                            "grupo [0, 1]")))
  ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.20, color = NA) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = c("0" = COL_CONTROL, "1" = COL_VULNERABLE),
                       labels = c("Control", "Vulnerable"), name = NULL) +
    scale_fill_manual(values  = c("0" = COL_CONTROL, "1" = COL_VULNERABLE),
                      labels = c("Control", "Vulnerable"), name = NULL) +
    labs(title = title, x = xlab, y = "Empirical Effort Difference") +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "bottom")
}

fig3_emp <- (plot_slope(m_iri,  "IRI_PreocupacionEmpatica_DIRd", "IRI",
                        "Empathic Concern by Group") +
             plot_slope(m_maia, "MAIA_DIRt", "MAIA",
                        "Interoceptive Awareness by Group") +
             plot_slope(m_sass, "SASS_DIRt", "SASS",
                        "Social Adaptation by Group")) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 14))

print(fig3_emp)

## ggsave("figure3_empirical.png", fig3_emp, width = 10.5, height = 3.8, dpi = 600, bg = "white")

