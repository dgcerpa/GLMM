
## Regresiones empíricas — datos reales de esfuerzo
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(emmeans)
library(car)
library(ggeffects)
library(patchwork)


#########################
## Data

# Slopes empíricos por sujeto y agente: pendiente OLS de decision ~ effort_level
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

# Merge con cuestionarios
df_emp <- read.csv("dataset_final.csv", stringsAsFactors = FALSE) %>%
  select(sub, grupo, MAIA_DIRt, SASS_DIRt, Fatigue_diff) %>%
  left_join(slopes_emp, by = "sub") %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()



###################################
## Modelos diff_effort_emp con interacción de grupo

## Modelo 1: MAIA total
m_maia <- lm(diff_effort_emp ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_emp)
print(summary(m_maia))
print(Anova(m_maia, type = "II"))
trends <- emtrends(m_maia, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1)))
print(summary(trends, infer = c(TRUE, TRUE)))
print(pairs(trends))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 0)))
print(cor.test(~ MAIA_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 1)))


## Modelo 2: SASS total
m_sass <- lm(diff_effort_emp ~ SASS_DIRt * grupo + Fatigue_diff, data = df_emp)
print(summary(m_sass))
print(Anova(m_sass, type = "II"))
trends <- emtrends(m_sass, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1)))
print(summary(trends, infer = c(TRUE, TRUE)))
print(pairs(trends))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 0)))
print(cor.test(~ SASS_DIRt + diff_effort_emp, data = subset(df_emp, grupo == 1)))


###################################
## Figura 2: proporción empírica de aceptar por nivel de esfuerzo

COL_SELF       <- "#8AA624"
COL_OTHER      <- "#6A4C93"
COL_CONTROL    <- "#E76F51"
COL_VULNERABLE <- "#2A9D8F"

# Agregación: proporción por sujeto en cada celda, luego media y SEM entre sujetos
p_accept_subject <- long %>%
  group_by(sub, grupo, agent, effort_level) %>%
  summarise(p_accept = mean(decision), .groups = "drop") %>%
  mutate(Agent = factor(agent, levels = c(0, 1), labels = c("Self", "Other")),
         Group = factor(grupo, levels = c(0, 1), labels = c("Control", "Vulnerable")))

p_accept_cell <- p_accept_subject %>%
  group_by(Group, Agent, effort_level) %>%
  summarise(mean = mean(p_accept),
            sem  = sd(p_accept) / sqrt(n()),
            .groups = "drop") %>%
  mutate(lo = mean - 1.96 * sem,
         hi = mean + 1.96 * sem)


## Versión 1: Panel A (curvas) + Panel B (distribución de diff_effort_emp)

p_A <- ggplot(p_accept_cell, aes(x = effort_level, y = mean,
                                 color = Agent, fill = Agent)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Group) +
  scale_color_manual(values = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_fill_manual(values  = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.5, 1.0)) +
  labs(x = "Effort level", y = "P(accept work offer)",
       color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

df_dist <- df_emp %>%
  mutate(Group = factor(grupo, levels = c(0, 1), labels = c("Control", "Vulnerable")))

p_B <- ggplot(df_dist, aes(x = Group, y = diff_effort_emp,
                           color = Group, fill = Group)) +
  geom_violin(alpha = 0.35, trim = FALSE, width = 0.7, linewidth = 0.4) +
  geom_boxplot(width = 0.12, alpha = 0.85, outlier.shape = NA,
               color = "black", linewidth = 0.4) +
  geom_jitter(width = 0.06, alpha = 0.7, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  scale_color_manual(values = c("Control" = COL_CONTROL, "Vulnerable" = COL_VULNERABLE)) +
  scale_fill_manual(values  = c("Control" = COL_CONTROL, "Vulnerable" = COL_VULNERABLE)) +
  labs(x = NULL, y = "Effort Difference (Other \u2212 Self)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

fig2_v1 <- (p_A + p_B) +
  plot_layout(widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

print(fig2_v1)
ggsave("figure2_empirical_v1.png", fig2_v1,
       width = 9.5, height = 4.2, dpi = 600, bg = "white")


## Versión 2: solo Panel A, con puntos individuales por sujeto

# Offset horizontal por agente para reducir overlap entre puntos Self y Other
p_accept_subject <- p_accept_subject %>%
  mutate(x_offset = effort_level + ifelse(agent == 0, -0.12, 0.12))

fig2_v2 <- ggplot(p_accept_cell, aes(x = effort_level, y = mean,
                                     color = Agent, fill = Agent)) +
  geom_jitter(data = p_accept_subject,
              aes(x = x_offset, y = p_accept),
              width = 0.05, height = 0, alpha = 0.30, size = 1.2) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Group) +
  scale_color_manual(values = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_fill_manual(values  = c("Self" = COL_SELF, "Other" = COL_OTHER)) +
  scale_x_continuous(breaks = 1:4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = "Effort level", y = "P(accept work offer)",
       color = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 11))

print(fig2_v2)
ggsave("figure2_empirical_v2.png", fig2_v2,
       width = 7.5, height = 4.2, dpi = 600, bg = "white")



###################################
## Figura 3: pendientes Control vs Vulnerable

plot_slope <- function(model, data, xvar, xlab, title) {
  pred <- as.data.frame(ggpredict(model,
                                  terms = c(paste0(xvar, " [all]"),
                                            "grupo [0, 1]")))
  points_df <- data %>%
    mutate(group = factor(as.integer(grupo), levels = c(0, 1)))

  ggplot() +
    geom_ribbon(data = pred,
                aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
                alpha = 0.20, color = NA) +
    geom_point(data = points_df,
               aes(x = .data[[xvar]], y = diff_effort_emp, color = group),
               alpha = 0.55, size = 1.8) +
    geom_line(data = pred,
              aes(x = x, y = predicted, color = group),
              linewidth = 1.0) +
    scale_color_manual(values = c("0" = COL_CONTROL, "1" = COL_VULNERABLE),
                       labels = c("Control", "Vulnerable"), name = NULL) +
    scale_fill_manual(values  = c("0" = COL_CONTROL, "1" = COL_VULNERABLE),
                      labels = c("Control", "Vulnerable"), name = NULL) +
    labs(title = title, x = xlab, y = "Empirical Effort Difference") +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "bottom")
}

fig3_emp <- (plot_slope(m_maia, df_emp, "MAIA_DIRt", "MAIA",
                        "Interoceptive Awareness by Group") +
             plot_slope(m_sass, df_emp, "SASS_DIRt", "SASS",
                        "Social Adaptation by Group")) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold", size = 14))

print(fig3_emp)
ggsave("figure3_empirical.png", fig3_emp,
       width = 7.5, height = 3.8, dpi = 600, bg = "white")




t.test(diff_effort_emp ~ grupo, data = df_emp)
wilcox.test(diff_effort_emp ~ grupo, data = df_emp)
