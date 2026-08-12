

## Figuras



library(tidyverse)
library(readxl)
library(lme4)       # glmer()
library(car)        # vif()
library(ggeffects)  # ggpredict()
library(ggcorrplot) # ggcorrplot()
library(corrplot)   # corrplot()
library(emmeans)    # emtrends(), pairs()
library(performance) # check_model()


library(ggplot2)
library(dplyr)
library(scales)
library(ggdist)
library(patchwork)




######################################
## Importar datos de vuelta

alldata.sc <- read.csv("datos_long_glmm_filtrados.csv", header = T)


# Excluir trials omitidos y eliminar columna índice
alldata.sc <- subset(alldata.sc, decision!= 2)
alldata.sc <- subset(alldata.sc, select = -c(X))

## Modelo 3: efectos de reward y effort separados con slopes aleatorios
m4 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))




indv_dataset = read.csv("post_hoc_v2.csv", header = T)


pred_eff <- ggpredict(m4, terms = c("c.effort [all]", "agent", "grupo")) %>%
  as.data.frame() %>%
  mutate(
    Agent = factor(group, levels = c(0, 1), labels = c("Self", "Other")),
    Group = factor(facet, levels = c(0, 1), labels = c("Control", "Vulnerable"))
  )



panel_a <- ggplot(pred_eff, aes(x = x, y = predicted, colour = Agent, fill = Agent)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Group) +
  scale_y_continuous(
    limits = c(0.7, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = percent_format(accuracy = 1)
  ) +
  labs(x = "Standardised effort", y = "P(accept work offer)",
       title = "Predicted probabilities of decision") +
  theme_minimal(base_size = 12)

print(panel_a)

indv_dataset <- indv_dataset %>%
  mutate(sub = as.character(sub)) %>%
  left_join(
    alldata.sc %>%
      distinct(sub, grupo) %>%
      mutate(sub = as.character(sub)),
    by = "sub"
  ) %>%
  mutate(grupo = factor(grupo, levels = c(0, 1),
                        labels = c("Control", "Vulnerable")))



panel_b <- ggplot(indv_dataset, aes(x = grupo, y = diff_effort, fill = grupo)) +
  ggdist::stat_halfeye(adjust = 0.6, width = 0.6, .width = 0,
                       justification = -0.2, point_colour = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.05, size = 1.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(x = NULL, y = "Effort Difference") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(panel_b)

figure2 <- panel_a + panel_b +
  plot_layout(ncol = 2, widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A")


print(figure2)
















# Cargar datos
df_mod <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)


m1 <- lm(diff_effort ~ IRI_PreocupacionEmpatica_DIRd * grupo, data = df_mod)

m2 <- lm(diff_effort ~ MAIA_DIRt * grupo, data = df_mod)

m3 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)



m1 <- lm(diff_effort ~ IRI_PreocupacionEmpatica_DIRd * grupo, data = df_mod)
panel_a <- plot(ggpredict(m1, terms = c("IRI_PreocupacionEmpatica_DIRd", "grupo"))) +
  labs(x = "IRI", y = "Effort Difference", title = "Empathic Concern by Group") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.2), labels = number_format(accuracy = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(15, 25, by = 5), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  theme_minimal(base_size = 12)

m2 <- lm(diff_effort ~ MAIA_DIRt * grupo, data = df_mod)
panel_b <- plot(ggpredict(m2, terms = c("MAIA_DIRt", "grupo"))) +
  labs(x = "MAIA", y = "Effort Difference", title = "Interoceptive Awareness by Group") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.2), labels = number_format(accuracy = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(50, 125, by = 25), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  theme_minimal(base_size = 12)

m3 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)
panel_c <- plot(ggpredict(m3, terms = c("SASS_DIRt", "grupo"))) +
  labs(x = "SASS", y = "Effort Difference", title = "Social Adaptation by Group") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Vulnerable"), name = "Group") +
  scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.2), labels = number_format(accuracy = 0.1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(25, 55, by = 10), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  theme_minimal(base_size = 12)

figure3 <- panel_a + panel_b + panel_c +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

print(figure3)













pred_eff <- ggpredict(m4, terms = c("c.effort [all]", "agent", "grupo"))
write.csv(as.data.frame(pred_eff), "pred_eff.csv", row.names = FALSE)

