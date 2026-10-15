
# GLMM: completion failure on accepted trials
## 'success' is coded 1 = completion failure


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)


###############################################################################
# Libraries

library(tidyverse)
library(lme4)
library(emmeans)
library(performance)
library(car)


###############################################################################
# Import Data

alldata.sc <- read.csv("data_glmm_filtered.csv", header = T)

## Accepted trials only
alldata.sc_a <- subset(alldata.sc, decision == 1)
#alldata.sc_a$Fatigue_diff <- as.numeric(scale(alldata.sc_a$Fatigue_diff))

## Optimizer settings
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


###############################################################################
# GLMM

## Model 1: four-way interaction + random slopes
m1 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
           data=alldata.sc_a, family=binomial, control=ctrl)

## Model 2: four-way interaction + random intercept
m2 <- glmer(success ~ c.reward*agent*c.effort*grupo + Fatigue_diff + (1|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model 3: mirrored (reward*agent*grupo + effort*agent*grupo) + random slopes
m3 <- glmer(success ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model 4: mirrored + random intercept (reported)
m4 <- glmer(success ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1|sub),
            data=alldata.sc_a, family=binomial, control=ctrl)

## Model comparison
anova(m1, m2, m3, m4)


###############################################################################
# Reported model: m4

## Singular fits
isSingular(m1)
isSingular(m2)
isSingular(m3)
isSingular(m4)

## Summary of the reported model
summary(m4)
car::Anova(m4, type = "II")
performance::r2_nakagawa(m4)
anova(m4)

## Effort slopes by group
et <- emtrends(m4, ~ grupo, var = "c.effort")   # averaged over agent
summary(et, infer = c(TRUE, TRUE))
pairs(et)

## Empirical failure rates (accepted trials) by group
alldata.sc_a %>% group_by(grupo) %>%
  summarise(n_trials = n(), fail_rate = mean(success), .groups = "drop")


###############################################################################
# Figure 5: main effect of group (averaged over beneficiary)

## Group labels and colors
LAB_NONVUL <- "Non-vulnerable"; LAB_VUL <- "Vulnerable"
COL_NONVUL <- "#E76F51";        COL_VUL <- "#2A9D8F"   # same palette as Figs. 2 and 3

## Convert p-values to significance stars
p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.10) "." else "ns"
}

## Estimated probability by group (averaged over agent; effort and reward at z = 0)
df_grp <- as.data.frame(
  emmeans(m4, ~ grupo, type = "response", at = list(c.reward = 0, c.effort = 0)))
df_grp$Group <- factor(df_grp$grupo, levels = c(0, 1), labels = c(LAB_NONVUL, LAB_VUL))

## Empirical failure rate per subject (all accepted trials)
pts <- alldata.sc_a %>%
  group_by(sub, grupo) %>%
  summarise(prob_indiv = mean(success), .groups = "drop") %>%
  mutate(Group = factor(grupo, levels = c(0, 1), labels = c(LAB_NONVUL, LAB_VUL)))

## Significance bracket: p-value of the group main effect (Type II), as reported in the text
p_grupo <- car::Anova(m4, type = "II")["grupo", "Pr(>Chisq)"]
y_top   <- max(pts$prob_indiv, na.rm = TRUE)
sig <- data.frame(x1 = 1, x2 = 2, y = y_top * 1.08, yl = y_top * 1.12,
                  lab = p_a_estrellas(p_grupo))

## Plot
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

## Print and save figure
print(p5)
ggsave("figure5.png", p5, width = 5, height = 5, dpi = 300, bg = "white")
