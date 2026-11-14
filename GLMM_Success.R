# GLMM: completion failure on accepted trials + Figure 5 (A: NASA-TLX; B: completion failure)
## 'success' is coded 1 = completion failure
## Run fatigue.R first: Figure 5A uses 'wide' and 'p_int' from its environment


###############################################################################
# Libraries

library(tidyverse)
library(lme4)
library(emmeans)
library(performance)
library(car)
library(patchwork)
library(svglite)


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
# Figure 5: perceived effort difference (A) and completion failure (B) by group

## Check that fatigue.R was run first (objects needed for panel A)
if (!exists("wide") || !exists("p_int")) {
  stop("Run fatigue.R first: 'wide' and 'p_int' are required for Figure 5A.")
}

## Group labels and colors (same palette as Figs. 2 and 3)
LAB_NONVUL <- "Non-vulnerable"; LAB_VUL <- "Vulnerable"
COL_NONVUL <- "#E76F51";        COL_VUL <- "#2A9D8F"
COLS <- setNames(c(COL_NONVUL, COL_VUL), c(LAB_NONVUL, LAB_VUL))

## Convert p-values to significance stars
p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.10) "." else "ns"
}


###############################################################################
# Figure 5: shared style (matches Figs. 2B and 3)

## Reproducible jitter
set.seed(1)

## Common theme
theme_fig5 <- theme_classic(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

## Significance bracket placed above the observed data range
make_bracket <- function(y_values, p) {
  y_max <- max(y_values, na.rm = TRUE)
  rng   <- diff(range(y_values, na.rm = TRUE))
  data.frame(x1 = 1, x2 = 2,
             y    = y_max + 0.08 * rng,
             yl   = y_max + 0.13 * rng,
             tick = 0.03 * rng,
             lab  = p_a_estrellas(p))
}


###############################################################################
# Figure 5, Panel A: perceived effort difference (NASA-TLX, hard - easy) by group

## Mean and t-based 95% CI by group (from 'wide', created in fatigue.R)
diff_ci <- wide %>%
  group_by(grupo) %>%
  summarise(media = mean(NASA_diff),
            ci    = qt(0.975, df = n() - 1) * sd(NASA_diff) / sqrt(n()),
            .groups = "drop")

## Significance bracket: p-value of the difficulty x group interaction ('p_int', from fatigue.R)
sig_A <- make_bracket(wide$NASA_diff, p_int)

## Plot
p5A <- ggplot(wide, aes(x = grupo, y = NASA_diff, color = grupo, fill = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_violin(alpha = 0.35, trim = TRUE, width = 0.7, linewidth = 0.4) +
  geom_jitter(width = 0.06, alpha = 0.7, size = 1.5) +
  geom_errorbar(data = diff_ci, inherit.aes = FALSE,
                aes(x = grupo, ymin = media - ci, ymax = media + ci),
                width = 0.08, linewidth = 0.6, color = "black") +
  geom_point(data = diff_ci, inherit.aes = FALSE, aes(x = grupo, y = media),
             shape = 21, size = 3, fill = "white", color = "black", stroke = 0.8) +
  geom_segment(data = sig_A, inherit.aes = FALSE, aes(x = x1, xend = x2, y = y, yend = y)) +
  geom_segment(data = sig_A, inherit.aes = FALSE, aes(x = x1, xend = x1, y = y, yend = y - tick)) +
  geom_segment(data = sig_A, inherit.aes = FALSE, aes(x = x2, xend = x2, y = y, yend = y - tick)) +
  geom_text(data = sig_A, inherit.aes = FALSE, aes(x = 1.5, y = yl, label = lab), size = 5) +
  scale_color_manual(values = COLS) +
  scale_fill_manual(values  = COLS) +
  labs(x = NULL, y = "Perceived Effort Difference (Hard \u2212 Easy)",
       title = "Perceived Effort") +
  theme_fig5


###############################################################################
# Figure 5, Panel B: probability of completion failure (main effect of group, averaged over beneficiary)

## Estimated probability and 95% CI by group (averaged over agent; effort and reward at z = 0)
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
sig_B   <- make_bracket(pts$prob_indiv, p_grupo)

## Plot
p5B <- ggplot(pts, aes(x = Group, y = prob_indiv, color = Group, fill = Group)) +
  geom_violin(alpha = 0.35, trim = TRUE, width = 0.7, linewidth = 0.4) +
  geom_jitter(width = 0.06, height = 0, alpha = 0.7, size = 1.5) +
  geom_errorbar(data = df_grp, inherit.aes = FALSE,
                aes(x = Group, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.08, linewidth = 0.6, color = "black") +
  geom_point(data = df_grp, inherit.aes = FALSE, aes(x = Group, y = prob),
             shape = 21, size = 3, fill = "white", color = "black", stroke = 0.8) +
  geom_segment(data = sig_B, inherit.aes = FALSE, aes(x = x1, xend = x2, y = y, yend = y)) +
  geom_segment(data = sig_B, inherit.aes = FALSE, aes(x = x1, xend = x1, y = y, yend = y - tick)) +
  geom_segment(data = sig_B, inherit.aes = FALSE, aes(x = x2, xend = x2, y = y, yend = y - tick)) +
  geom_text(data = sig_B, inherit.aes = FALSE, aes(x = 1.5, y = yl, label = lab), size = 5) +
  scale_color_manual(values = COLS) +
  scale_fill_manual(values  = COLS) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Probability of Failure", title = "Task Completion") +
  theme_fig5


###############################################################################
# Figure 5: composition (A + B)

## Composition
fig5 <- (p5A + p5B) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))


## Print and save figure
print(fig5)
ggsave("figure5A.svg", p5A, width = 5, height = 4.5, dpi = 600, bg = "white")
ggsave("figure5B.svg", p5B, width = 5, height = 4.5, dpi = 600, bg = "white")
ggsave("figure5.svg", fig5, width = 9.5, height = 4.5, dpi = 600, bg = "white")


