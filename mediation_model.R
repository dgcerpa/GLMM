
# Moderated causal mediation: MAIA -> prosocial effort -> SASS, by group


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)

## Reset session: close graphics devices, set options and define a console-clear helper
suppressMessages({
  if (!is.null(dev.list())) dev.off()
  graphics.off()
  options(stringsAsFactors = FALSE, digits = 4, warn = 0)
  clear <- function() cat("\014\n")
})
clear()

## Working directory (optional)
# MAIN_PATH <- "/home/ax/esd-lab/diego-uai/"
# setwd(MAIN_PATH)


###############################################################################
# Libraries

library(ggplot2)
library(dplyr)
library(DiagrammeR)
library(patchwork)
library(svglite)


###############################################################################
# Import Data

## Variables of interest
group_var <- "group"
main_vars <- c("diff_effort", "SASS_DIRt", "MAIA_DIRt", "Fatigue_diff")
vars <- main_vars

## Load data (grupo renamed to group)
data <- read.csv("dataset_final.csv", header = T) %>% rename(!!sym(group_var) := grupo)

## Alternative prosocial-effort indices (optional)
# data$diff_effort <- data$diff_effort1
# data$diff_effort <- data$diff_effort2

## Keep variables of interest and drop incomplete cases
data <- data %>%
  dplyr::select(group, dplyr::all_of(vars)) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>%
  na.omit() %>%
  dplyr::select(dplyr::all_of(c(vars, group_var)))


###############################################################################
# Mediation variables

## X = MAIA, M = prosocial effort, Y = SASS, C = dFatigue (covariate)
data$X <- data$MAIA_DIRt
data$M <- data$diff_effort
data$Y <- data$SASS_DIRt
data$C <- data$Fatigue_diff

## Group factor
data$G <- factor(data$group, levels = c(0, 1), labels = c("No Vulnerable", "Vulnerable"))

## Split data by group
data_low  <- subset(data, G == "No Vulnerable")
data_high <- subset(data, G == "Vulnerable")


###############################################################################
# Mediator and outcome models by group

## Mediator (M ~ X + C) and outcome (Y ~ X + M + C) models
med_low  <- lm(M ~ X + C, data = data_low)
out_low  <- lm(Y ~ X + M + C, data = data_low)
med_high <- lm(M ~ X + C, data = data_high)
out_high <- lm(Y ~ X + M + C, data = data_high)


###############################################################################
# Exploratory plots of the mediation paths

## X -> M
p1 <- ggplot(data, aes(X, M, color = G)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic(base_size = 13) +
  labs(x = "MAIA", y = "diff_effort", color = "Group", title = "X → M")

## M -> Y
p2 <- ggplot(data, aes(M, Y, color = G)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic(base_size = 13) +
  labs(x = "diff_effort", y = "SASS", color = "Group", title = "M → Y")

## X -> Y
p3 <- ggplot(data, aes(X, Y, color = G)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic(base_size = 13) +
  labs(x = "MAIA", y = "SASS", color = "Group", title = "X → Y")

## Composition of the three panels
p <- wrap_plots(list(p1, p2, p3), ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")
p


###############################################################################
# Bootstrap mediation by group

## Total effect in the full sample (optional)
# dir_all <- lm(Y ~ X + C, data = data)
# summary(dir_all)

## Number of bootstrap resamples
# set.seed(1)
nboot <- 5000
# nboot <- 10000
# nboot <- 1000
# nboot <- 3000

## ACME, ADE, total effect and proportion mediated (nonparametric bootstrap)
res_low  <- mediation::mediate(med_low,  out_low,  treat = "X", mediator = "M", boot = TRUE, sims = nboot)
res_high <- mediation::mediate(med_high, out_high, treat = "X", mediator = "M", boot = TRUE, sims = nboot)


###############################################################################
# Path diagram (exploratory)

## Path coefficients (a, b, c') by group
lab_a  <- sprintf("a\nNon-vuln=%.3f\nVuln=%.3f", coef(med_low)["X"], coef(med_high)["X"])
lab_b  <- sprintf("b\nNon-vuln=%.3f\nVuln=%.3f", coef(out_low)["M"], coef(out_high)["M"])
lab_cp <- sprintf("c_prime\nNon-vuln=%.3f\nVuln=%.3f", coef(out_low)["X"], coef(out_high)["X"])

## Diagram (Graphviz)
g <- grViz(sprintf('
digraph mediation {

graph [layout=neato overlap=false splines=true]
node  [shape=box style="rounded,filled" fontname="Helvetica"]

X [label="X" pos="0,0!"   fillcolor="#d9ead3"]
M [label="M" pos="3,1.5!" fillcolor="#cfe2f3"]
Y [label="Y" pos="6,0!"   fillcolor="#fff2cc"]

X -> M [label="%s"]
M -> Y [label="%s"]
X -> Y [label="%s"]
}
', lab_a, lab_b, lab_cp))
g


###############################################################################
# Results of the mediation by group

## Clear console and print summaries
clear()
summary(res_low)
summary(res_high)


###############################################################################
# Moderated mediation (joint models with group interactions)

## Numeric group (0 = non-vulnerable, 1 = vulnerable)
data$G_num <- as.integer(as.character(data$group))

## Seed for the bootstrap
set.seed(1)

## Joint mediator and outcome models (all predictors interact with group)
med_mod <- lm(M ~ X*G_num + C*G_num, data = data)
out_mod <- lm(Y ~ X*G_num + M*G_num + C*G_num, data = data)

## Moderated mediation model (input for test.modmed)
fit_mod <- mediation::mediate(med_mod, out_mod, treat = "X", mediator = "M",
                              control.value = 0, treat.value = 1,
                              boot = TRUE, boot.ci.type = "perc", sims = nboot)

## Conditional effects: non-vulnerable group
control_mod <- mediation::mediate(med_mod, out_mod, treat = "X", mediator = "M",
                                  covariates = list(G_num = 0), control.value = 0, treat.value = 1,
                                  boot = TRUE, boot.ci.type = "perc", sims = nboot)

## Conditional effects: vulnerable group
vulnerable_mod <- mediation::mediate(med_mod, out_mod, treat = "X", mediator = "M",
                                     covariates = list(G_num = 1), control.value = 0, treat.value = 1,
                                     boot = TRUE, boot.ci.type = "perc", sims = nboot)

## Between-group difference in ACME and ADE
group_difference <- mediation::test.modmed(fit_mod, covariates.1 = list(G_num = 0),
                                           covariates.2 = list(G_num = 1), sims = nboot)

## Print results
summary(control_mod)
summary(vulnerable_mod)
group_difference
summary(med_mod)
summary(out_mod)


###############################################################################
# Figure 4: mediation path diagrams by group (values from res_low / res_high)

## Convert p-values to significance stars
p_stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))

## Panel function: path diagram + effects (ACME, ADE, total, proportion mediated)
mediation_panel <- function(res, tag, title, col_ind = "#1AA3E8", col_dir = "#E8231B") {

  fmt <- function(x) formatC(x, format = "f", digits = 4)

  ## Boxes, arrows (a, b, c') and path labels; coord_fixed keeps the angles exact
  boxes <- data.frame(
    lab  = c("Δ Effort", "MAIA", "SASS"),
    xmin = c(2.90, 0.45, 5.25), xmax = c(5.35, 2.70, 7.40),
    ymin = c(5.50, 3.22, 3.22), ymax = c(6.45, 4.22, 4.22)
  )
  arrows <- data.frame(
    x = c(1.60, 4.10, 2.70), y = c(4.22, 5.50, 3.72),
    xend = c(3.95, 6.50, 5.20), yend = c(5.47, 4.25, 3.72),
    col = c(col_ind, col_ind, col_dir)
  )
  ang <- with(arrows, atan2(yend - y, xend - x) * 180 / pi)
  path_lab <- data.frame(
    lab = c("path a", "path b", "path c′"),
    x = c(2.45, 5.65, 3.95), y = c(5.12, 5.12, 3.33), angle = c(ang[1:2], 0)
  )

  ## Effects; colored acronyms via a black layer + colored layer (plotmath phantom)
  eff <- data.frame(
    lab_black = c('phantom("ACME")*" "*"(a×b) ="', 'phantom("ADE")*" "*"(c′) ="', '"total effect ="'),
    lab_col   = c('"ACME"*" "*phantom("(a×b) =")', '"ADE"*" "*phantom("(c′) =")', 'phantom("")'),
    col = c(col_ind, col_dir, "black"),
    val = c(paste0(fmt(res$d0), p_stars(res$d0.p)),
            paste0(fmt(res$z0), p_stars(res$z0.p)),
            paste0(fmt(res$tau.coef), p_stars(res$tau.p))),
    y = c(2.50, 1.85, 1.20)
  )

  ## Plot
  ggplot() +
    geom_segment(data = arrows, aes(x, y, xend = xend, yend = yend), colour = arrows$col,
                 linewidth = 1.1, arrow = arrow(length = unit(0.13, "in"), type = "closed"),
                 linejoin = "mitre") +
    geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "white", colour = "black", linewidth = 0.6) +
    geom_text(data = boxes, aes((xmin + xmax) / 2, (ymin + ymax) / 2, label = lab), size = 5.6) +
    geom_text(data = path_lab, aes(x, y, label = lab, angle = angle), size = 5, fontface = "italic") +
    geom_text(data = eff, aes(3.75, y, label = lab_black), parse = TRUE, hjust = 1, size = 4.9) +
    geom_text(data = eff, aes(3.75, y, label = lab_col), parse = TRUE, hjust = 1, size = 4.9,
              colour = eff$col) +
    geom_text(data = eff, aes(3.95, y, label = val), hjust = 0, size = 4.9) +
    annotate("text", x = 3.90, y = 0.57, size = 4.9,
             label = sprintf("Magnitude of mediation %.1f%%", 100 * res$n0)) +
    annotate("text", x = 0.30, y = 7.15, label = tag, hjust = 0, size = 7.4, fontface = "bold") +
    annotate("text", x = 0.95, y = 7.15, label = title, hjust = 0, size = 5.6) +
    coord_fixed(xlim = c(0.3, 7.55), ylim = c(0.3, 7.55), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(4, 8, 4, 8), plot.background = element_rect(fill = "white", colour = NA))
}

## Composition: A (non-vulnerable) + B (vulnerable)
fig4 <- wrap_plots(
  mediation_panel(res_low,  "A", "Group 0 (Non-Vulnerable)"),
  mediation_panel(res_high, "B", "Group 1 (Vulnerable)"),
  ncol = 2
)

## Print and save figure
print(fig4)
ggsave("figure4.svg", fig4, width = 9.5, height = 4.5, dpi = 600, bg = "white")
