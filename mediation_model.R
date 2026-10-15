
# Moderated causal mediation: MAIA -> prosocial effort -> SASS, by group


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)

## Reset session: clear workspace, close graphics devices and set options
suppressMessages({
  rm(list=ls());
  if(!is.null(
    dev.list())
  ) dev.off();
  graphics.off();
  options(stringsAsFactors = F,
          "scipen" = 100,
          "digits" = 4,
          warn = 0);
  clear <- function() cat("\014\n");
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


###############################################################################
# Import Data

## Variables of interest
group_var <- "group"
main_vars <- c('diff_effort', "SASS_DIRt", "MAIA_DIRt", "Fatigue_diff")
vars <- main_vars

## Load data (grupo renamed to group)
data <- read.csv("dataset_final.csv", header = T) %>% rename(!!sym(group_var) := grupo)
# data$diff_effort <- data$diff_effort1
# data$diff_effort <- data$diff_effort2

## Keep variables of interest and drop incomplete cases
data <- data %>% dplyr::select(group, dplyr::all_of(vars)) %>% dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>% na.omit() %>% select(all_of(c(vars, group_var)))


###############################################################################
# Mediation variables

## X = MAIA, M = prosocial effort, Y = SASS, C = dFatigue (covariate)
data$X <- data$MAIA_DIRt
data$M <- data$diff_effort
data$Y <- data$SASS_DIRt
data$C <- data$Fatigue_diff

## Group factor
data$G <- factor(data$group, levels = c(0, 1), labels = c("Non-vulnerable", "Vulnerable"))

## Split data by group
data_low  <- subset(data, G == "Non-vulnerable")
data_high <- subset(data, G == "Vulnerable")


###############################################################################
# Mediator and outcome models by group

## Mediator (M ~ X + C) and outcome (Y ~ X + M + C) models
med_low <- lm(M ~ X + C, data=data_low)
out_low <- lm(Y ~ X + M + C, data=data_low)
med_high <- lm(M ~ X + C, data=data_high)
out_high <- lm(Y ~ X + M + C, data=data_high)


###############################################################################
# Exploratory plots of the mediation paths

## X -> M
p1 <- ggplot(data,aes(X,M,color=G))+
  geom_point(alpha=.7)+
  geom_smooth(method="lm",se=TRUE)+
  theme_classic(base_size=13)+
  labs(
    x="MAIA",
    y="diff_effort",
    color="Group",
    title="X \u2192 M"
  )

## M -> Y
p2 <- ggplot(data,aes(M,Y,color=G))+
  geom_point(alpha=.7)+
  geom_smooth(method="lm",se=TRUE)+
  theme_classic(base_size=13)+
  labs(
    x="diff_effort",
    y="SASS",
    color="Group",
    title="M \u2192 Y"
  )

## X -> Y
p3 <- ggplot(data,aes(X,Y,color=G))+
  geom_point(alpha=.7)+
  geom_smooth(method="lm",se=TRUE)+
  theme_classic(base_size=13)+
  labs(
    x="MAIA",
    y="SASS",
    color="Group",
    title="X \u2192 Y"
  )

## Composition of the three panels
p <- patchwork::wrap_plots(
  list(p1,p2,p3),
  ncol=3,
  guides="collect"
)

p <- p&theme(legend.position="bottom")

p


###############################################################################
# Bootstrap mediation by group

## Total effect in the full sample (optional)
# dir_all <- lm(Y ~ X + C, data=data)
# summary(dir_all)

## Number of bootstrap resamples
# set.seed(1)
nboot <- 5000
# nboot <- 10000
# nboot <- 1000
# nboot <- 3000

## ACME, ADE, total effect and proportion mediated (nonparametric bootstrap)
res_low <- mediation::mediate(med_low, out_low, treat="X", mediator="M", boot=TRUE, sims=nboot)
res_high <- mediation::mediate(med_high, out_high, treat="X", mediator="M", boot=TRUE, sims=nboot)


###############################################################################
# Path diagram

## Path coefficients (a, b, c') by group
lab_a  <- sprintf("a\nNon-vuln=%.3f\nVuln=%.3f", coef(med_low)["X"],  coef(med_high)["X"])
lab_b  <- sprintf("b\nNon-vuln=%.3f\nVuln=%.3f", coef(out_low)["M"],  coef(out_high)["M"])
lab_cp <- sprintf("c_prime\nNon-vuln=%.3f\nVuln=%.3f", coef(out_low)["X"], coef(out_high)["X"])

## Diagram (Graphviz)
g <- grViz(sprintf('
digraph mediation {

graph [
  layout=neato
  overlap=false
  splines=true
]

node [
  shape=box
  style="rounded,filled"
  fontname="Helvetica"
]

X [
  label="X\"
  pos="0,0!"
  fillcolor="#d9ead3"
]

M [
  label="M\"
  pos="3,1.5!"
  fillcolor="#cfe2f3"
]

Y [
  label="Y\"
  pos="6,0!"
  fillcolor="#fff2cc"
]

X -> M [
  label="%s"
]

M -> Y [
  label="%s"
]

X -> Y [
  label="%s"
]

}
',lab_a,lab_b,lab_cp))

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
med_mod <- lm(M ~ X*G_num + C*G_num,data=data)
out_mod <- lm(Y ~ X*G_num + M*G_num + C*G_num,data=data)

## Moderated mediation model (input for test.modmed)
fit_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

## Conditional effects: non-vulnerable group
control_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",covariates=list(G_num=0),control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

## Conditional effects: vulnerable group
vulnerable_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",covariates=list(G_num=1),control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

## Between-group difference in ACME and ADE
group_difference <- mediation::test.modmed(fit_mod,covariates.1=list(G_num=0),covariates.2=list(G_num=1),sims=nboot)

## Print results
summary(control_mod)
summary(vulnerable_mod)
group_difference
summary(med_mod)
summary(out_mod)
