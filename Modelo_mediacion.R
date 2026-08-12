

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

MAIN_PATH <- "/home/ax/esd-lab/diego-uai/"
setwd(MAIN_PATH)

# Data preprocessing
library("ggplot2")
library('corpcor')
library('dplyr')
library('psych')
library('lavaan')
library('skimr')
library('MASS')
library("boot")
library("mediation")
library("tidyr")
library("purrr")
library("DiagrammeR")
library("semPlot")
library("patchwork")

conflicted::conflicts_prefer(dplyr::rename, dplyr::select, dplyr::filter, lmerTest::lmer)

group_var <- "group"
main_vars <- c('diff_effort', "SASS_DIRt", "MAIA_DIRt", "Fatigue_diff")

vars <- main_vars
data <- read.csv("data/data-all-slopes.csv", header = T) %>% rename(!!sym(group_var) := grupo)
data$diff_effort <- data$diff_effort1
# data$diff_effort <- data$diff_effort2
data <- data %>% dplyr::select(group, dplyr::all_of(vars)) %>% dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>% na.omit() %>% select(all_of(c(vars, group_var)))


data$X <- data$MAIA_DIRt
data$M <- data$diff_effort
data$Y <- data$SASS_DIRt
data$C <- data$Fatigue_diff

data$G <- factor(data$group)

data_low <- subset(data, G=="0")
data_high <- subset(data, G=="1")

med_low <- lm(M ~ X + C, data=data_low)
out_low <- lm(Y ~ X + M + C, data=data_low)
med_high <- lm(M ~ X + C, data=data_high)
out_high <- lm(Y ~ X + M + C, data=data_high)


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

p <- patchwork::wrap_plots(
  list(p1,p2,p3),
  ncol=3,
  guides="collect"
)

p <- p&theme(legend.position="bottom")

p

# dir_all <- lm(Y ~ X + C, data=data)
# summary(dir_all)

# set.seed(1)

nboot <- 5000
# nboot <- 10000
# nboot <- 1000
# nboot <- 3000
res_low <- mediation::mediate(med_low, out_low, treat="X", mediator="M", boot=TRUE, sims=nboot)
res_high <- mediation::mediate(med_high, out_high, treat="X", mediator="M", boot=TRUE, sims=nboot)



lab_a <- sprintf("a\nG0=%.3f\nG1=%.3f",
                 coef(med_low)["X"],
                 coef(med_high)["X"])

lab_b <- sprintf("b\nG0=%.3f\nG1=%.3f",
                 coef(out_low)["M"],
                 coef(out_high)["M"])

lab_cp <- sprintf("c_prime\nG0=%.3f\nG1=%.3f",
                  coef(out_low)["X"],
                  coef(out_high)["X"])


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

clear()
summary(res_low)
summary(res_high)






data$G_num <- as.integer(as.character(data$G))
set.seed(1)
med_mod <- lm(M ~ X*G_num + C*G_num,data=data)
out_mod <- lm(Y ~ X*G_num + M*G_num + C*G_num,data=data)

fit_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

control_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",covariates=list(G_num=0),control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

vulnerable_mod <- mediation::mediate(med_mod,out_mod,treat="X",mediator="M",covariates=list(G_num=1),control.value=0,treat.value=1,boot=TRUE,boot.ci.type="perc",sims=nboot)

group_difference <- mediation::test.modmed(fit_mod,covariates.1=list(G_num=0),covariates.2=list(G_num=1),sims=nboot)

summary(control_mod)
summary(vulnerable_mod)
group_difference
summary(med_mod)
summary(out_mod)









