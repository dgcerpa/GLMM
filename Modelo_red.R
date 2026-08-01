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

MAIN_PATH <- "/home/ax/diego-uai/"
setwd(MAIN_PATH)

# Data preprocessing
library("mgm")
library("ggplot2")
library("huge")
library('bootnet')
library('NetworkComparisonTest')
library('corpcor')

library('psychonetrics')
library('qgraph')
library('dplyr')
library('psych')
library('lavaan')
library('skimr')
library('MASS')
conflicted::conflicts_prefer(dplyr::rename, dplyr::select, dplyr::filter, lmerTest::lmer)

plot_nets <- function(Net1, Net2) {par(mfrow=c(1,2)); plot(Net1); plot(Net2)}

summaryNCT <- function(NCT_res) {
  cat("NETWORK INVARIANCE TEST\n", NCT_res$nwinv.pval, "\nGLOBAL STRENGTH INVARIANCE TEST\n", NCT_res$glstrinv.pval, "\nCENTRALITY INVARIANCE TEST\n")
  NCT_res$diffcen.pval
}

residualize <- function(data, target_vars, covars, group) {
  result <- data
  groups <- unique(data[[group]])
  for (g in groups) {
    idx <- data[[group]] == g
    for (v in target_vars) {
      formula <- as.formula(paste(v, "~", paste(covars, collapse = "+")))
      result[idx, v] <- residuals(lm(formula, data = data[idx, ]))
    }
  }
  result
}

group_var <- "group"
covariates <- c()
# covariates <- c(covariates, "diff_reward", "AIM_num", "NASA_diff")
# net_vars <- c(net_vars, 'MAIA_ConcienciaEmocional_DIRd', 'MAIA_EscuchaCuerpo_DIRd', 'MAIA_Confianza_DIRd')

covariates <- c(covariates, "diff_reward")
covariates <- c(covariates, "AIM_num")
covariates <- c(covariates, "diff_success")
covariates <- c(covariates, "Fatigue_pre_7")
covariates <- c(covariates, "ASSIST_DIRt")
# covariates <- c(covariates, "NASA_diff")
covariates <- c(covariates, "IFS_Total_DIRd")
covariates <- c(covariates, "DASS21_depresion_DIRd")


net_vars <- c('diff_effort')
net_vars <- c(net_vars, 'MAIA_ConcienciaEmocional_DIRd')
net_vars <- c(net_vars, 'MAIA_EscuchaCuerpo_DIRd')
net_vars <- c(net_vars, 'MAIA_Confianza_DIRd')
net_vars <- c(net_vars, 'MAIA_Percibir_DIRd')
net_vars <- c(net_vars, 'MAIA_Autorregulacion_DIRd')
net_vars <- c(net_vars, 'MAIA_AusenciaDistraccion_DIRd')
net_vars <- c(net_vars, 'MAIA_AusenciaPreocupacion_DIRd')

# net_vars <- c(net_vars, 'IRI_DIRt')
# net_vars <- c(net_vars, 'SASS_DIRt')
# net_vars <- c(net_vars, 'SASS_DIRt')
# net_vars <- c(net_vars, 'IRI_PreocupacionEmpatica_DIRd')
# net_vars <- c(net_vars, 'IRI_TomaPerspectiva_DIRd')
# net_vars <- c(net_vars, 'IRI_TomaPerspectiva_DIRd')


vars <- c(covariates, net_vars)
data <- read.csv("data/dataset_full.csv", header = T) %>% rename(!!sym(group_var) := grupo) 
# data <- data %>% mutate(diff_success = abs(tasa_fallo_self - tasa_fallo_other)) 
# data <- data %>% mutate(diff_success = tasa_fallo_self - tasa_fallo_other) 
data <- data %>% mutate(diff_success = tasa_fallo_other - tasa_fallo_self) 
data <- data %>% dplyr::select(group, dplyr::all_of(vars)) %>% dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>% na.omit() %>% select(all_of(c(vars, group_var)))
data$group <- as.factor(data$group)

# --- RESIDUALIZATION ---
data <- residualize(data, net_vars, covariates, group_var)

vars <- paste0("x", 1:length(net_vars))
df <- data %>% select(-all_of(covariates)) %>% setNames(c(vars, group_var)) %>% mutate(group = factor(group, levels = c("0","1"), labels = c("Control","Vulnerable")))

dat0 <- df %>% filter(group == "Control") %>% select(starts_with("x"))
dat1 <- df %>% filter(group == "Vulnerable") %>% select(starts_with("x"))

gamma <- 0.75
gamma <- 0.8
threshold <- TRUE
# threshold <- FALSE
lambda.min = 0.1
Net1 <- qgraph(cor_auto(dat1, forcePD = TRUE), graph="glasso", sampleSize=nrow(dat1), layout="circle", lambda.min.ratio=lambda.min, gamma=gamma, threshold=threshold, tuning=gamma, cut=0, palette="colorblind", theme="Hollywood", esize=10, vsize=9.5, nodeNames=vars, legend=F, legend.cex=1.975, layoutScale=c(1,1), layoutOffset=c(0,0), GLratio=1.25, title="Vulnerable", doPlot=T)

Net0 <- qgraph(cor_auto(dat0, forcePD = TRUE), graph="glasso", sampleSize=nrow(dat0), layout="circle", lambda.min.ratio=lambda.min, gamma=gamma, threshold=threshold, tuning=gamma, cut=0, palette="colorblind", theme="Hollywood", esize=10, vsize=9.5, nodeNames=vars, legend=F, legend.cex=1.975, layoutScale=c(1,1), layoutOffset=c(0,0), GLratio=1.25, title="Control", doPlot=T)
plot_nets(Net0, Net1)


Network1 <- bootnet::estimateNetwork(dat1, default = "EBICglasso",  threshold = threshold, lambda.min.ratio=lambda.min, tuning=gamma, corMethod = "cor_auto")

Network0 <- bootnet::estimateNetwork(dat0, default = "EBICglasso",  threshold = threshold, lambda.min.ratio=lambda.min, tuning=gamma, corMethod = "cor_auto")

nodes01 <- intersect(Network0$labels, Network1$labels)
nodes <- "all"
centrality <- c("betweenness", "strength")
padj <- "fdr"
# padj <- "BH"
# padj <- "none"
it = 300
NCT_res <- NetworkComparisonTest::NCT(Network0, Network1, gamma = gamma, it=it, test.edges = F, edges = "all", test.centrality = T, centrality=centrality, nodes=nodes, p.adjust.methods = padj, make.positive.definite = TRUE)
clear()
summaryNCT(NCT_res)
