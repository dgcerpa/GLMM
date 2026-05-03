

# Robust LMM: k ~ Agent * Grupo + (1|subject_id)
# Modelos: parabólico e hiperbólico
# Diego Garrido Cerpa - Viña del Mar 2026


# Librerías

library(tidyverse)
library(readxl)
library(robustlmm)
library(sfsmisc)
library(lmtest)
library(MASS)

library(nparLD)
library(lmerTest)

library(conflicted)

conflicts_prefer(dplyr::select, dplyr::filter)
conflicts_prefer(lmerTest::lmer)



# Helper: extraer coeficientes + p-values desde t-values
pvals_from_t <- function(model) {
  cc <- coef(summary(model))
  if (inherits(model, "rlmerMod")) {
    df_res <- nobs(model) - length(fixef(model))
  } else if (inherits(model, "rlm")) {
    df_res <- model$converged  # won't work — use this instead:
    df_res <- length(model$residuals) - length(model$coefficients)
  } else {
    df_res <- model$df.residual
  }
  p <- 2 * pt(abs(cc[, "t value"]), df = df_res, lower.tail = FALSE)
  out <- cbind(cc, p.value = p)
  print(round(out, 4))
}




###############################
## Modelo Antiguo

# Datos
df <- read_excel("Datos/params_2k1b_all_families.xlsx") %>%
  mutate(grupo = factor(grupo, levels = c(0, 1), labels = c("Control", "Experimental")),
         subject_id = factor(subject_id))

# Formato largo: parabólico
df_p <- df %>%
  select(subject_id, grupo, k_self = p_2k1b_k_self, k_other = p_2k1b_k_other) %>%
  pivot_longer(c(k_self, k_other), names_to = "Agent", values_to = "k") %>%
  mutate(Agent = factor(ifelse(Agent == "k_self", 0, 1)))

# Formato largo: hiperbólico
df_h <- df %>%
  select(subject_id, grupo, k_self = h_2k1b_k_self, k_other = h_2k1b_k_other) %>%
  pivot_longer(c(k_self, k_other), names_to = "Agent", values_to = "k") %>%
  mutate(Agent = factor(ifelse(Agent == "k_self", 0, 1)))



###############################
## Modelo parabólico

# Robust Linear Mixed-Effects Regression
fit_p <- rlmer(k ~ Agent * grupo + (1 | subject_id), data = df_p)
summary(fit_p)
pvals_from_t(fit_p)



# Robust Linear Mixed-Effects Regression (sin random effects)
rlm_p <- MASS::rlm(k ~ Agent * grupo, data = df_p, maxit = 100)
summary(rlm_p)
pvals_from_t(rlm_p)


# ANOVA de medidas repetidas
lmm_p <- lmer(k ~ Agent * grupo + (1 | subject_id), data = df_p)
anova(lmm_p, type = 3)


# ANOVA No paramétrico
npar_p <- f1.ld.f1(y = df_p$k, time = df_p$Agent, group = df_p$grupo,
                   subject = df_p$subject_id, time.name = "Agent",
                   group.name = "Grupo", description = FALSE, plot.RTE = FALSE)
print(round(npar_p$ANOVA.test, 4))



###############################
## Modelo hiperbólico
# Robust Linear Mixed-Effects Regression
fit_h <- rlmer(k ~ Agent * grupo + (1 | subject_id), data = df_h)
summary(fit_h)
pvals_from_t(fit_h)


# Robust Linear Mixed-Effects Regression (sin random effects)
rlm_h <- MASS::rlm(k ~ Agent * grupo, data = df_h, maxit = 100)
summary(rlm_h)
pvals_from_t(rlm_h)


# ANOVA de medidas repetidas
lmm_h <- lmer(k ~ Agent * grupo + (1 | subject_id), data = df_h)
anova(lmm_h, type = 3)


# ANOVA No paramétrico
npar_h <- f1.ld.f1(y = df_h$k, time = df_h$Agent, group = df_h$grupo,
                   subject = df_h$subject_id, time.name = "Agent",
                   group.name = "Grupo", description = FALSE, plot.RTE = FALSE)
print(round(npar_h$ANOVA.test, 4))





###############################
# Modelo Nuevo

# Datos
df_new <- read.csv("Datos/2k1b_modelos.csv") %>%
  mutate(grupo = factor(intervention, levels = c(0, 1), labels = c("Control", "Experimental")),
         subject_id = factor(ui))

# Formato largo: parabólico
df_p_new <- df_new %>%
  select(subject_id, grupo, k_self = p_self_k, k_other = p_other_k) %>%
  pivot_longer(c(k_self, k_other), names_to = "Agent", values_to = "k") %>%
  mutate(Agent = factor(ifelse(Agent == "k_self", 0, 1)))


# Formato largo: hiperbólico
df_h_new <- df_new %>%
  select(subject_id, grupo, k_self = h_self_k, k_other = h_other_k) %>%
  pivot_longer(c(k_self, k_other), names_to = "Agent", values_to = "k") %>%
  mutate(Agent = factor(ifelse(Agent == "k_self", 0, 1)))



###############################
#W Modelo parabólico

# Robust Linear Mixed-Effects Regression
fit_p_new <- rlmer(k ~ Agent * grupo + (1 | subject_id), data = df_p_new)
summary(fit_p_new)
pvals_from_t(fit_p_new)


# Robust Linear Mixed-Effects Regression (sin random effects)
rlm_p_new <- MASS::rlm(k ~ Agent * grupo, data = df_p_new, maxit = 100)
summary(rlm_p_new)
pvals_from_t(rlm_p_new)


# ANOVA de medidas repetidas
lmm_p_new <- lmer(k ~ Agent * grupo + (1 | subject_id), data = df_p_new)
anova(lmm_p_new, type = 3)


# ANOVA No paramétrico
npar_p_new <- f1.ld.f1(y = df_p_new$k, time = df_p_new$Agent, group = df_p_new$grupo,
                   subject = df_p_new$subject_id, time.name = "Agent",
                   group.name = "Grupo", description = FALSE, plot.RTE = FALSE)
print(round(npar_p_new$ANOVA.test, 4))



###############################
## Modelo hiperbólico

# Robust Linear Mixed-Effects Regression
fit_h_new <- rlmer(k ~ Agent * grupo + (1 | subject_id), data = df_h_new)
summary(fit_h_new)
pvals_from_t(fit_h_new)


# Robust Linear Mixed-Effects Regression (sin random effects)
rlm_h_new <- MASS::rlm(k ~ Agent * grupo, data = df_h_new, maxit = 100)
summary(rlm_h_new)
pvals_from_t(rlm_h_new)


# ANOVA de medidas repetidas
lmm_h_new <- lmer(k ~ Agent * grupo + (1 | subject_id), data = df_h_new)
anova(lmm_h_new, type = 3)


# ANOVA No paramétrico
npar_h_new <- f1.ld.f1(y = df_h_new$k, time = df_h_new$Agent, group = df_h_new$grupo,
                   subject = df_h_new$subject_id, time.name = "Agent",
                   group.name = "Grupo", description = FALSE, plot.RTE = FALSE)
print(round(npar_h_new$ANOVA.test, 4))




