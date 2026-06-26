

## GLMM: Modelo mixto generalizado
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(lme4)       # glmer()
library(ggeffects)  # ggpredict()
library(emmeans)    # emtrends(), pairs()



###################################
## Importar datos

alldata.sc <- read.csv("Datos/datos_long_glmm_filtrados.csv", header = T)
df <- read_csv("dataset_full_final.csv")

##
alldata.sc <- left_join(alldata.sc, 
                        df %>% dplyr::select(sub, DASS21_depresion_DIRd, MAIA_DIRt, SASS_DIRt, Fatigue_diff), 
                        by = "sub")

## Excluir trials omitidos y eliminar columna índice
alldata.sc <- subset(alldata.sc, decision!= 2)
alldata.sc <- subset(alldata.sc, select = -c(X))




###################################
### Modelos con DASS21 y MAIA ####

m_dass <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + Fatigue_diff + (1 + c.effort + c.reward|sub),
                data = alldata.sc, family = binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m_diff <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df)


summary(m_dass)
summary(m_diff)





## Correlaciones parciales SASS ~ diff_effort por grupo, controlando por DASS21 (ajuste FDR)
library(ppcor)
cor_ctrl <- with(subset(df, grupo == 0), pcor.test(SASS_DIRt, diff_effort, Fatigue_diff))
cor_exp  <- with(subset(df, grupo == 1), pcor.test(SASS_DIRt, diff_effort, Fatigue_diff))
p_fdr <- p.adjust(c(cor_ctrl$p.value, cor_exp$p.value), method = "fdr")
data.frame(grupo = c("Control", "Experimental"), 
           r = c(cor_ctrl$estimate, cor_exp$estimate), 
           p = c(cor_ctrl$p.value, cor_exp$p.value), 
           p_fdr = p_fdr)




###################################
# correlaciones parciales SASS ~ diff_effort "controlando por el grupo opuesto"

# Correlación parcial en dataset completo con `grupo` como covariable
# Una sola pcor.test sobre todo df, partialeando `grupo` (y luego `grupo` + DASS).
# Da SOLO 2 valores únicos: B1=B2 y B3=B4 (la dummy "control" y "exp" son la misma variable invertida).
B1 <- with(df, pcor.test(SASS_DIRt, diff_effort, grupo))
B4 <- with(df, pcor.test(SASS_DIRt, diff_effort, df[, c("grupo", "Fatigue_diff")]))
p_B <- p.adjust(c(B1$p.value, B4$p.value), method = "fdr")
data.frame(version = c("grupo", "grupo+fatigue"),
           r = c(B1$estimate, B4$estimate),
           p = c(B1$p.value, B4$p.value), p_fdr = p_B)





###################################
## Residualización para GLMM

## Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "MAIA_DIRt",
                 "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
                 "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
                 "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd", 
                 "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd", "Fatigue_diff")

df_mod <- df %>%
  dplyr::select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))


## Función
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


## Variables y covariables

group_var <- "grupo"
covariates <- c()

covariates <- c(covariates, "Fatigue_diff")

main_vars <- c('diff_effort')
main_vars <- c(main_vars, 'MAIA_ConcienciaEmocional_DIRd')
main_vars <- c(main_vars, 'MAIA_EscuchaCuerpo_DIRd')
main_vars <- c(main_vars, 'MAIA_Confianza_DIRd')
main_vars <- c(main_vars, 'MAIA_Autorregulacion_DIRd')
main_vars <- c(main_vars, 'MAIA_AusenciaDistraccion_DIRd')
main_vars <- c(main_vars, 'MAIA_AusenciaPreocupacion_DIRd')
main_vars <- c(main_vars, 'MAIA_Percibir_DIRd')
main_vars <- c(main_vars, 'MAIA_RegulacionAtencion_DIRd')

vars <- c(covariates, main_vars)


## Acotar df
df_mod <- df_mod %>% 
  dplyr::select(grupo, dplyr::all_of(vars)) %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), as.numeric)) %>% 
  na.omit() %>% 
  dplyr::select(all_of(c(vars, group_var)))

df_mod$grupo <- as.factor(df_mod$grupo)


## Resifualizar por covariables
df_mod <- residualize(df_mod, main_vars, covariates, group_var)


## Modelo subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)

print(summary(m2))





