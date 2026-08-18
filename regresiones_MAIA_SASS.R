

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(car)        # vif()
library(broom)      # tidy() / glance()
library(performance) # check_model() opcional
library(ggplot2)
library(ggeffects)
library(emmeans)



#########################
## Data

# Cargar datos
df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo == "1")


# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "MAIA_DIRt", "SASS_DIRt",
                 "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
                 "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
                 "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd", 
                 "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd", "Fatigue_diff")

df_mod <- df %>%
  dplyr::select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))




###################################
## Modelo diff_effort sin interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd + MAIA_AusenciaDistraccion_DIRd +
           MAIA_AusenciaPreocupacion_DIRd + MAIA_RegulacionAtencion_DIRd +
           MAIA_ConcienciaEmocional_DIRd + MAIA_Autorregulacion_DIRd +
           MAIA_EscuchaCuerpo_DIRd + MAIA_Confianza_DIRd + Fatigue_diff,
         data = df_mod)
print(summary(m2))



###################################
## Modelo diff_effort con interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff, data = df_mod)
print(summary(m1))

trends_maia <- emtrends(m1, ~ grupo, var = "MAIA_DIRt", at = list(grupo = c(0, 1))) # Post-hoc: slopes de MAIA por grupo
summary(trends_maia, infer = c(TRUE, TRUE)) # Slopes por grupo con IC 95% y test contra 0
pairs(trends_maia) # Contraste entre grupos (equivale al término de interacción de m1)
car::Anova(m1, type = "II")


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)
print(summary(m2))


## interaccion de 5 sub escalas (agrupar sub escalas)



#################################
## Modelos SASS

## Modelo 3: SASS total (diff_effort) sin interacción de grupo
m3 <- lm(diff_effort ~ SASS_DIRt, data = df_mod)
print(summary(m3))


## Modelo 4: SASS total (diff_effort) con interacción de grupo
m4 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m4))


trends_sass <- emtrends(m4, ~ grupo, var = "SASS_DIRt", at = list(grupo = c(0, 1))) # Post-hoc: slopes de SASS por grupo
summary(trends_sass, infer = c(TRUE, TRUE)) # Slopes por grupo con IC 95% y test contra 0
pairs(trends_sass) # Contraste entre grupos (equivale al término de interacción)
Anova(m4, type = "II") # Efectos principales correctos (SS tipo II ignoran las interacciones al testear los términos de menor orden)



