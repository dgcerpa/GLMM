

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(car)        # vif()
library(broom)      # tidy() / glance()
library(performance) # check_model() opcional
library(ggplot2)
library(ggeffects)


#########################
## Data

# Cargar datos
df <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo == "1")


# Subset analítico (casos completos en todas las variables usadas)
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
m1 <- lm(diff_effort ~ MAIA_DIRt * grupo, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)
print(summary(m2))




###################################
## Modelo effort_other sin interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(effort_other ~ MAIA_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(effort_other ~ MAIA_Percibir_DIRd + MAIA_AusenciaDistraccion_DIRd +
           MAIA_AusenciaPreocupacion_DIRd + MAIA_RegulacionAtencion_DIRd +
           MAIA_ConcienciaEmocional_DIRd + MAIA_Autorregulacion_DIRd +
           MAIA_EscuchaCuerpo_DIRd + MAIA_Confianza_DIRd,
         data = df_mod)
print(summary(m2))




###################################
## Modelo effort_other con interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(effort_other ~ MAIA_DIRt * grupo, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(effort_other ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)
print(summary(m2))




