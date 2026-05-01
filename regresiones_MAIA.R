

## Regresiones múltiples
## diff_effort y effort_other como VD y la interocepción (cuestionario MAIA) como VI
##
## Modelos
##   M1 = MAIA total
##   M2 = 8 subescalas MAIA


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
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo != "0")


# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "MAIA_DIRt",
                 "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
                 "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
                 "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd", 
                 "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd",
                 "SASS_DIRt")

df_mod <- df %>%
  select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))



###################################
## Modelo completo (diff_effort) sin interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd + MAIA_AusenciaDistraccion_DIRd +
           MAIA_AusenciaPreocupacion_DIRd + MAIA_RegulacionAtencion_DIRd +
           MAIA_ConcienciaEmocional_DIRd + MAIA_Autorregulacion_DIRd +
           MAIA_EscuchaCuerpo_DIRd + MAIA_Confianza_DIRd,
         data = df_mod)
print(summary(m2))













m1 <- lm(diff_effort ~ MAIA_DIRt * grupo + IRI_Cognitivo * grupo +
           IRI_Afectivo * grupo, data = df_mod)
print(summary(m1))




m1 <- lm(diff_effort ~ MAIA_DIRt + SASS_DIRt + IRI_Afectivo + IRI_Cognitivo, data = df_mod)
print(summary(m1))



df_mod_exp <- subset(df_mod, grupo != "0")

m2 <- lm(diff_effort ~ MAIA_DIRt + SASS_DIRt + IRI_Afectivo + IRI_Cognitivo, data = df_mod_exp)
print(summary(m2))



df_mod_ctrl <- subset(df_mod, grupo != "1")

m3 <- lm(diff_effort ~ MAIA_DIRt + SASS_DIRt + IRI_Afectivo + IRI_Cognitivo, data = df_mod_ctrl)
print(summary(m3))








###################################
## Modelo completo (diff_effort) con interacción de grupo

## Modelo 1: MAIA total
m1 <- lm(diff_effort ~ MAIA_DIRt * grupo, data = df_mod)
print(summary(m1))


plot(ggpredict(m1, terms = c("MAIA_DIRt", "grupo"))) +
  ggtitle("Modelo 4") +
  ylab("Diff Effort") +
  xlab("MAIA Total") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()


## Modelo 2: 8 subescalas MAIA
m2 <- lm(diff_effort ~ MAIA_Percibir_DIRd * grupo + MAIA_AusenciaDistraccion_DIRd * grupo +
           MAIA_AusenciaPreocupacion_DIRd * grupo + MAIA_RegulacionAtencion_DIRd * grupo +
           MAIA_ConcienciaEmocional_DIRd * grupo + MAIA_Autorregulacion_DIRd * grupo +
           MAIA_EscuchaCuerpo_DIRd * grupo + MAIA_Confianza_DIRd * grupo,
         data = df_mod)
print(summary(m2))




###################################
## Modelo completo (effort_other) sin interacción de grupo

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
## Modelo completo (effort_other) con interacción de grupo

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


