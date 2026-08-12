

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(car)
library(broom)
library(performance)
library(ggplot2)
library(ggeffects)
library(emmeans)


#########################
## Data

# Cargar datos
df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo != "1")

# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "SASS_DIRt",
                 "IFS_Total_DIRd",
                 "IFS_SeriesMotoras_DIRd", "IFS_InstruccionesConflictivas_DIRd",
                 "IFS_ControlInhibitorioMotor_DIRd", "IFS_RepeticionDigitosAtras_DIRd",
                 "IFS_MesesAtras_DIRd", "IFS_MemoriaTrabajoVisual_DIRd", 
                 "IFS_Refranes_DIRd", "IFS_ControlInhibitorioVerbal_DIRd")

df_mod <- df %>%
  select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))



###################################
## Modelo diff_effort sin interacción de grupo

## Modelo 1: IFS total
m1 <- lm(diff_effort ~ IFS_Total_DIRd, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas IFS
m2 <- lm(diff_effort ~ IFS_SeriesMotoras_DIRd + IFS_InstruccionesConflictivas_DIRd +
           IFS_ControlInhibitorioMotor_DIRd + IFS_RepeticionDigitosAtras_DIRd +
           IFS_MesesAtras_DIRd + IFS_MemoriaTrabajoVisual_DIRd +
           IFS_Refranes_DIRd + IFS_ControlInhibitorioVerbal_DIRd,
         data = df_mod)
print(summary(m2))




###################################
## Modelo diff_effort con interacción de grupo

## Modelo 1: IFS total
m1 <- lm(diff_effort ~ IFS_Total_DIRd * grupo, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas IFS
m2 <- lm(diff_effort ~ IFS_SeriesMotoras_DIRd * grupo + IFS_InstruccionesConflictivas_DIRd * grupo +
           IFS_ControlInhibitorioMotor_DIRd * grupo + IFS_RepeticionDigitosAtras_DIRd * grupo +
           IFS_MesesAtras_DIRd * grupo + IFS_MemoriaTrabajoVisual_DIRd * grupo +
           IFS_Refranes_DIRd * grupo + IFS_ControlInhibitorioVerbal_DIRd * grupo,
         data = df_mod)
print(summary(m2))




###################################
## Modelo effort_other sin interacción de grupo

## Modelo 1: IFS total
m1 <- lm(effort_other ~ IFS_Total_DIRd, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas IFS
m2 <- lm(effort_other ~ IFS_SeriesMotoras_DIRd + IFS_InstruccionesConflictivas_DIRd +
           IFS_ControlInhibitorioMotor_DIRd + IFS_RepeticionDigitosAtras_DIRd +
           IFS_MesesAtras_DIRd + IFS_MemoriaTrabajoVisual_DIRd +
           IFS_Refranes_DIRd + IFS_ControlInhibitorioVerbal_DIRd,
         data = df_mod)
print(summary(m2))




###################################
## Modelo effort_other con interacción de grupo

## Modelo 1: IFS total
m1 <- lm(effort_other ~ IFS_Total_DIRd * grupo, data = df_mod)
print(summary(m1))


## Modelo 2: 8 subescalas IFS
m2 <- lm(effort_other ~ IFS_SeriesMotoras_DIRd * grupo + IFS_InstruccionesConflictivas_DIRd * grupo +
           IFS_ControlInhibitorioMotor_DIRd * grupo + IFS_RepeticionDigitosAtras_DIRd * grupo +
           IFS_MesesAtras_DIRd * grupo + IFS_MemoriaTrabajoVisual_DIRd * grupo +
           IFS_Refranes_DIRd * grupo + IFS_ControlInhibitorioVerbal_DIRd * grupo,
         data = df_mod)
print(summary(m2))




#################################
## Modelos SASS

## Modelo 3: SASS total (diff_effort) sin interacción de grupo
m3 <- lm(diff_effort ~ SASS_DIRt, data = df_mod)
print(summary(m3))


## Modelo 4: SASS total (diff_effort) con interacción de grupo
m4 <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m4))


## Modelo 3: SASS total (effort_other) sin interacción de grupo
m3 <- lm(effort_other ~ SASS_DIRt, data = df_mod)
print(summary(m3))


## Modelo 4: SASS total (effort_other) con interacción de grupo
m4 <- lm(effort_other ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m4))



# Post-hoc: slopes de SASS por grupo
trends_sass <- emtrends(m4, ~ grupo, var = "SASS_DIRt",
                        at = list(grupo = c(0, 1)))

# Slopes por grupo con IC 95% y test contra 0
summary(trends_sass, infer = c(TRUE, TRUE))

# Contraste entre grupos (equivale al término de interacción)
pairs(trends_sass)


# Efectos principales correctos (SS tipo II ignoran las interacciones al testear los términos de menor orden)
Anova(m4, type = "II")

# Alternativa equivalente: centrar SASS y reajustar
df_mod$SASS_c <- scale(df_mod$SASS_DIRt, center = TRUE, scale = FALSE)
m4c <- lm(diff_effort ~ SASS_c * grupo, data = df_mod)
summary(m4c)
