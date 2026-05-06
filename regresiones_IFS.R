

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
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

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


# plot(ggpredict(m1, terms = c("MAIA_DIRt", "grupo"))) +
#   ggtitle("Modelo 4") +
#   ylab("Diff Effort") +
#   xlab("MAIA Total") +
#   scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
#   theme_minimal()


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


# plot(ggpredict(m1, terms = c("MAIA_DIRt", "grupo"))) +
#   ggtitle("Modelo 4") +
#   ylab("Diff Effort") +
#   xlab("MAIA Total") +
#   scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
#   theme_minimal()


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


plot(ggpredict(m4, terms = c("SASS_DIRt", "grupo"))) +
  ggtitle("Modelo 4") +
  ylab("Diff Effort") +
  xlab("SASS Total") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()



## Modelo 3: SASS total (effort_other) sin interacción de grupo
m3 <- lm(effort_other ~ SASS_DIRt, data = df_mod)
print(summary(m3))



## Modelo 4: SASS total (effort_other) con interacción de grupo
m4 <- lm(effort_other ~ SASS_DIRt * grupo, data = df_mod)
print(summary(m4))


# plot(ggpredict(m4, terms = c("SASS_DIRt", "grupo"))) +
#   ggtitle("Modelo 4") +
#   ylab("Effort other") +
#   xlab("SASS Total") +
#   scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
#   theme_minimal()

