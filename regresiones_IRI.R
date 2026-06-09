

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
df <- read_csv("Documentos/GitHub/GLMM/Datos/dataset_full_v2.csv")

# Filtro por grupo
# df <- subset(df, grupo != "1")

# Construir compuestos IRI para M3
df <- df %>%
  mutate(
    IRI_Cognitivo = IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd,
    IRI_Afectivo  = IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd
  )

# Subset analítico (casos completos en todas las variables usadas)
vars_usadas <- c("grupo",
                 "diff_effort",
                 "effort_other",
                 "IRI_DIRt",
                 "IRI_Fantasia_DIRd", "IRI_TomaPerspectiva_DIRd",
                 "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd",
                 "IRI_Cognitivo", "IRI_Afectivo")

df_mod <- df %>%
  select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric))



###################################
## Modelo diff_effort sin interacción de grupo


## Modelo 1: IRI total
m1 <- lm(diff_effort ~ IRI_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 4 subescalas IRI
m2 <- lm(diff_effort ~ IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd +
                        IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd,
         data = df_mod)
print(summary(m2))


## Modelo 3: 2 compuestos (cognitivo / afectivo)
m3 <- lm(diff_effort ~ IRI_Cognitivo + IRI_Afectivo, data = df_mod)
print(summary(m3))




###################################
## Modelo diff_effort con interacción de grupo


## Modelo 1: IRI total
m1 <- lm(diff_effort ~ IRI_DIRt * grupo, data = df_mod)
print(summary(m1))



## Modelo 2: 4 subescalas IRI
m2 <- lm(diff_effort ~ IRI_Fantasia_DIRd * grupo + IRI_TomaPerspectiva_DIRd * grupo +
           IRI_PreocupacionEmpatica_DIRd * grupo + IRI_IncomodidadPersonal_DIRd * grupo,
         data = df_mod)
print(summary(m2))


## Modelo 3: 2 compuestos (cognitivo / afectivo)
m3 <- lm(diff_effort ~ IRI_Cognitivo * grupo + IRI_Afectivo * grupo, data = df_mod)
m31 <- lm(diff_effort ~ IRI_Cognitivo + IRI_Afectivo , data = df_mod)

print(summary(m3))
print(summary(m31))
anova(m31,m3)



## IRI Preocupación empática * grupo
m4 <- lm(diff_effort ~ IRI_PreocupacionEmpatica_DIRd * grupo, data = df_mod)
print(summary(m4))



# Gráfico del m4

plot(ggpredict(m4, terms = c("IRI_PreocupacionEmpatica_DIRd", "grupo"))) +
  ggtitle("Correlación Preocupación Empática y Diferencia de Esfuerzo") +
  ylab("Diff Effort") +
  xlab("IRI - Preocupación Empática") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()



## IRI toma de perspectiva * grupo
m5 <- lm(diff_effort ~ IRI_TomaPerspectiva_DIRd * grupo, data = df_mod)
print(summary(m5))



###################################
## Modelo effort_other sin interacción de grupo


## Modelo 1: IRI total
m1 <- lm(effort_other ~ IRI_DIRt, data = df_mod)
print(summary(m1))


## Modelo 2: 4 subescalas IRI
m2 <- lm(effort_other ~ IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd +
           IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd,
         data = df_mod)
print(summary(m2))


## Modelo 3: 2 compuestos (cognitivo / afectivo)
m3 <- lm(effort_other ~ IRI_Cognitivo + IRI_Afectivo, data = df_mod)
print(summary(m3))




###################################
## Modelo effort_other con interacción de grupo


## Modelo 1: IRI total
m1 <- lm(effort_other ~ IRI_DIRt * grupo, data = df_mod)
print(summary(m1))



## Modelo 2: 4 subescalas IRI
m2 <- lm(effort_other ~ IRI_Fantasia_DIRd * grupo + IRI_TomaPerspectiva_DIRd * grupo +
           IRI_PreocupacionEmpatica_DIRd * grupo + IRI_IncomodidadPersonal_DIRd * grupo,
         data = df_mod)
print(summary(m2))


## Modelo 3: 2 compuestos (cognitivo / afectivo)
m3 <- lm(effort_other ~ IRI_Cognitivo * grupo + IRI_Afectivo * grupo, data = df_mod)
print(summary(m3))





