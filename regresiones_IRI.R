

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026

## Librerías

library(tidyverse)
library(car)
library(broom)
library(performance)
library(ggplot2)
library(ggeffects)

#########################
## Data

# Cargar datos
df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Filtro por grupo
# df <- subset(df, grupo != "1")

# Construir compuestos IRI para M3
df <- df %>%
  mutate(IRI_Cognitivo = IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd,
         IRI_Afectivo  = IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd)

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
print(summary(m3))






df_mod$Fatigue_diff <- as.numeric(df$Fatigue_diff)
mA <- lm(diff_effort ~ IRI_DIRt * grupo + Fatigue_diff, data = df_mod)
mB <- lm(diff_effort ~ IRI_Cognitivo * grupo + IRI_Afectivo * grupo + Fatigue_diff, data = df_mod)
print(car::Anova(mA, type = "II"), digits = 6)   # IRI_DIRt:grupo p ≈ 0.4845
print(car::Anova(mB, type = "II"), digits = 6)   # IRI_Afectivo:grupo F ≈ 0.00447
emtrends(mB, ~ grupo, var = "IRI_Cognitivo", at = list(grupo = c(0, 1))) |>
  summary(infer = c(TRUE, TRUE))                 # NV p ≈ .010; SV p ≈ .250


