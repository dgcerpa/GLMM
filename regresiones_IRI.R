

## Regresiones múltiples
## diff_effort y effort_other como VD y la empatía (cuestionario IRI) como VI
## 
## Modelos
##   M1 = IRI total
##   M2 = 4 subescalas (Fantasia, TomaPerspectiva, PreocEmpatica, IncomodidadPers)
##   M3 = 2 compuestos: (Fantasia + TomaPerspectiva) y (PreocEmp + IncomodidadPers)


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
## Modelo completo (diff_effort) sin interacción de grupo


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




## Modelo 1: IRI total
m1 <- lm(diff_effort ~ IRI_PreocupacionEmpatica_DIRd * grupo + 
                       IRI_TomaPerspectiva_DIRd * grupo,
         data = df_mod)
print(summary(m1))


# Gráfico del m4

plot(ggpredict(m1, terms = c("IRI_PreocupacionEmpatica_DIRd", "grupo"))) +
  ggtitle("Modelo 4") +
  ylab("Diff Effort") +
  xlab("IRI - Preocupación Empática") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()





###################################
## Modelo completo (diff_effort) con interacción de grupo


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



## IRI Preocupación empática * grupo
m4 <- lm(diff_effort ~ IRI_PreocupacionEmpatica_DIRd * grupo, data = df_mod)
print(summary(m4))



# Gráfico del m4

plot(ggpredict(m4, terms = c("IRI_PreocupacionEmpatica_DIRd", "grupo"))) +
  ggtitle("Modelo 4") +
  ylab("Diff Effort") +
  xlab("IRI - Preocupación Empática") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()



## IRI toma de perspectiva * grupo
m5 <- lm(diff_effort ~ IRI_TomaPerspectiva_DIRd * grupo, data = df_mod)
print(summary(m5))



# Gráfico del m4

plot(ggpredict(m5, terms = c("IRI_TomaPerspectiva_DIRd", "grupo"))) +
  ggtitle("Modelo 5") +
  ylab("Diff Effort") +
  xlab("IRI - Toma de perspectiva") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()




###################################
## Modelo completo (effort_other) sin interacción de grupo


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
## Modelo completo (effort_other) con interacción de grupo


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





