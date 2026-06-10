

## Regresiones múltiples
# Diego Garrido Cerpa - Viña del Mar 2026

## Librerías

library(tidyverse)
library(car)        # vif()
library(broom)      # tidy() / glance()
library(performance) # check_model() opcional
library(ggplot2)
library(ggeffects)
library(patchwork)

#########################
## Data

# Cargar datos
df <- read_csv("dataset_full_final.csv")

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
m21 <- lm(diff_effort ~ IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd +
           IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd,
         data = df_mod)
print(summary(m2))
print(summary(m21))



# Gráfico del m2 (con interacción de grupo): 4 paneles (2x2)
g2_fan <- plot(ggpredict(m2, terms = c("IRI_Fantasia_DIRd", "grupo"))) +
  ggtitle("Fantasía") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Fantasía") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

g2_tom <- plot(ggpredict(m2, terms = c("IRI_TomaPerspectiva_DIRd", "grupo"))) +
  ggtitle("Toma de Perspectiva") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Toma de Perspectiva") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

g2_pre <- plot(ggpredict(m2, terms = c("IRI_PreocupacionEmpatica_DIRd", "grupo"))) +
  ggtitle("Preocupación Empática") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Preocupación Empática") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

g2_inc <- plot(ggpredict(m2, terms = c("IRI_IncomodidadPersonal_DIRd", "grupo"))) +
  ggtitle("Incomodidad Personal") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Incomodidad Personal") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

graf_m2 <- (g2_fan | g2_tom) / (g2_pre | g2_inc) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Subescalas IRI y Diferencia de Esfuerzo por Grupo (m2)") &
  theme(legend.position = "bottom")
print(graf_m2)


# Gráfico del m21 (sin grupo, efectos principales): 4 paneles (2x2)
g21_fan <- plot(ggpredict(m21, terms = "IRI_Fantasia_DIRd")) +
  ggtitle("Fantasía") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Fantasía") +
  theme_minimal()

g21_tom <- plot(ggpredict(m21, terms = "IRI_TomaPerspectiva_DIRd")) +
  ggtitle("Toma de Perspectiva") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Toma de Perspectiva") +
  theme_minimal()

g21_pre <- plot(ggpredict(m21, terms = "IRI_PreocupacionEmpatica_DIRd")) +
  ggtitle("Preocupación Empática") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Preocupación Empática") +
  theme_minimal()

g21_inc <- plot(ggpredict(m21, terms = "IRI_IncomodidadPersonal_DIRd")) +
  ggtitle("Incomodidad Personal") +
  ylab("Diferencia de Esfuerzo (self - other)") + xlab("IRI - Incomodidad Personal") +
  theme_minimal()

graf_m21 <- (g21_fan | g21_tom) / (g21_pre | g21_inc) +
  plot_annotation(title = "Subescalas IRI y Diferencia de Esfuerzo (m21)")
print(graf_m21)







## Modelo 3: 2 compuestos (cognitivo / afectivo)
m3 <- lm(diff_effort ~ IRI_Cognitivo * grupo + IRI_Afectivo * grupo, data = df_mod)
m31 <- lm(diff_effort ~ IRI_Cognitivo + IRI_Afectivo , data = df_mod)

print(summary(m3))
print(summary(m31))
anova(m31,m3)




# Gráfico del m3 (con interacción de grupo): dos paneles lado a lado
g3_cog <- plot(ggpredict(m3, terms = c("IRI_Cognitivo", "grupo"))) +
  ggtitle("Empatía Cognitiva") +
  ylab("Diferencia de Esfuerzo (self - other)") +
  xlab("IRI - Cognitivo (Fantasía + Toma de Perspectiva)") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

g3_afe <- plot(ggpredict(m3, terms = c("IRI_Afectivo", "grupo"))) +
  ggtitle("Empatía Afectiva") +
  ylab("Diferencia de Esfuerzo (self - other)") +
  xlab("IRI - Afectivo (Preocupación Empática + Incomodidad Personal)") +
  scale_colour_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  scale_fill_discrete(labels = c("0" = "Control", "1" = "Experimental"), name = "Grupo") +
  theme_minimal()

graf_m3 <- (g3_cog | g3_afe) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Empatía y Diferencia de Esfuerzo por Grupo (m3)") &
  theme(legend.position = "bottom")
print(graf_m3)


# Gráfico del m31 (sin grupo, efectos principales): dos paneles lado a lado
g31_cog <- plot(ggpredict(m31, terms = "IRI_Cognitivo")) +
  ggtitle("Empatía Cognitiva") +
  ylab("Diferencia de Esfuerzo (self - other)") +
  xlab("IRI - Cognitivo (Fantasía + Toma de Perspectiva)") +
  theme_minimal()

g31_afe <- plot(ggpredict(m31, terms = "IRI_Afectivo")) +
  ggtitle("Empatía Afectiva") +
  ylab("Diferencia de Esfuerzo (self - other)") +
  xlab("IRI - Afectivo (Preocupación Empática + Incomodidad Personal)") +
  theme_minimal()

graf_m31 <- (g31_cog | g31_afe) +
  plot_annotation(title = "Empatía y Diferencia de Esfuerzo (m31)")
print(graf_m31)



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





