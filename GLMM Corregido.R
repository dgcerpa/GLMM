

## GLMM: Modelo mixto generalizado
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(readxl)
library(lme4)       # glmer()
library(readxl)
library(car)        # vif()
library(ggeffects)  # ggpredict()
library(ggcorrplot) # ggcorrplot()
library(corrplot)   # corrplot()
library(emmeans)    # emtrends(), pairs()
library(performance) # check_model()



#########################
## Data

# Cargar datos
glm <- read.csv("Datos/datos_long_glmm_seba.csv", header = T)


# Limpieza de datos: filtrar sujetos con demasiados ceros (>25%) en OTHER y SELF
glm_filtred <- glm[glm$zeros_OTHER <= 25, ]
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]


# Inspeccionar estructura del dataset
str(glm)

# Convertir variables a factor/numérico y renombrar columnas
alldata <- data.frame(as.factor(glm_filtred$sub),
                      as.factor(glm_filtred$decision),
                      as.factor(glm_filtred$grupo),
                      as.factor(glm_filtred$agent),
                      as.factor(glm_filtred$AIM_2),
                      as.numeric(as.character(glm_filtred$success)),
                      as.numeric(as.character(glm_filtred$tasa_fallo_other)),
                      as.numeric(as.character(glm_filtred$tasa_fallo_self)),
                      as.numeric(as.character(glm_filtred$zeros_OTHER)),
                      as.numeric(as.character(glm_filtred$zeros_SELF)),
                      as.numeric(as.character(glm_filtred$reward)),
                      as.numeric(as.character(glm_filtred$effort)),
                      as.numeric(as.character(glm_filtred$trail)),
                      as.numeric(as.character(glm_filtred$AIM_num)),
                      as.numeric(as.character(glm_filtred$AIM_3)),
                      as.numeric(as.character(glm_filtred$AIM_4))
)

colnames(alldata)<-c("sub", "decision", "grupo", "agent", "AIM_2", "success", "c.tasa_fallo_other",
                     "c.tasa_fallo_self", "c.zeros_other", "c.zeros_self", "c.reward", "c.effort", "c.trail",
                     "c.AIM_num", "c.AIM_3", "c.AIM_4")


# Estandarizar (z-score) todas las variables numéricas (prefijo c.)
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


# Guardar datos filtrados y estandarizados
write.csv(alldata.sc, "Datos/datos_long_filtrados.csv")



######################################
## Importar datos de vuelta

alldata.sc <- read.csv("datos_long_glmm_filtrados.csv", header = T)


# Excluir trials omitidos y eliminar columna índice
alldata.sc <- subset(alldata.sc, decision!= 2)
alldata.sc <- subset(alldata.sc, select = -c(X))




###################################
### GLMM decision ####

## Modelo 1: interacción completa con slopes aleatorios por sujeto
m1 <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


## Modelo 2: interacción completa con solo intercept aleatorio
m2 <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1|sub),
            data = alldata.sc,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))


## Modelo 3: efectos de reward y effort separados con slopes aleatorios
m3 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


## Modelo 4: igual que m3 con solo intercept aleatorio
m4 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Modelos descartados (no convergen o son más complejos sin mejora)
# m5  <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward + agent|sub),
#              data=alldata.sc,
#              family=binomial,
#              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
#
#
#
# m6  <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1 + c.effort + c.reward + agent|sub),
#              data=alldata.sc,
#              family=binomial,
#              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
#
#
# m7 <- glmer(decision ~ c.reward*c.effort*agent + (1+ c.reward + c.effort + agent|sub),
#             data=alldata.sc,
#             family=binomial,
#             control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
#
#
# m8 <- glmer(decision ~ c.reward*c.effort*agent*grupo + (1+ c.reward + c.effort + agent|sub),
#             data=alldata.sc,
#             family=binomial,
#             control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
#
#
# m9 <- glmer(decision ~ c.reward*agent*grupo + c.effort*grupo +  (1 + c.effort + c.reward|sub),
#             data=alldata.sc,
#             family=binomial,
#             control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
#


# Comparación de modelos
modelo_comp_dec <- anova(m1, m2, m3, m4)




# Gráfico del modelo ganador (AIC)
plot(ggpredict(m3, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 3") +
  ylab("Probabilidad de trabajo") +
  xlab("Nivel de esfuerzo") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("grupo = 0" = "Control", "grupo = 1" = "Experimental"))) +
  theme_minimal()










# Resumen de modelos seleccionados
# summary(m1)

summary(m3)
car::Anova(m3, type = "II")
isSingular(m3)
performance::r2_nakagawa(m3)
anova(m1, m2, m3, m4)


# (1) Slopes por grupo, dentro de cada agent
z_grupo_en_agent <- emtrends(m3, ~ grupo | agent, var = "c.effort")
summary(z_grupo_en_agent, infer = c(TRUE, TRUE))
pairs(z_grupo_en_agent, adjust = "fdr")

# (2) Slopes por agent, dentro de cada grupo
z_agent_en_grupo <- emtrends(m3, ~ agent | grupo, var = "c.effort")
summary(z_agent_en_grupo, infer = c(TRUE, TRUE))
pairs(z_agent_en_grupo, adjust = "fdr")







###################
# Post-Hoc

# Slopes de effort por grupo condicional en agent
z<-emtrends(m3, ~ grupo | agent,
            var = "c.effort")

summary(z)
pairs(z, adjust = "fdr")


# Slopes de effort por agent condicional en grupo
z2<-emtrends(m3, ~ agent | grupo,
             var = "c.effort")

summary(z2)
pairs(z2, adjust = "fdr")




###################
# Gráficos Post-Hoc

# Etiquetas comunes
lab_grupo <- c("0" = "Control", "1" = "Experimental")
lab_agent <- c("0" = "Self",    "1" = "Other")


## ---- Gráfico 1: Forest plot de slopes (z y z2) ----
# Estimación puntual e IC95% de las pendientes de c.effort para cada
# combinación grupo x agent. Es la representación directa de z / z2.

df_z <- as.data.frame(summary(z, infer = c(TRUE, TRUE)))

# Homogeneizar nombres de IC (emtrends sobre GLMM usa asymp.LCL/UCL)
names(df_z)[names(df_z) == "asymp.LCL"] <- "lower.CL"
names(df_z)[names(df_z) == "asymp.UCL"] <- "upper.CL"

# Asegurar que grupo y agent sean factores (vienen como numéricos)
df_z$grupo <- factor(df_z$grupo)
df_z$agent <- factor(df_z$agent)

g1 <- ggplot(df_z,
             aes(x = grupo, y = c.effort.trend,
                 ymin = lower.CL, ymax = upper.CL,
                 colour = agent)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(position = position_dodge(width = 0.4), size = 0.8) +
  scale_x_discrete(labels = lab_grupo) +
  scale_colour_discrete(labels = lab_agent, name = "Agente") +
  labs(title = "Pendientes de esfuerzo (c.effort) sobre la decisión",
       subtitle = "Estimación e IC95% por grupo x agente (modelo m1)",
       x = "Grupo", y = "Slope de c.effort (escala logit)") +
  theme_minimal(base_size = 13)

print(g1)


## ---- Gráfico 2: Contrastes pareados con IC95% ----
# Diferencias entre slopes con FDR; las estrellas marcan p<.05.

contr_z <- as.data.frame(pairs(z, adjust = "fdr"))
contr_z$panel <- paste0("Por agente: ",
                        factor(contr_z$agent,
                               levels = c(0, 1),
                               labels = c("Self", "Other")))

contr_z2 <- as.data.frame(pairs(z2, adjust = "fdr"))
contr_z2$panel <- paste0("Por grupo: ",
                         factor(contr_z2$grupo,
                                levels = c(0, 1),
                                labels = c("Control", "Experimental")))

contrastes <- bind_rows(contr_z, contr_z2)
contrastes$lower <- contrastes$estimate - 1.96 * contrastes$SE
contrastes$upper <- contrastes$estimate + 1.96 * contrastes$SE
contrastes$sig   <- ifelse(contrastes$p.value < 0.05, "*", "")

g2 <- ggplot(contrastes,
             aes(x = estimate, y = contrast,
                 xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_pointrange(colour = "steelblue", size = 0.7) +
  geom_text(aes(label = sig), nudge_y = 0.25, size = 6) +
  facet_wrap(~ panel, scales = "free_y") +
  labs(title = "Contrastes pareados de slopes de c.effort",
       subtitle = "Diferencia de pendientes (IC95%, FDR). * p < .05",
       x = "Diferencia estimada (logit)", y = NULL) +
  theme_minimal(base_size = 13)

print(g2)


## ---- Gráfico 3: Curvas predichas de decisión ----
# Visualización intuitiva: probabilidad predicha de aceptar la oferta en
# función de c.effort, separada por grupo y agente. Las pendientes z/z2
# corresponden a la inclinación de cada curva.

pred_eff <- ggpredict(m3, terms = c("c.effort [all]", "agent", "grupo"))

g3 <- plot(pred_eff) +
  scale_colour_discrete(labels = lab_agent, name = "Agente") +
  scale_fill_discrete(labels   = lab_agent, name = "Agente") +
  facet_wrap(~ facet, labeller = labeller(facet = lab_grupo)) +
  labs(title = "Probabilidad predicha de aceptar vs. esfuerzo",
       subtitle = "Curvas que ilustran las pendientes estimadas en z y z2",
       x = "Esfuerzo (z-score)", y = "P(decisión = aceptar)") +
  theme_minimal(base_size = 13)

print(g3)


# Guardar figuras (ajusta ruta/dimensiones si lo necesitas)
dir.create("Figuras", showWarnings = FALSE)
ggsave("Figuras/posthoc_slopes.png",     g1, width = 7, height = 5, dpi = 300)
ggsave("Figuras/posthoc_contrastes.png", g2, width = 8, height = 5, dpi = 300)
ggsave("Figuras/posthoc_curvas.png",     g3, width = 8, height = 5, dpi = 300)











##########################################
# Extraer slopes individuales de esfuerzo

# Separar datos por agente
data.self<- subset(alldata.sc, agent!="1")
data.other<- subset(alldata.sc, agent!="0")


# Reescalar variables numéricas dentro de cada subconjunto
numcols <- grep("^c\\.",names(data.other))
data.other.sc <- data.other
data.other.sc[,numcols] <- scale(data.other.sc[,numcols])


numcols <- grep("^c\\.",names(data.self))
data.self.sc <- data.self
data.self.sc[,numcols] <- scale(data.self.sc[,numcols])


# GLMM por agente para estimar slopes individuales
lmself_rs.sc<-glmer(decision ~ c.reward + c.effort + (1+c.reward + c.effort|sub),
                    data=data.self.sc,
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))


lmother_rs.sc<-glmer(decision ~ c.reward + c.effort + (1+c.reward + c.effort|sub),
                     data=data.other.sc,
                     family=binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=2e5)))


# Extraer coeficientes individuales por sujeto
indvself <- coef(lmself_rs.sc)
indvother <- coef(lmother_rs.sc)


# Construir dataset con slopes de reward y effort por sujeto
indv_dataset <- data.frame(
  sub          = rownames(indvself$sub),
  reward_self  = indvself$sub$c.reward,
  effort_self  = indvself$sub$c.effort,
  reward_other = indvother$sub$c.reward,
  effort_other = indvother$sub$c.effort
)

# Calcular diferencias entre agentes (other - self)
indv_dataset$diff_effort <- indv_dataset$effort_other - indv_dataset$effort_self
indv_dataset$diff_reward <- indv_dataset$reward_other - indv_dataset$reward_self

# Guardar slopes individuales
write.csv(indv_dataset, "post_hoc_v2.csv", row.names = FALSE)







###################################
## Juntar datasets

# Cargar datos de cuestionarios, slopes y parámetros computacionales
pasar <- read.csv("Datos/pasar_a_diego_v2.csv")
pasar <- pasar[, -c(1, (ncol(pasar)-5):ncol(pasar))]

post_hoc <- read.csv("Datos/post_hoc_v2_sin_grupo.csv")

params <- read_excel("Datos/Modelos Comp/params_2k1b_all_families.xlsx") %>%
  select(sub = subject_id, p_2k1b_k_self, p_2k1b_k_other, p_2k1b_beta, p_2k1b_diff_k)

# Unir por sujeto y guardar dataset completo
dataset_completo <- pasar %>%
  left_join(post_hoc, by = "sub") %>%
  left_join(params, by = "sub")

write.csv(dataset_completo, "Datos/dataset_full_v2.csv", row.names = FALSE)















###################################


### GLMM success ####

alldata.sc_a <- subset(alldata.sc, decision!= 0)


m1 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1 + c.effort + c.reward|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

m2 <- glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

m3 <- glmer(success ~ c.reward*agent*c.effort*grupo + (1 + c.reward + c.effort|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))
m4 <- glmer(success ~ c.reward*agent*c.effort*grupo + (1|sub)
            ,data=alldata.sc_a,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl=list(maxfun=2e5)))

# Modelo comparison

model_comp_success <- anova(m1, m2, m3, m4)



plot(ggpredict(m2, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 2") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()







#POST HOC modelo succes#### 
# NOTA: el mensaje "Results may be misleading due to involvement in interactions"
# NO es un error. Aparece porque 'grupo' y 'agent' interactúan con las covariables
# continuas c.reward y c.effort. emmeans promedia esas covariables en su media (= 0,
# porque están estandarizadas con scale()), de modo que estas medias marginales
# representan la probabilidad de éxito a recompensa y esfuerzo promedio. El cálculo
# es correcto; fijamos las covariables con 'at' para dejarlo explícito y documentado.

em_grupo_en_agent <- emmeans(m2, ~ grupo | agent,
                             type = "response",
                             at = list(c.reward = 0, c.effort = 0))
pairs(em_grupo_en_agent)

summary(m2)


slopes_success_m2 <- emtrends(m2,~ grupo*agent,
                              var = "c.reward")
pairs(slopes_success_m2)





# Grilla completa grupo x agente para los gráficos (covariables fijadas en su media)
df_success_m2 <- as.data.frame(
  emmeans(m2, ~ grupo * agent,
          type = "response",
          at = list(c.reward = 0, c.effort = 0))
)

# 'grupo' y 'agent' vienen como numéricos (0/1) al leer el CSV; los pasamos a factor
# para que las escalas discretas y de color de ggplot funcionen correctamente.
df_success_m2$grupo <- factor(df_success_m2$grupo)
df_success_m2$agent <- factor(df_success_m2$agent)





#graficos success POST HOC 
ggplot(df_success_m2, aes(x = agent, y = prob, color = grupo)) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  scale_x_discrete(
    labels = c("0" = "Self", "1" = "Other")
  ) +
  scale_color_manual(
    values = c("0" = "#1F77B4", "1" = "#D62728"), # Puedes ajustar los colores aquí
    labels = c("0" = "Control", "1" = "Vulnerable"),
    name = "Grupo"
  ) +
  labs(
    x = "Agente",
    y = "Probabilidad predicha de éxito",
    color = "Grupo"
  ) +
  theme_light(base_size = 14)


ggplot(df_success_m2, aes(x = grupo, y = prob, fill = agent)) +
  geom_col(
    position = position_dodge(width = 0.6),
    width = 0.5, color = "black"
  ) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.15, color = "black",
    position = position_dodge(width = 0.6)
  ) +
  scale_x_discrete(
    labels = c("0" = "Control", "1" = "Vulnerable")
  ) +
  scale_fill_manual(
    values = c("0" = "#1F77B4", "1" = "#D62728"),
    labels = c("0" = "Self", "1" = "Other"),
    name = "Agente"
  ) +
  labs(
    x = "Grupo",
    y = "Probabilidad de éxito"
  ) +
  theme_classic(base_size = 14)









########################################
## Actualizar datos

# Librerías

library(readr)   # read_csv / write_csv
library(readxl)  # read_excel
library(dplyr)   # manipulacion de datos

# Archivos

ruta_dataset <- "dataset_full_final.csv"
ruta_modelos <- "Datos/Modelos Comp/2k1b_modelos.csv"
ruta_params  <- "Datos/Modelos Comp/params_2k1b_all_families.xlsx"
ruta_salida  <- "dataset_full_final.csv"   # sobrescribe el original

# ELIMINAR columnas antiguas
dataset <- read_csv(ruta_dataset, show_col_types = FALSE)

cols_a_eliminar <- c("p_2k1b_k_self", "p_2k1b_k_other",
                     "p_2k1b_beta", "p_2k1b_diff_k")

dataset <- dataset %>% select(-any_of(cols_a_eliminar))

# Columnas del script NUEVO (sufijo "_new")
modelos <- read_csv(ruta_modelos, show_col_types = FALSE)

# Prefijos solicitados desde 2k1b_modelos.csv
prefijos_new <- c("p_self_k", "p_other_k", "p_beta", "p_other_self_k",
                  "h_self_k", "h_other_k", "h_beta", "h_other_self_k")

cols_new <- names(modelos)[
  vapply(names(modelos),
         function(x) any(startsWith(x, prefijos_new)),
         logical(1))
]

modelos_sel <- modelos %>%
  select(ui, all_of(cols_new)) %>%       # 'ui' es la llave (= 'sub')
  rename_with(~ paste0(.x, "_new"), all_of(cols_new)) %>%
  rename(sub = ui)

# Columnas del script VIEJO (sufijo "_old")
params <- read_excel(ruta_params)

# Columnas que comienzan con "p_" o "h_"
cols_old <- names(params)[startsWith(names(params), "p_") |
                            startsWith(names(params), "h_")]

params_sel <- params %>%
  select(subject_id, all_of(cols_old)) %>%   # 'subject_id' es la llave (= 'sub')
  rename_with(~ paste0(.x, "_old"), all_of(cols_old)) %>%
  rename(sub = subject_id)

# Unir todo
dataset_final <- dataset %>%
  left_join(modelos_sel, by = "sub") %>%
  left_join(params_sel,  by = "sub")

# Guardar 
write_csv(dataset_final, ruta_salida)
