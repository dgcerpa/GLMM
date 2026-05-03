

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




## Importar datos de vuelta

alldata.sc <- read.csv("Datos/datos_long_filtrados.csv", header = T)


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


## Modelo 5: igual que m3 con solo intercept aleatorio
m5 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Modelos descartados (no convergen o son más complejos sin mejora)
# m4  <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward + agent|sub),
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
modelo_comp_dec <- anova(m1, m2, m3, m5)


# Gráfico del modelo ganador (AIC)
plot(ggpredict(m3, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 3") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()


# Resumen de modelos seleccionados
summary(m1)
summary(m3)




###################
# Post-Hoc

# Slopes de effort por grupo condicional en agent
z<-emtrends(m1, ~ grupo | agent,
            var = "c.effort")

summary(z)
pairs(z, adjust = "fdr")


# Slopes de effort por agent condicional en grupo
z2<-emtrends(m1, ~ agent | grupo,
             var = "c.effort")

summary(z2)
pairs(z2, adjust = "fdr")




####
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
lmself_rs.sc<-glmer(decision ~ c.reward*grupo + c.effort*grupo + (1+c.reward + c.effort|sub),
                    data=data.self.sc,
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))


lmother_rs.sc<-glmer(decision ~ c.reward*grupo + c.effort*grupo + (1+c.reward + c.effort|sub),
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
write.csv(indv_dataset, "post_hoc.csv", row.names = FALSE)




###################################
## Juntar datasets

# Cargar datos de cuestionarios, slopes y parámetros computacionales
pasar <- read.csv("Datos/pasar_a_diego_v2.csv")
pasar <- pasar[, -c(1, (ncol(pasar)-5):ncol(pasar))]

post_hoc <- read.csv("Datos/post_hoc.csv")

params <- read_excel("Datos/params_2k1b_all_families.xlsx") %>%
  select(sub = subject_id, p_2k1b_k_self, p_2k1b_k_other, p_2k1b_beta, p_2k1b_diff_k)

# Unir por sujeto y guardar dataset completo
dataset_completo <- pasar %>%
  left_join(post_hoc, by = "sub") %>%
  left_join(params, by = "sub")

write.csv(dataset_completo, "dataset_full.csv", row.names = FALSE)


