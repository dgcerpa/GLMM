

# Importar Librerías

library(tidyverse)
library(readxl)
library(lme4)
library(readxl)
library(car)
library(ggeffects)
library(ggcorrplot)
library(corrplot)
library(emmeans)
library(performance)



# Importar datos

glm <- read.csv("Datos/datos_long_glmm_seba.csv", header = T)


# Limpieza de datos


glm_filtred <- glm[glm$zeros_OTHER <= 25, ] 
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]


str(glm)

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


numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


write.csv(alldata.sc, "Datos/datos_long_filtrados.csv")




##


alldata.sc <- read.csv("Datos/datos_long_filtrados.csv", header = T)


alldata.sc <- subset(alldata.sc, decision!= 2)
alldata.sc <- subset(alldata.sc, select = -c(X))

 



### GLMM decision ####

m1 <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


m2 <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1|sub),
            data = alldata.sc,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))


m3 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


m5 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



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


summary(m1)
summary(m3)





### POST HOC DECISON####


#Se crea una lista con los valores de effort
niveles_esfuerzo <- list(c.effort = c(-1.3412537, -0.4459018, 0.4494501, 1.3448020))


# Calculamos las medias marginales
m3_post_hoc_effort <- emmeans(m3, ~  grupo*agent | c.effort,
                              at = niveles_esfuerzo,
                              type = "response") #este formato compara por niveles de esfuerzo 

pairs(m3_post_hoc_effort)


#Segundo forma de realizar el post hoc comparando pendientes 
slopes_effort_m3 <- emtrends(m3,~  agent * grupo,
                             var = "c.effort") #este formato compara por pendiente 
pairs(slopes_effort_m3)




# Comparación de niveles de Esfuerzo por Agente
# Objetivo: Ver diferencias entre niveles de esfuerzo DENTRO de cada agente.
effort_contrast_agent <- emmeans(m3, 
                                 pairwise ~ c.effort | agent, 
                                 at = niveles_esfuerzo, 
                                 type = "response", # Para obtener probabilidades en el output
                                 pbkrtest.limit = 5012, lmerTest.limit = 5021)

summary(effort_contrast_agent)


# Contraste de Interacción de Tendencias 
# Evaluar si la FORMA (tendencia lineal/cuadrática) de la curva de esfuerzo
# es diferente entre Self y Other.
concon <- contrast(effort_contrast_agent[[1]], 
                   interaction = c("poly", "consec"), 
                   by = NULL) 

summary(concon)


# Comparación de Agentes por nivel de Esfuerzo 
# Ver si hay diferencias significativas entre Self vs Other en cada punto de esfuerzo específico.
agent_contrast_effort <- emmeans(m3, 
                                 pairwise ~ agent | c.effort, 
                                 at = niveles_esfuerzo, 
                                 type = "response",
                                 pbkrtest.limit = 5012, lmerTest.limit = 5021)

summary(agent_contrast_effort)


# Comparación de Grupos por Agente
# Ver si el Grupo Control difiere del Vulnerable mirando a cada agente por separado
# (Promediando a través de los niveles de esfuerzo, a menos que especifiques 'at').
grupo_contrast <- emmeans(m3, 
                          pairwise ~ grupo | agent,
                          at = niveles_esfuerzo,
                          type = "response",
                          pbkrtest.limit = 5012, lmerTest.limit = 5021)
summary(grupo_contrast)







z<-emtrends(m1, ~ grupo | agent,
            var = "c.effort")



summary(z)
pairs(z, adjust = "fdr")



z2<-emtrends(m1, ~ agent | grupo,
             var = "c.effort")

summary(z2)
pairs(z2, adjust = "fdr")










####


data.self<- subset(alldata, agent!="1")
data.other<- subset(alldata, agent!="0")



numcols <- grep("^c\\.",names(data.other))
data.other.sc <- data.other
data.other.sc[,numcols] <- scale(data.other.sc[,numcols])



numcols <- grep("^c\\.",names(data.self))
data.self.sc <- data.self
data.self.sc[,numcols] <- scale(data.self.sc[,numcols])



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



indvself <- coef(lmself_rs.sc)
indvother <- coef(lmother_rs.sc)









