


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





glm <- read.csv("datos_long_glmm_seba.csv", header = T)

#Se elimina a todo participante que tenga una omision mayor a 25 tanto en other como self
glm_filtred <- glm[glm$zeros_OTHER <= 25, ] 
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]
glm_filtred <- subset(glm_filtred, decision!= 2)


#Change the tipe of the variable

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


#estandariza las variables de interes" 
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


#data slope vulnerable 
data.cont <- subset(alldata.sc, grupo!="1")

#data slope control
data.vul <- subset(alldata.sc, grupo!="0")  



#data slope self
data.other<- subset(alldata.sc, agent!="0")


#data slope other 
data.self<- subset(alldata.sc, agent!="1")



#modelos 


#modelo de vulnerabilidad 
lmvul <- glmer(decision ~ c.reward*agent + c.effort*agent + (1 + c.effort + c.reward|sub)
               ,data=data.cont,
               family=binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl=list(maxfun=2e5)))

plot(ggpredict(lmvul, terms = c("c.reward", "agent"))) +
  ggtitle("Modelo 4.1 simplificado SIN grupo Effort × Agent") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  theme_minimal()

#modelo grupo control 
lmctl <- glmer(decision ~ c.reward*agent + c.effort*agent + (1 + c.effort + c.reward|sub)
               ,data=data.vul,
               family=binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl=list(maxfun=2e5)))


#modelo de self
lmself<-glmer(decision ~ c.reward*grupo + c.effort*grupo + (1 + c.effort + c.reward|sub)
                   ,data=data.self,
                   family=binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl=list(maxfun=2e5)))



#modelo de other
lmother <-glmer(decision ~ c.reward*grupo + c.effort*grupo + (1 + c.effort + c.reward|sub)
                              ,data=data.other,
                              family=binomial,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=2e5)))



#extraer slopes de self, other, control y vulnerable 
indvself <- coef(lmself)
indvother <- coef(lmother)
indctl <- coef(lmctl)
indvvul <- coef(lmvul)

#extraer data frame
df_other <- indvother[[1]]
df_self <- indvself[[1]]
df_ctl <- indctl[[1]]
df_vul <- indvvul[[1]]


#pasar eje x como columna para identificar a los sujetos 
indvself_2 <- rownames_to_column(df_self, var = "sub")
head(indvself_2)
#Renomabrar variables de serf
colnames(indvself_2) <-c("sub", "Intercept_self", "c.reward_self", "grupo1_self", "c.effort_self", "c.reward:grupo1_self",
                         "grupo1:c.effort_self")

#pasar eje x como columna para identificar a los sujetos 
indvother_2 <- rownames_to_column(df_other, var = "sub")

#Renombrar variables de other
colnames(indvother_2) <-c("sub", "Intercept_other", "c.reward_other", "grupo1_other", "c.effort_other", "c.reward:grupo1_other",
                          "grupo1:c.effort_other")

#pasar eje x como columna para identificar a los sujetos 
indvvul_2 <- rownames_to_column(df_vul, var = "sub")
#Renombrar variables vulnerabilidad 
colnames(indvvul_2) <- c("sub","Intercept_vul", "c.reward_vul", "agent_vul", "c.effort_vul",
                                      " c.reward:agent1_vul", "agent1:c.effort_vul")

#pasar eje x como columna para identificar a los sujetos 
indctl_2 <- rownames_to_column(df_ctl, var = "sub")
#Renombrar variables vulnerabilidad 
colnames(indctl_2) <- c("sub","Intercept_ctl", "c.reward_ctl", "agent_ctl", "c.effort_ctl",
                                      " c.reward:agent1_ctl", "agent1:c.effort_ctl")

head(indvvul_2)
#JUNTAR AMBAS DATAS 
slope <- indvother_2 %>%
  left_join(indvself_2,by = "sub") #data self

slope <- slope %>%
  left_join(indctl_2, by = "sub")#data de los slopes del grupo control

slope <- slope %>%
  left_join(indvvul_2, by = "sub")#data grupo vulnetables

slope_v2 <- slope %>%
  select(sub,c.reward_other,c.effort_other,c.reward_self,c.effort_self) # seleccionamos effort y reward para sacar diferenicas 

mg <- slope_v2 %>%
  mutate(
    DIF_effort = c.effort_other - c.effort_self, 
    DIF_reward = c.reward_other - c.reward_self,
    
  ) %>%
  select(sub,DIF_effort,DIF_reward) 

slope <- slope %>%
  left_join(mg,by = "sub") # juntamos datas




#Pasar "sub" a int para juntar base de datos de huepe
slope$sub <- as.integer(slope$sub)




#Leer base de datos HUEPE
viña <- read.csv("datos_analisis_all.csv")


#Juntar base de datos de SLOPE con datos de HUEPE

quillota <- viña %>%
  left_join(slope, by = "sub")


#Filtrar base de datos 
limache <- quillota[quillota$zeros_SELF <= 25,]
limache <- limache[limache$zeros_OTHER <= 25,]

#Guardar base de datos 
write.csv(limache, "limache.csv")


#### Correlaciones ####

#Seleccionamos las variables de interés
limache_select <- limache %>%
  select(c.effort_other,c.effort_self, c.reward_other, c.reward_self, DIF_reward , DIF_effort  , IFS_Total_DIRd, UCLA_DIRt, CSI_DIRt, PSS_DIRt,
         DASS21_depresion_DIRd, DASS21_estres_DIRd, DASS21_ansiedad_DIRd, SWBS_DIRt)

     #Realizamos la correlación para ver la interacción de las variables
correlacion <- cor(limache_select, use = "complete.obs", method = "spearman")
correlacion_1 <- cor_pmat(limache_select, conf.level = 0.95, method = "spearman")


corrplot(correlacion, 
         method = "color", 
         type = "upper",
         p.mat = correlacion_1, # Aquí le pasamos los valores p
         sig.level = 0.05,             # Nuestro límite de significancia
         insig = "pch",                # "pch" pone una cruz en los no significativos
         addCoef.col = "black", 
         tl.col = "black", 
         diag = FALSE)


#Para observar mejor variables significativas, separamos SWBS (bienestar social) y UCLA (soledad) 
soledad <- limache %>%
  select(c.reward_other ,c.reward_self, c.effort_other, c.effort_self, grupo, UCLA_DIRt, SWBS_DIRt, DASS21_depresion_DIRd,
         DASS21_estres_DIRd, DASS21_ansiedad_DIRd) 


cor_soledad <- cor(soledad, use = "complete.obs", method = "spearman")
cor_soledad_1 <- cor_pmat(soledad, conf.level = 0.95, method = "spearman")

corrplot(cor_soledad, 
         method = "color", 
         type = "upper",
         p.mat = cor_soledad_1, # Aquí le pasamos los valores p
         sig.level = 0.05,             # Nuestro límite de significancia
         insig = "pch",                # "pch" pone una cruz en los no significativos
         addCoef.col = "black", 
         tl.col = "black", 
         diag = FALSE)



###Regresión lineal####    

#Aplicamos una regresión en los datos seleccionados (soledad)
Valdivia <- lm(c.reward_self ~ UCLA_DIRt, data = soledad) 

ggplot(data = soledad, aes(x = UCLA_DIRt, y = c.reward_self)) +
  geom_point(color = "steelblue", alpha = 0.7) +   
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relación entre c.reward_self y UCLA_DIRt",
    x = "Soledad",
    y = "c.reward_self"
  ) +
  theme_minimal()

  








