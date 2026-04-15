

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

glm <- read.csv("datos_long_glmm_seba.csv", header = T)


# Limpieza de datos


glm_filtred <- glm[glm$zeros_OTHER <= 25, ] 
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]
glm_filtred <- subset(glm_filtred, decision!= 2)



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







z<-emtrends(m3, ~ grupo | agent,
            var = "c.effort")



summary(z)
pairs(z, adjust = "fdr")



z2<-emtrends(m3, ~ agent | grupo,
             var = "c.effort")

summary(z2)
pairs(z2, adjust = "fdr")








#graficos DECISION

df_decision_m3 <- as.data.frame(m3_post_hoc_effort)

ggplot(
  df_decision_m3,
  aes(
    x = c.effort,
    y = prob,
    group = interaction(grupo, agent),
    color = grupo,
    linetype = agent
  )
) +
  geom_ribbon(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL,
      fill = grupo
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "0" = "#0000FF",  
      "1" = "#FF0000"
    ),
    labels = c("0" = "Control", "1" = "Vulnerable"),
    name = "Grupo"
  ) +
  scale_fill_manual(
    values = c(
      "0" = "#00EEEE",
      "1" = "#EE3B3B"
    ),
    labels = c("0" = "Control", "1" = "Vulnerable"),
    name = "Grupo"
  ) +
  scale_linetype_manual(
    values = c(
      "1" = "solid",
      "0" = "dashed"
    ),
    labels = c("0" = "Self", "1" = "Other"),
    name = "Agente"
  ) +
  labs(
    x = "Nivel de esfuerzo",
    y = "Probabilidad de decisión"
  ) +
  theme_classic(base_size = 14) + 
  facet_wrap(~grupo) + 
  theme(
    strip.text = element_blank(),     # Borra el texto (0 y 1)
    strip.background = element_blank() # Borra el recuadro gris
  )







































#data slope vulnerable 
data_control <- subset(alldata.sc, grupo!="1")

#data slope control
data_vulnerable <- subset(alldata.sc, grupo!="0")  



#data slope self
data_other<- subset(alldata.sc, agent!="0")


#data slope other 
data_self<- subset(alldata.sc, agent!="1")



# Modelos 


# Modelo de control

lmcontrol <- glmer(decision ~ c.reward*agent + c.effort*agent + (1 + c.effort + c.reward|sub),
            data=data_control,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Modelo grupo vulnerable 

lmvulnerable <- glmer(decision ~ c.reward*agent + c.effort*agent + (1 + c.effort + c.reward|sub),
                   data=data_vulnerable,
                   family=binomial,
                   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Modelo de self
lmself <- glmer(decision ~ c.reward*grupo + c.effort*grupo + (1 + c.effort + c.reward|sub),
            data=data_self,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# Modelo de other
lmother <- glmer(decision ~ c.reward*grupo + c.effort*grupo + (1 + c.effort + c.reward|sub),
                data=data_other,
                family=binomial,
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



# plot(ggpredict(lmother, terms = c("c.effort", "c.reward [-1, 0, 1]", "grupo"))) +
#   ggtitle("Modelo Other") +
#   ylab("Probabilidad predicha de decisión") +
#   xlab("Nivel de esfuerzo (c.effort)") +
#   scale_colour_discrete(
#     labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
#     name = "Reward"
#   ) +
#   scale_fill_discrete(
#     labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
#     name = "Reward"
#   ) +
#   facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
#   theme_minimal()




summary(lmcontrol)
summary(lmvulnerable)

summary(lmself)
summary(lmother)





#extraer slopes de self, other, control y vulnerable 
indvself <- coef(lmself)
indvother <- coef(lmother)
indctl <- coef(lmcontrol)
indvvul <- coef(lmvulnerable)


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



plot(ggpredict(m2_succ_split_inter_ri, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 2") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()







#POST HOC modelo succes#### 
em_posthoc_m2_succes <- emmeans(m2_succ_split_inter_ri, 
                                ~ grupo * agent,
                                type = "response")

pairs(em_posthoc_m2_succes)


slopes_success_m2 <- emtrends(m2_succ_split_inter_ri,
                              ~ grupo*agent,
                              var = "c.reward")
pairs(slopes_success_m2)

df_success_m2 <- as.data.frame(em_posthoc_m2_succes)





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
    breaks = c(0,1),
    labels = c("Self", "Other")
  ) +
  scale_color_manual(
    values = c("0" = "#1F77B4", "1" = "#D62728"), # Puedes ajustar los colores aquí
    labels = c("0" = "Control", "1" = "Vulnerable"),
    name = "Grupo"
  ) +
  labs(
    x = "Agente",
    y = "Probabilidad predicha de fallo",
    color = "Grupo"
  ) +
  theme_light(base_size = 14)


ggplot(df_success_m2, aes(x = factor(agent), y = prob, fill = grupo)) +
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
    labels = c("0" = "Self", "1" = "Other")
  ) +
  scale_fill_manual(
    values = c("0" = "#1F77B4", "1" = "#D62728"),
    labels = c("0" = "Control", "1" = "Vulnerable"),
    name = "Grupo"
  ) +
  labs(
    x = "Agente",
    y = "Probabilidad predicha de fallo"
  ) +
  
  theme_classic(base_size = 14)











