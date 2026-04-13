

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

glm <- read.csv("datos_long_models_coni.csv", header = T)


# Limpieza de datos

glm_filtred <- subset(glm, decision!= 2)

alldata <- data.frame(as.factor(glm_filtred$sub),
                      as.factor(glm_filtred$decision),
                      as.factor(glm_filtred$grupo),
                      as.factor(glm_filtred$agent),
                      as.numeric(as.character(glm_filtred$success)),
                      as.numeric(as.character(glm_filtred$zeros_OTHER)),
                      as.numeric(as.character(glm_filtred$zeros_SELF)),
                      as.numeric(as.character(glm_filtred$reward)),
                      as.numeric(as.character(glm_filtred$effort)),
                      as.numeric(as.character(glm_filtred$trial)))

colnames(alldata)<-c("sub", "decision", "grupo", "agent", "success", 
                     "c.zeros_other", "c.zeros_self", "c.reward", "c.effort", "c.trial")

numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])




# Guardar datos para gráfico 3D MATLAB
export_3d <- alldata.sc %>%
  filter(decision != 2) %>%
  select(sub, decision, c.reward, c.effort, agent, grupo)

write.csv(export_3d, "data_for_3d_matlab.csv", row.names = FALSE)
cat("Guardado: data_for_3d_matlab.csv\n")






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


m4  <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward + agent|sub),
             data=alldata.sc,
             family=binomial,
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


m5 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


m6  <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1 + c.effort + c.reward + agent|sub),
             data=alldata.sc,
             family=binomial,
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))


m7 <- glmer(decision ~ c.reward*c.effort*agent + (1+ c.reward + c.effort + agent|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))


m8 <- glmer(decision ~ c.reward*c.effort*agent*grupo + (1+ c.reward + c.effort + agent|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))


m9 <- glmer(decision ~ c.reward*agent*grupo + c.effort*grupo +  (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))


# Comparación de modelos
modelo_comp_dec <- anova(m1, m2, m3, m4, m5, m6, m7, m8, m9)


# Gráfico del modelo ganador (AIC)
plot(ggpredict(m4, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 4") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()





### POST HOC DECISON####

#Se crea una lista con los valores de effort
niveles_esfuerzo <- list(c.effort = c(-1.3412537, -0.4459018, 0.4494501, 1.3448020))


# Calculamos las medias marginales
em_posthoc_m4_decision <- emmeans(m4_dec_split_inter_rs_agent, 
                                  ~  grupo*agent | c.effort, 
                                  at = niveles_esfuerzo,
                                  type = "response") #este formato compara por niveles de esfuerzo 

pairs(em_posthoc_m4_decision)


#Segundo forma de realizar el post hoc comparando pendientes 
slopes_effort_m4_decision <- emtrends(m4_dec_split_inter_rs_agent,
                                      ~  agent * grupo,
                                      var = "c.effort") #este formato compara por pendiente 
pairs(slopes_effort_m4_decision)




# Comparación de niveles de Esfuerzo por Agente
# Objetivo: Ver diferencias entre niveles de esfuerzo DENTRO de cada agente.
effort_contrast_agent <- emmeans(m4_dec_split_inter_rs_agent, 
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
agent_contrast_effort <- emmeans(m4_dec_split_inter_rs_agent, 
                                 pairwise ~ agent | c.effort, 
                                 at = niveles_esfuerzo, 
                                 type = "response",
                                 pbkrtest.limit = 5012, lmerTest.limit = 5021)

summary(agent_contrast_effort)


# Comparación de Grupos por Agente
# Ver si el Grupo Control difiere del Vulnerable mirando a cada agente por separado
# (Promediando a través de los niveles de esfuerzo, a menos que especifiques 'at').
grupo_contrast <- emmeans(m1, 
                          pairwise ~ grupo | agent,
                          at = niveles_esfuerzo,
                          type = "response",
                          pbkrtest.limit = 5012, lmerTest.limit = 5021)
summary(grupo_contrast)




#graficos DECISION

df_decision_m4 <- as.data.frame(em_posthoc_m4_decision)

ggplot(
  df_decision_m4,
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


































# dentro del grupo experimental
data_exp_self  <- alldata.sc %>% filter(grupo == 1, agent == 0)
data_exp_other <- alldata.sc %>% filter(grupo == 1, agent == 1)

m_exp_self <- glmer(decision ~ c.reward * c.effort + (1 + c.effort + c.reward | sub),
                    data = data_exp_self, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

plot(ggpredict(m_exp_self, terms = c("c.effort", "c.reward [-1, 0, 1]"))) +
  ggtitle("Experimental - Self") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward") +
  theme_minimal()




m_exp_other <- glmer(decision ~ c.reward * c.effort + (1 + c.effort + c.reward | sub),
                     data = data_exp_other, family = binomial,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


plot(ggpredict(m_exp_other, terms = c("c.effort", "c.reward [-1, 0, 1]"))) +
  ggtitle("Experimental - Other") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward") +
  theme_minimal()


# Lo mismo para control
data_ctrl_self  <- alldata.sc %>% filter(grupo == 0, agent == 0)
data_ctrl_other <- alldata.sc %>% filter(grupo == 0, agent == 1)

m_ctrl_self <- glmer(decision ~ c.reward * c.effort + (1 + c.effort + c.reward | sub),
                     data = data_ctrl_self, family = binomial,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


plot(ggpredict(m_ctrl_self, terms = c("c.effort", "c.reward [-1, 0, 1]"))) +
  ggtitle("Control - Self") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward") +
  theme_minimal()


m_ctrl_other <- glmer(decision ~ c.reward * c.effort + (1 + c.effort + c.reward | sub),
                      data = data_ctrl_other, family = binomial,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


plot(ggpredict(m_ctrl_other, terms = c("c.effort", "c.reward [-1, 0, 1]"))) +
  ggtitle("Control - Other") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward") +
  theme_minimal()






p <- ggpredict(m1, terms = c("c.effort", "c.reward [-1, 0, 1]", "agent", "grupo"))

plot(p) +
  ggtitle("Interacción de 4 vías: Effort × Reward × Agent × Grupo") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward"
  ) +
  scale_fill_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward"
  ) +
  facet_grid(facet ~ panel, 
             labeller = labeller(
               panel = c("0" = "Control", "1" = "Experimental"),
               facet = c("0" = "Self", "1" = "Other")
             )) +
  theme_minimal()




































#estandariza las variables de interes" 
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


#data slope vulnerable 
data_control <- subset(alldata.sc, grupo!="1")

#data slope control
data_vulnerable <- subset(alldata.sc, grupo!="0")  



#data slope self
data_other<- subset(alldata.sc, agent!="0")


#data slope other 
data_self<- subset(alldata.sc, agent!="1")



# Modelos 


# Modelo de vulnerabilidad 
lmcontrol <- glmer(decision ~ c.reward*agent*c.effort + (1 + c.effort + c.reward|sub),
                   data=data_control,
                   family=binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl=list(maxfun=2e5)))


# Modelo grupo control 
lmvulnerable <- glmer(decision ~ c.reward*agent*c.effort + (1 + c.effort + c.reward|sub),
                      data=data_vulnerable,
                      family=binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl=list(maxfun=2e5)))


# Modelo de self
lmself<-glmer(decision ~ c.reward*grupo*c.effort + (1 + c.effort + c.reward|sub),
              data=data_self,
              family=binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl=list(maxfun=2e5)))



# Modelo de other
lmother <-glmer(decision ~ c.reward*grupo*c.effort + (1 + c.effort + c.reward|sub),
                data=data_other,
                family=binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun=2e5)))




plot(ggpredict(lmother, terms = c("c.effort", "c.reward [-1, 0, 1]", "grupo"))) +
  ggtitle("Modelo Other") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward"
  ) +
  scale_fill_discrete(
    labels = c("-1" = "Reward bajo", "0" = "Reward medio", "1" = "Reward alto"),
    name = "Reward"
  ) +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()



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








