



library(tidyverse)
library(readxl)
library(lme4)
library(readxl)
library(car)
library(ggeffects)
library(emmeans)
library(performance)





glm <- read.csv("datos_long_models_coni.csv", header = T)


glm_filtred <- glm[glm$zeros_OTHER <= 25, ] 
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]
glm_filtred <- subset(glm_filtred, decision!= 2)


#Change the tipe of the variable

str(glm)

alldata <- data.frame(as.factor(glm_filtred$sub),
                      as.factor(glm_filtred$decision),
                      as.factor(glm_filtred$grupo),
                      as.factor(glm_filtred$agent),
                      as.numeric(as.character(glm_filtred$success)),
                      as.numeric(as.character(glm_filtred$zeros_OTHER)),
                      as.numeric(as.character(glm_filtred$zeros_SELF)),
                      as.numeric(as.character(glm_filtred$reward)),
                      as.numeric(as.character(glm_filtred$effort)),
                      as.numeric(as.character(glm_filtred$trial))
)

colnames(alldata)<-c("sub", "decision", "grupo", "agent", "success", 
                     "c.zeros_other", "c.zeros_self", "c.reward", "c.effort", "c.trial")


numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])

### GLMM decision ####
#mx = numero del modelo
#dec = decision
#full_inter = todas las interacciones
#split_inter = interacciones separadas 
#rs = randon slope
#ri = random intercep

# Este modelo observa todas la interacciones, con ramdon effect
m1_dec_full_inter_rs <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1 + c.effort + c.reward|sub)
                              ,data=alldata.sc,
                              family=binomial,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=2e5)))
#Los resultados de este modelo entregan un efecto significativo en 
#reward, agent, effort*agent*grupo


# Este modelo observa todas la interacciones, sin ramdon effect
m2_dec_full_inter_ri <- glmer(decision ~ c.reward*agent*c.effort*grupo + (1|sub)
                              ,data = alldata.sc,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun=2e5)))
#Los resultados de este modelo entregan un efecto significativo en 
#reward, agent, effort, grupo, agent*grupo, agent*effort*grupo



#  Este modelo separa las interacciones por reward y effort 
m3_dec_split_inter_rs <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub)
                               ,data=alldata.sc,
                               family=binomial,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl=list(maxfun=2e5)))


#Los resultados de este mdelo entregan un efecto significativo en 
#reward, agent, grupo y agent*grupo*effort

m4_dec_split_inter_rs_agent  <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward + agent|sub)
                                      ,data=alldata.sc,
                                      family=binomial,
                                      control = glmerControl(optimizer = "bobyqa",
                                                             optCtrl=list(maxfun=2e5)))
#Los resutlados de este modelo entregan un efecto singificativo en 
#c.reward, agent, c.reward:agent, agent:c.effort y agent:grupo:c.effort


#   Este modelo separa las interacciones por reward y effort, es sin efectos ramdons
m5_dec_split_inter_ri <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1|sub)
                               ,data=alldata.sc,
                               family=binomial,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
#Los resultados de este modelo entregan un efecto significativo en 
#reward, agent, grupo, effort, reward*agent, agent*grupo, agent*grupo*effort



modelo_comp_dec <- anova(m1_dec_full_inter_rs, m2_dec_full_inter_ri, m3_dec_split_inter_rs, m4_dec_split_inter_rs_agent, m5_dec_split_inter_ri)
#Al comparar los modelos, gana el m3_dec_split_inter_rs 


# 
# plot(ggpredict(m4_dec_split_inter_rs_agent, terms = c("c.effort", "agent", "grupo"))) +
#   ggtitle("Modelo 4 simplificado SIN grupo Effort × Agent") +
#   ylab("Probabilidad predicha de decisión") +
#   xlab("Nivel de esfuerzo (c.effort)") +
#   theme_minimal()


plot(ggpredict(m3_dec_split_inter_rs, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 3") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()















### POST HOC DECISON####

#Se crea una lista con los valores de effort
niveles_esfuerzo <- list(c.effort = c(-1.3412537, -0.4459018, 0.4494501, 1.3448020))


# Calculamos las medias marginales
em_posthoc_m3_decision <- emmeans(m3_dec_split_inter_rs, 
                                  ~  grupo*agent | c.effort, 
                                  at = niveles_esfuerzo,
                                  type = "response") #este formato compara por niveles de esfuerzo 

pairs(em_posthoc_m3_decision)


#Segundo forma de realizar el post hoc comparando pendientes 
slopes_effort_m3_decision <- emtrends(m3_dec_split_inter_rs,
                                      ~  agent * grupo,
                                      var = "c.effort")#este formato compara por pendiente 
pairs(slopes_effort_m3_decision)




# Comparación de niveles de Esfuerzo por Agente
# Objetivo: Ver diferencias entre niveles de esfuerzo DENTRO de cada agente.
effort_contrast_agent <- emmeans(m3_dec_split_inter_rs, 
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
agent_contrast_effort <- emmeans(m3_dec_split_inter_rs, 
                                 pairwise ~ agent | c.effort, 
                                 at = niveles_esfuerzo, 
                                 type = "response",
                                 pbkrtest.limit = 5012, lmerTest.limit = 5021)

summary(agent_contrast_effort)


# Comparación de Grupos por Agente
# Ver si el Grupo Control difiere del Vulnerable mirando a cada agente por separado
# (Promediando a través de los niveles de esfuerzo, a menos que especifiques 'at').
grupo_contrast <- emmeans(m3_dec_split_inter_rs, 
                          pairwise ~ grupo | agent,
                          at = niveles_esfuerzo,
                          type = "response",
                          pbkrtest.limit = 5012, lmerTest.limit = 5021)
summary(grupo_contrast)







#graficos DECISION

df_decision_m3 <- as.data.frame(em_posthoc_m3_decision)

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














### GLMM success ####

alldata.sc_a <- subset(alldata.sc, decision!= 0)


#Este modelo observa las interacciones en su conjunto,
m1_succ_split_inter_rs <-glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1 + c.effort + c.reward|sub)
                               ,data=alldata.sc_a,
                               family=binomial,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
#Los resultados de este modelo entregan efecto en agente y effort



#En este modelo se separan las interacciones por reward y effort, además de que no se tienen efectos ramdon
m2_succ_split_inter_ri <-glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1|sub)
                               ,data=alldata.sc_a,
                               family=binomial,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
#Los resultados de este entregan un efecto en agente, effort, agente*grupo



#En este modelo se separan las interacciones por reward y effort, además de que no se tienen efectos ramdon
m3_succ_full_inter_rs <-glmer(success ~ c.reward*agent*c.effort*grupo + (1 + c.reward + c.effort|sub)
                              ,data=alldata.sc_a,
                              family=binomial,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=2e5)))
#Los resultados de este modelo entregan un efecto en agente, effort, reward*effort, agente*grupo


#Este modelo observa las interacciones en su conjunto, no presenta efectos ramdon
m4_succ_full_inter_ri <-glmer(success ~ c.reward*agent*c.effort*grupo + (1|sub)
                              ,data=alldata.sc_a,
                              family=binomial,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=2e5)))
#Los resultados del modelo entrega interacciones en effort, grupo, agent*grupo
#reward*effort*grupo




model_comp_success <- anova(m1_succ_split_inter_rs, m2_succ_split_inter_ri, m3_succ_full_inter_rs, m4_succ_full_inter_ri)
#Al comparar los modelos, gana el m1_succ_split_inter_rs


plot(ggpredict(m2_succ_split_inter_ri, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 2") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  scale_colour_discrete(labels = c("0" = "Self", "1" = "Other"), name = "Agente") +
  facet_wrap(~facet, labeller = labeller(facet = c("0" = "Control", "1" = "Experimental"))) +
  theme_minimal()








# #Grafico de los modelos success 
# ggplot(ggpredict(m4_succ_full_inter_ri, c("grupo","agent")
#           + theme_classic(base_size = 14)
#           )) %>% plot()














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







