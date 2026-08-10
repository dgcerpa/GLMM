
# Data cleaning

#########################
# Data

# Import Data
glm <- read.csv("Datos/datos_long_glmm_full.csv", header = T)


# Data Cleaning: filter subjects with omitions (>25%) OTHER nor SELF
glm_filtred <- glm[glm$zeros_OTHER <= 25, ]
glm_filtred <- glm_filtred[glm_filtred$zeros_SELF <=25, ]


# Transform variables
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


# Estandarize (z-score) every column with prefix c.
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


# Exclude omited trials and delete index column
alldata.sc <- subset(alldata.sc, decision!= 2)

# Save Data
write.csv(alldata.sc, "datos_long_glmm_filtrados.csv")



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


