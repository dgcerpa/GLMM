

## GLMM: Modelo mixto generalizado
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)
library(lme4)       # glmer()
library(ggeffects)  # ggpredict()
library(emmeans)    # emtrends(), pairs()




######################################
## Importar datos

alldata.sc <- read.csv("Datos/datos_long_glmm_filtrados.csv", header = T)


# Excluir trials omitidos y eliminar columna índice
alldata.sc <- subset(alldata.sc, decision!= 2)
alldata.sc <- subset(alldata.sc, select = -c(X))




###################################
### GLMM decision ####


## Modelo 3: efectos de reward y effort separados con slopes aleatorios
m3 <- glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub),
            data=alldata.sc,
            family=binomial,
            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))



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

