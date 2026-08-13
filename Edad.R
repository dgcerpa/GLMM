
# Sexo y Edad
## Diego Garrido Cerpa - Viña del Mar 2026

## Librerías

library(tidyverse)


## Descriptivos demográficos

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

df_grupo_c = subset(df, grupo == "0")
df_grupo_v = subset(df, grupo == "1")

# Edad: media y desviación estándar
mean(df_grupo_c$Edad, na.rm = TRUE)   # M
sd(df_grupo_c$Edad, na.rm = TRUE)     # DE

# Conteo por sexo
table(df_grupo_c$Sexo)

# Edad: media y desviación estándar
mean(df_grupo_v$Edad, na.rm = TRUE)   # M
sd(df_grupo_v$Edad, na.rm = TRUE)     # DE

# Conteo por sexo
table(df_grupo_v$Sexo)
