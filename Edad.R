## Merge Sexo y Edad
# Diego Garrido Cerpa - Viña del Mar 2026

## Librerías

library(tidyverse)

#########################
## Data

# Cargar datos
df    <- read.csv("Datos/dataset_full_v2.csv", stringsAsFactors = FALSE)
parts <- read.csv("Datos/Participantes_Final_ID.csv", stringsAsFactors = FALSE)

# Seleccionar id, Sexo y Edad (la columna de sexo trae paréntesis)
parts <- parts %>%
  select(id, Sexo = starts_with("Sexo"), Edad)

#########################
## Merge

# Unir por sub (df) = id (parts); se descartan los id sin match
df <- df %>%
  left_join(parts, by = c("sub" = "id"))

# Guardar
write.csv(df, "dataset_full_v2_con_sexo_edad.csv", row.names = FALSE)


#########################
## Descriptivos demográficos

df <- read.csv("dataset_full_v2_con_sexo_edad.csv", stringsAsFactors = FALSE)



# Edad: media y desviación estándar
mean(df$Edad, na.rm = TRUE)   # M
sd(df$Edad, na.rm = TRUE)     # DE

# Conteo por sexo
table(df$Sexo)


