

## Extracción de columnas de Fatiga y NASA desde datos_limpios.csv
## Filtra por sujetos presentes en dataset_full.csv y anexa grupo


## Librerías

library(dplyr)


#########################
## Data

# Referencia de sujetos (ID y grupo) desde dataset_full
ref <- read.csv("dataset_full.csv", stringsAsFactors = FALSE) %>% select(ID = sub, grupo)

# Extrae columnas de interés, calza con ref, calcula NASA_diff y ordena por ID
read.csv("Datos/datos_limpios.csv", stringsAsFactors = FALSE) %>%
  select(ID = ID_check, Fatigue_pre_7, Fatigue_post_7, NASA_effort_easy_4, NASA_effort_hard_4) %>%
  inner_join(ref, by = "ID") %>%
  mutate(NASA_diff = NASA_effort_hard_4 - NASA_effort_easy_4) %>%
  mutate(Fatigue_diff = Fatigue_post_7 - Fatigue_pre_7) %>%
  select(ID, grupo, everything()) %>%
  arrange(ID) %>%
  write.csv("Datos/fatigue_nasa.csv", row.names = FALSE)
