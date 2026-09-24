
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











## ------------------------------------------------------------------
## Años de escolaridad: extraer desde Participantes.xlsx,
## unir a dataset_final.csv por sub (= columna CÓDIGO) y describir
## ------------------------------------------------------------------

library(readxl)
library(dplyr)

RUTA_XLSX <- "Participantes.xlsx"     # ajusta la ruta si es necesario
RUTA_CSV  <- "dataset_final.csv"

# Selecciona una columna por patrón (tolera tildes, mayúsculas y espacios finales)
pick <- function(df, patron) {
  hit <- grep(patron, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) stop("No encuentro columna con patrón: ", patron)
  hit[1]
}

# El CÓDIGO viene con punto como separador de miles en algunos casos (0.849 = 849)
norm_cod <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(x) & x < 10, round(x * 1000), round(x))
}

leer_hoja <- function(hoja, patron_cod) {
  d <- read_excel(RUTA_XLSX, sheet = hoja)
  tibble(sub          = norm_cod(d[[pick(d, patron_cod)]]),
         Anos_estudio = suppressWarnings(as.numeric(d[[pick(d, "a.os de estudio")]]))) %>%
    filter(!is.na(sub))
}

educ <- bind_rows(leer_hoja("Vulnerable", "^C.DIGO"),
                  leer_hoja("Control",    "^C.digo de participante")) %>%
  distinct(sub, .keep_all = TRUE)

# Unir al dataset final (solo sub y años de estudio: sin nombres, correos ni RUT)
df <- read.csv(RUTA_CSV, stringsAsFactors = FALSE)
df$Anos_estudio <- educ$Anos_estudio[match(df$sub, educ$sub)]

cat("Participantes sin calce de código:", sum(!df$sub %in% educ$sub), "\n")   # esperado: 0
cat("Participantes sin dato de escolaridad:", sum(is.na(df$Anos_estudio)), "\n")  # esperado: 3

# Respaldo y guardado
file.copy(RUTA_CSV, "dataset_final_backup.csv", overwrite = TRUE)
write.csv(df, RUTA_CSV, row.names = FALSE)

## ---- Descriptivos y comparación entre grupos ----
descr_educ <- df %>%
  group_by(grupo) %>%
  summarise(n_con_dato = sum(!is.na(Anos_estudio)),
            M  = mean(Anos_estudio, na.rm = TRUE),
            SD = sd(Anos_estudio,   na.rm = TRUE), .groups = "drop")
print(as.data.frame(descr_educ))

tt_educ <- t.test(Anos_estudio ~ grupo, data = df)   # Welch
print(tt_educ)

x0 <- na.omit(df$Anos_estudio[df$grupo == 0]); x1 <- na.omit(df$Anos_estudio[df$grupo == 1])
sp <- sqrt(((length(x0) - 1) * var(x0) + (length(x1) - 1) * var(x1)) /
             (length(x0) + length(x1) - 2))
cat(sprintf("Cohen's d = %.2f\n", (mean(x0) - mean(x1)) / sp))
