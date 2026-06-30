# =============================================================
# Actualizar dataset_full_final.csv con columnas de modelos 2k1b
#
# Pasos:
#   1. Eliminar columnas p_2k1b_k_self, p_2k1b_k_other,
#      p_2k1b_beta y p_2k1b_diff_k del dataset original.
#   2. Agregar columnas del script NUEVO (2k1b_modelos.csv) con
#      sufijo "_new".
#   3. Agregar columnas del script VIEJO
#      (params_2k1b_all_families.xlsx) con sufijo "_old".
# =============================================================

library(readr)   # read_csv / write_csv
library(readxl)  # read_excel
library(dplyr)   # manipulacion de datos

# --- Rutas ---------------------------------------------------
# Ajusta el working directory a la carpeta del proyecto si hace falta:
# setwd("C:/Users/yangy/Desktop/GLMM")

ruta_dataset <- "dataset_full_final.csv"
ruta_modelos <- "Datos/Modelos Comp/2k1b_modelos.csv"
ruta_params  <- "Datos/Modelos Comp/params_2k1b_all_families.xlsx"
ruta_salida  <- "dataset_full_final.csv"   # sobrescribe el original

# --- 1. Leer dataset y ELIMINAR columnas ---------------------
dataset <- read_csv(ruta_dataset, show_col_types = FALSE)

cols_a_eliminar <- c("p_2k1b_k_self", "p_2k1b_k_other",
                     "p_2k1b_beta", "p_2k1b_diff_k")

dataset <- dataset %>% select(-any_of(cols_a_eliminar))

# --- 2. Columnas del script NUEVO (sufijo "_new") ------------
modelos <- read_csv(ruta_modelos, show_col_types = FALSE)

# Prefijos solicitados desde 2k1b_modelos.csv
prefijos_new <- c("p_self_k", "p_other_k", "p_beta", "p_other_self_k",
                  "h_self_k", "h_other_k", "h_beta", "h_other_self_k")

cols_new <- names(modelos)[
  vapply(names(modelos),
         function(x) any(startsWith(x, prefijos_new)),
         logical(1))
]

modelos_sel <- modelos %>%
  select(ui, all_of(cols_new)) %>%       # 'ui' es la llave (= 'sub')
  rename_with(~ paste0(.x, "_new"), all_of(cols_new)) %>%
  rename(sub = ui)

# --- 3. Columnas del script VIEJO (sufijo "_old") ------------
params <- read_excel(ruta_params)

# Columnas que comienzan con "p_" o "h_"
cols_old <- names(params)[startsWith(names(params), "p_") |
                          startsWith(names(params), "h_")]

params_sel <- params %>%
  select(subject_id, all_of(cols_old)) %>%   # 'subject_id' es la llave (= 'sub')
  rename_with(~ paste0(.x, "_old"), all_of(cols_old)) %>%
  rename(sub = subject_id)

# --- 4. Unir todo por la llave 'sub' -------------------------
dataset_final <- dataset %>%
  left_join(modelos_sel, by = "sub") %>%
  left_join(params_sel,  by = "sub")

# --- 5. Guardar ----------------------------------------------
write_csv(dataset_final, ruta_salida)

cat("Listo. Filas:", nrow(dataset_final),
    "| Columnas:", ncol(dataset_final), "\n")
cat("Nuevas columnas (_new):\n"); print(paste0(cols_new, "_new"))
cat("Nuevas columnas (_old):\n"); print(paste0(cols_old, "_old"))
