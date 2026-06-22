
## Análisis de mediación con bootstrap (toda la muestra)
## Diego Garrido Cerpa - Viña del Mar 2026
##
## Cuatro modelos:
##   1. Mediación simple grupo → MAIA → diff_effort
##   2. Mediación simple grupo → SASS → diff_effort
##   3. Mediación paralela grupo → {MAIA, SASS} → diff_effort
##   4. Mediación moderada (Hayes 14) vía SASS, grupo modera camino b
##
## Estimación: lavaan con bootstrap percentil-corregido (BCa), 5000 muestras.
## El "efecto indirecto" significativo se determina por IC95% que NO incluye 0.


## Librerías

library(lavaan)
library(dplyr)


#########################
## Data

df <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)

# Variables del análisis y casos completos
df_med <- df %>%
  select(grupo, diff_effort, MAIA_DIRt, SASS_DIRt) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

cat("N total:", nrow(df_med), "\n")
print(table(df_med$grupo))

# Configuración global de bootstrap (puede ajustarse)
N_BOOT <- 5000
SEED   <- 42




####################################################
## MODELO 1: Mediación simple grupo → MAIA → diff_effort

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO 1: grupo → MAIA → diff_effort\n")
cat(strrep("=", 60), "\n", sep = "")

mod1 <- '
  # Camino a: grupo predice al mediador
  MAIA_DIRt ~ a*grupo

  # Camino b: mediador predice VD ; cp: efecto directo de grupo
  diff_effort ~ b*MAIA_DIRt + cp*grupo

  # Efectos definidos
  indirect := a*b
  total    := cp + (a*b)
'

set.seed(SEED)
fit1 <- sem(mod1, data = df_med, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen del modelo ---\n")
print(summary(fit1, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit1, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




####################################################
## MODELO 2: Mediación simple grupo → SASS → diff_effort

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO 2: grupo → SASS → diff_effort\n")
cat(strrep("=", 60), "\n", sep = "")

mod2 <- '
  SASS_DIRt ~ a*grupo
  diff_effort ~ b*SASS_DIRt + cp*grupo

  indirect := a*b
  total    := cp + (a*b)
'

set.seed(SEED)
fit2 <- sem(mod2, data = df_med, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen del modelo ---\n")
print(summary(fit2, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit2, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




####################################################
## MODELO 3: Mediación paralela grupo → {MAIA, SASS} → diff_effort

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO 3: Mediación paralela con MAIA y SASS\n")
cat(strrep("=", 60), "\n", sep = "")

mod3 <- '
  # Caminos a (de grupo a cada mediador)
  MAIA_DIRt ~ a1*grupo
  SASS_DIRt ~ a2*grupo

  # Caminos b + efecto directo
  diff_effort ~ b1*MAIA_DIRt + b2*SASS_DIRt + cp*grupo

  # Los mediadores pueden estar correlacionados
  MAIA_DIRt ~~ SASS_DIRt

  # Efectos indirectos específicos y total
  ind_MAIA       := a1*b1
  ind_SASS       := a2*b2
  total_indirect := (a1*b1) + (a2*b2)
  total          := cp + total_indirect
'

set.seed(SEED)
fit3 <- sem(mod3, data = df_med, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen del modelo ---\n")
print(summary(fit3, ci = TRUE, standardized = TRUE))

cat("\n--- Efectos indirectos específicos (IC95% BCa) ---\n")
print(parameterEstimates(fit3, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a1", "a2", "b1", "b2", "cp",
                            "ind_MAIA", "ind_SASS", "total_indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




####################################################
## MODELO 4: Mediación moderada Hayes 14
## grupo → SASS → diff_effort, con grupo moderando el camino b
## ¿La fuerza del efecto indirecto a*b difiere entre Control y Vulnerable?

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO 4: Hayes 14 — grupo modera el camino b\n")
cat(strrep("=", 60), "\n", sep = "")

# Término de interacción (precomputado para lavaan)
df_med$SASS_x_grupo <- df_med$SASS_DIRt * df_med$grupo

mod4 <- '
  # Camino a
  SASS_DIRt ~ a*grupo

  # Camino b moderado por grupo + efecto directo
  diff_effort ~ b*SASS_DIRt + cp*grupo + bW*SASS_x_grupo

  # Efectos indirectos condicionales
  ind_control    := a*b               # grupo = 0 (Control)
  ind_vulnerable := a*(b + bW)        # grupo = 1 (Vulnerable)

  # Índice de mediación moderada
  # (diferencia entre los dos efectos indirectos condicionales)
  index_MM := a*bW
'

set.seed(SEED)
fit4 <- sem(mod4, data = df_med, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen del modelo ---\n")
print(summary(fit4, ci = TRUE, standardized = TRUE))

cat("\n--- Efectos indirectos condicionales e índice MM (IC95% BCa) ---\n")
print(parameterEstimates(fit4, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "bW",
                            "ind_control", "ind_vulnerable", "index_MM")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




####################################################
## Tabla consolidada: efectos indirectos de los 4 modelos

cat("\n", strrep("=", 60), "\n", sep = "")
cat("TABLA CONSOLIDADA — Efectos indirectos\n")
cat(strrep("=", 60), "\n\n", sep = "")

extract_indirect <- function(fit, labels, model_name) {
  pe <- parameterEstimates(fit, boot.ci.type = "bca.simple", level = 0.95)
  pe[pe$label %in% labels,
     c("label", "est", "se", "ci.lower", "ci.upper", "pvalue")] %>%
    mutate(modelo = model_name) %>%
    select(modelo, everything())
}

tabla <- bind_rows(
  extract_indirect(fit1, c("indirect"), "1. Simple vía MAIA"),
  extract_indirect(fit2, c("indirect"), "2. Simple vía SASS"),
  extract_indirect(fit3, c("ind_MAIA", "ind_SASS", "total_indirect"),
                   "3. Paralela"),
  extract_indirect(fit4, c("ind_control", "ind_vulnerable", "index_MM"),
                   "4. Hayes 14")
)

# Marcador visual: ¿el IC95% incluye 0?
tabla$significativo <- ifelse(sign(tabla$ci.lower) == sign(tabla$ci.upper),
                              "✓ (CI no cruza 0)", "ns")

print(tabla, row.names = FALSE)




