
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




####################################################
## GRÁFICOS PARA REPORTAR

library(ggplot2)

# Carpeta de salida
dir.create("plots_mediacion", showWarnings = FALSE)

# Tema base consistente para los 3 gráficos
theme_med <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.minor = element_blank())


# ============ Gráfico 1: Forest de los 4 modelos ============
# Vista panorámica de todos los efectos indirectos con sus IC95%

plot1_data <- tabla %>%
  mutate(
    sig           = sign(ci.lower) == sign(ci.upper),
    label_display = paste0(modelo, ": ", label),
    label_display = factor(label_display, levels = rev(label_display))
  )

p1 <- ggplot(plot1_data, aes(x = est, y = label_display, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = ci.lower, xmax = ci.upper), size = 0.7) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#1D9E75"),
                     labels = c("FALSE" = "ns", "TRUE" = "IC95% no cruza 0"),
                     name = "") +
  labs(x = "Estimado (IC95% BCa)", y = NULL,
       title = "Efectos indirectos de los 4 modelos de mediación",
       subtitle = sprintf("N = %d, bootstrap %s muestras",
                          nrow(df_med), format(N_BOOT, big.mark = ","))) +
  theme_med +
  theme(legend.position = "bottom")

print(p1)
ggsave("plots_mediacion/fig1_forest_todos_modelos.png", p1,
       width = 10, height = 5, dpi = 150)


# ============ Gráfico 2: Hayes 14 (resultado central) ============
# Acerca el zoom al Modelo 4 para destacar el índice MM y los efectos
# indirectos condicionales por grupo

hayes_data <- tabla %>%
  filter(modelo == "4. Hayes 14") %>%
  mutate(
    label_clean = factor(case_when(
      label == "ind_control"    ~ "Indirecto en Control",
      label == "ind_vulnerable" ~ "Indirecto en Vulnerable",
      label == "index_MM"       ~ "Índice MM (diferencia)"
    ), levels = rev(c("Indirecto en Control",
                      "Indirecto en Vulnerable",
                      "Índice MM (diferencia)"))),
    sig = sign(ci.lower) == sign(ci.upper)
  )

p2 <- ggplot(hayes_data, aes(x = est, y = label_clean, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = ci.lower, xmax = ci.upper), size = 1.1) +
  geom_text(aes(label = sprintf("%.3f [%.3f, %.3f]", est, ci.lower, ci.upper)),
            vjust = -1.3, color = "gray20", size = 3.1) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#1D9E75"),
                     labels = c("FALSE" = "ns", "TRUE" = "IC95% no cruza 0"),
                     name = "") +
  labs(x = "Estimado (IC95% BCa)", y = NULL,
       title = "Modelo 4: Mediación moderada (Hayes 14) vía SASS",
       subtitle = "Efectos indirectos condicionales por grupo + índice MM") +
  theme_med +
  theme(legend.position = "bottom")

print(p2)
ggsave("plots_mediacion/fig2_hayes14_condicionales.png", p2,
       width = 10, height = 4.5, dpi = 150)


# ============ Gráfico 3: Camino b moderado ============
# El "por qué" del Modelo 4: las pendientes SASS → diff_effort
# operan en direcciones opuestas en cada grupo, lo que produce
# efectos indirectos opuestos en signo

m_b <- lm(diff_effort ~ SASS_DIRt * grupo, data = df_med)

pred_data <- expand.grid(
  SASS_DIRt = seq(min(df_med$SASS_DIRt), max(df_med$SASS_DIRt),
                  length.out = 100),
  grupo = c(0, 1)
)
pred_data$pred        <- predict(m_b, newdata = pred_data)
pred_data$grupo_label <- factor(pred_data$grupo, levels = c(0, 1),
                                labels = c("Control", "Vulnerable"))

df_plot <- df_med %>%
  mutate(grupo_label = factor(grupo, levels = c(0, 1),
                              labels = c("Control", "Vulnerable")))

p3 <- ggplot() +
  geom_point(data = df_plot,
             aes(x = SASS_DIRt, y = diff_effort, color = grupo_label),
             alpha = 0.55, size = 2) +
  geom_line(data = pred_data,
            aes(x = SASS_DIRt, y = pred, color = grupo_label),
            linewidth = 1.3) +
  scale_color_manual(values = c("Control" = "#4393C3",
                                "Vulnerable" = "#D6604D"),
                     name = "Grupo") +
  labs(x = "SASS (adaptación social)", y = "diff_effort",
       title = "Camino b moderado por grupo: SASS → diff_effort",
       subtitle = "Pendientes opuestas explican por qué el índice MM es positivo") +
  theme_med +
  theme(legend.position = "bottom")

print(p3)
ggsave("plots_mediacion/fig3_camino_b_moderado.png", p3,
       width = 9, height = 6, dpi = 150)


cat("\n", strrep("=", 60), "\n", sep = "")
cat("Gráficos guardados en ./plots_mediacion/\n")
cat("  fig1_forest_todos_modelos.png   - vista panorámica\n")
cat("  fig2_hayes14_condicionales.png  - resultado central\n")
cat("  fig3_camino_b_moderado.png      - mecanismo: pendientes opuestas\n")
cat(strrep("=", 60), "\n", sep = "")
