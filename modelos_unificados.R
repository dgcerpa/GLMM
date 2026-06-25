# ============================================================================
# MODELOS UNIFICADOS
# Diego Garrido Cerpa - Viña del Mar 2026
#
# Consolidación, sin redundancias, de los modelos contenidos en:
#   - analisis_correlaciones_parciales.R
#   - analisis_correlaciones_parciales_VI.R   (IDÉNTICO al anterior; ver nota)
#   - analisis_mediacion.R
#   - analisis_mediacion_v2.R
#
# NOTA DE REDUNDANCIA:
#   El script "analisis_correlaciones_parciales_VI.R" es matemáticamente
#   idéntico a "analisis_correlaciones_parciales.R": las correlaciones
#   parciales son simétricas, por lo que tratar diff_effort como VD o como VI
#   no altera los valores de r, IC ni p. La única diferencia es conceptual
#   (etiquetado y dirección textual). Por eso aquí queda UN solo modelo.
#
# Variables del dataset (dataset_full_final.csv):
#   - grupo        : 0 = Control, 1 = Vulnerable
#   - diff_effort  : Diferencia de esfuerzo (variable dependiente principal)
#   - MAIA_DIRt    : Interocepción (puntaje total)
#   - SASS_DIRt    : Adaptación social (puntaje total)
#
# Modelos contenidos:
#   CP1 — Correlaciones parciales (N total)
#   CP2 — Correlaciones parciales por grupo (Control vs Vulnerable)
#   CP3 — Comparación de aristas entre grupos (Fisher's z)
#   M1  — Mediación simple grupo → MAIA → diff_effort
#   M2  — Mediación simple grupo → SASS → diff_effort
#   M3  — Mediación paralela grupo → {MAIA, SASS} → diff_effort
#   M4  — Mediación moderada (Hayes 14): grupo modera el camino b vía SASS
#   M5  — Mediación simple MAIA → SASS → diff_effort (sólo Vulnerables)
# ============================================================================


## Librerías

library(ppcor)      # correlaciones parciales (pcor.test)
library(lavaan)     # SEM con bootstrap
library(dplyr)
library(ggplot2)
library(ggforce)    # geom_circle (gráficos GGM)
library(patchwork)  # combinar gráficos GGM lado a lado


## Datos

df <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)

# Subset completo (para CP1, CP2, CP3 y M1-M4)
df_full <- df %>%
  select(grupo, diff_effort, MAIA_DIRt, SASS_DIRt) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Subset por grupo (para CP2, CP3 y M5)
df_ctrl <- na.omit(df[df$grupo == 0, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")])
df_vuln <- na.omit(df[df$grupo == 1, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")])

# Configuración global de bootstrap
N_BOOT <- 5000
SEED   <- 42

cat("N total:", nrow(df_full),
    "  |  Control:", nrow(df_ctrl),
    "  |  Vulnerable:", nrow(df_vuln), "\n")




# ============================================================================
# MODELO CP1 — CORRELACIONES PARCIALES (N total)
# ============================================================================
# Qué hace:
#   Calcula correlaciones parciales de Pearson entre los pares de variables
#   {diff_effort, MAIA_DIRt, SASS_DIRt}, controlando en cada caso por la
#   tercera variable. Devuelve r, IC95% aproximado (Fisher) y p.
#
# Interpretación:
#   Aristas del modelo gráfico gaussiano (GGM). Una arista significativa
#   indica asociación parcial neta entre dos variables tras descontar
#   el efecto de la tercera.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO CP1 — Correlaciones parciales (N total)\n")
cat(strrep("=", 60), "\n\n", sep = "")

sub <- df_full[, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")]
cat("N =", nrow(sub), "\n\n")

cat("--- Correlaciones de Pearson ---\n")
print(round(cor(sub), 4))

cat("\n--- Correlaciones parciales (aristas GGM) ---\n\n")

ci_pc <- function(r, n) 1.96 * sqrt((1 - r^2) / (n - 3))

r1 <- pcor.test(sub$diff_effort, sub$MAIA_DIRt, sub$SASS_DIRt)
cat("diff_effort — MAIA_DIRt (ctrl SASS):\n")
cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
            r1$estimate,
            r1$estimate - ci_pc(r1$estimate, nrow(sub)),
            r1$estimate + ci_pc(r1$estimate, nrow(sub)),
            r1$p.value))

r2 <- pcor.test(sub$diff_effort, sub$SASS_DIRt, sub$MAIA_DIRt)
cat("diff_effort — SASS_DIRt (ctrl MAIA):\n")
cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
            r2$estimate,
            r2$estimate - ci_pc(r2$estimate, nrow(sub)),
            r2$estimate + ci_pc(r2$estimate, nrow(sub)),
            r2$p.value))

r3 <- pcor.test(sub$MAIA_DIRt, sub$SASS_DIRt, sub$diff_effort)
cat("MAIA_DIRt — SASS_DIRt (ctrl diff_effort):\n")
cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
            r3$estimate,
            r3$estimate - ci_pc(r3$estimate, nrow(sub)),
            r3$estimate + ci_pc(r3$estimate, nrow(sub)),
            r3$p.value))




# ============================================================================
# MODELO CP2 — CORRELACIONES PARCIALES POR GRUPO
# ============================================================================
# Qué hace:
#   Replica CP1 dentro de cada subgrupo (Control y Vulnerable), permitiendo
#   ver si las asociaciones parciales son específicas de un grupo.
#
# Devuelve:
#   data.frame con las 3 aristas (MAIA-SASS, diff-MAIA, diff-SASS) por grupo,
#   alimentado a la función de graficado GGM más abajo.
# ============================================================================

ggm_subgrupo <- function(data, etiqueta) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("CP2 — GRUPO:", etiqueta, "  |  N =", nrow(data), "\n")
  cat(strrep("=", 60), "\n\n", sep = "")

  cat("--- Correlaciones de Pearson ---\n")
  print(round(cor(data), 4))

  cat("\n--- Correlaciones parciales ---\n\n")
  n  <- nrow(data)
  ci <- function(r) 1.96 * sqrt((1 - r^2) / (n - 3))

  r1 <- pcor.test(data$diff_effort, data$MAIA_DIRt, data$SASS_DIRt)
  cat("diff_effort — MAIA_DIRt (ctrl SASS):\n")
  cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
              r1$estimate, r1$estimate - ci(r1$estimate),
              r1$estimate + ci(r1$estimate), r1$p.value))

  r2 <- pcor.test(data$diff_effort, data$SASS_DIRt, data$MAIA_DIRt)
  cat("diff_effort — SASS_DIRt (ctrl MAIA):\n")
  cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
              r2$estimate, r2$estimate - ci(r2$estimate),
              r2$estimate + ci(r2$estimate), r2$p.value))

  r3 <- pcor.test(data$MAIA_DIRt, data$SASS_DIRt, data$diff_effort)
  cat("MAIA_DIRt — SASS_DIRt (ctrl diff_effort):\n")
  cat(sprintf("  r = %.4f  |  IC95%% [%.3f, %.3f]  |  p = %.4f\n\n",
              r3$estimate, r3$estimate - ci(r3$estimate),
              r3$estimate + ci(r3$estimate), r3$p.value))

  data.frame(
    edge  = c("MAIA-SASS", "diff-MAIA", "diff-SASS"),
    r     = c(r3$estimate, r1$estimate, r2$estimate),
    p_val = c(r3$p.value,  r1$p.value,  r2$p.value),
    stringsAsFactors = FALSE
  )
}

res_ctrl <- ggm_subgrupo(df_ctrl, "CONTROL")
res_vuln <- ggm_subgrupo(df_vuln, "VULNERABLE")




# ============================================================================
# MODELO CP3 — COMPARACIÓN DE ARISTAS ENTRE GRUPOS (Fisher's z)
# ============================================================================
# Qué hace:
#   Para cada arista del GGM, compara la correlación parcial obtenida en
#   Control vs Vulnerable mediante una transformación z de Fisher.
#
# Interpretación:
#   Si Z es grande (|Z| > 1.96) y p < .05, la fuerza de la correlación
#   parcial difiere significativamente entre grupos.
# ============================================================================

fisher_z_test <- function(r1, n1, r2, n2) {
  z1 <- 0.5 * log((1 + r1) / (1 - r1))
  z2 <- 0.5 * log((1 + r2) / (1 - r2))
  se <- sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  Z  <- (z1 - z2) / se
  p  <- 2 * (1 - pnorm(abs(Z)))
  c(Z = Z, p = p)
}

n_c <- nrow(df_ctrl); n_v <- nrow(df_vuln)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO CP3 — Comparación entre grupos (Fisher's z)\n")
cat(strrep("=", 60), "\n\n", sep = "")

comp <- data.frame(
  arista       = res_ctrl$edge,
  r_ctrl       = round(res_ctrl$r, 4),
  r_vuln       = round(res_vuln$r, 4),
  Z            = NA_real_,
  p_diferencia = NA_real_
)
for (i in seq_len(nrow(comp))) {
  t <- fisher_z_test(res_ctrl$r[i], n_c, res_vuln$r[i], n_v)
  comp$Z[i]            <- round(t["Z"], 3)
  comp$p_diferencia[i] <- round(t["p"], 4)
}
print(comp, row.names = FALSE)




# ============================================================================
# GRÁFICOS GGM (CP1 muestra total y CP2 por grupo)
# ============================================================================
# Genera la red GGM con tres nodos (MAIA, SASS, diff_effort) y aristas
# coloreadas según significancia. Útil para visualizar CP1 y CP2.
# ============================================================================

plot_ggm <- function(edges_df, n, titulo, etiqueta_diff = "VD") {
  edges <- data.frame(
    x1 = c(200, 340, 340), y1 = c(200, 360, 360),
    x2 = c(480, 200, 480), y2 = c(200, 200, 200),
    r     = edges_df$r,
    p_val = edges_df$p_val,
    sig   = edges_df$p_val < 0.05
  )
  edges$label <- ifelse(edges$sig,
    sprintf("r = %.3f  p = %.3f ✓", edges$r, edges$p_val),
    sprintf("r = %.3f  ns",              edges$r))

  nodes <- data.frame(
    x = c(200, 480, 340), y = c(200, 200, 360),
    label = c("MAIA_DIRt\nInterocepción",
              "SASS_DIRt\nAdapt. social",
              sprintf("diff_effort\n%s", etiqueta_diff)),
    color = c("#7B52AB", "#1D9E75", "#888888")
  )

  edge_labels <- data.frame(
    x = c((200 + 480) / 2, (340 + 200) / 2, (340 + 480) / 2),
    y = c((200 + 200) / 2, (360 + 200) / 2, (360 + 200) / 2),
    label = edges$label, sig = edges$sig
  )

  ggplot() +
    geom_segment(data = edges,
                 aes(x = x1, y = y1, xend = x2, yend = y2,
                     linewidth = ifelse(sig, 2.5, 0.8),
                     linetype  = ifelse(sig, "solid", "dashed"),
                     color     = ifelse(sig, "#1D9E75", "grey60")),
                 show.legend = FALSE) +
    scale_linewidth_identity() +
    geom_label(data = edge_labels,
               aes(x = x, y = y, label = label,
                   color = ifelse(sig, "#0F6E56", "grey50")),
               size = 2.8, label.size = 0.3, fill = "white",
               show.legend = FALSE) +
    geom_point(data = nodes,
               aes(x = x, y = y, color = color),
               size = 24, show.legend = FALSE) +
    scale_color_identity() +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              color = "white", size = 3.0, fontface = "bold",
              lineheight = 0.9) +
    ggtitle(sprintf("%s  (N = %d)", titulo, n)) +
    xlim(20, 560) + ylim(100, 440) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
}

# Gráfico CP1 (N total) — usa los r y p de CP1 ya calculados arriba
edges_total <- data.frame(
  edge  = c("MAIA-SASS", "diff-MAIA", "diff-SASS"),
  r     = c(r3$estimate, r1$estimate, r2$estimate),
  p_val = c(r3$p.value,  r1$p.value,  r2$p.value)
)
p_total <- plot_ggm(edges_total, nrow(sub), "GGM — Muestra total")

# Gráficos CP2 (por grupo)
p_ctrl <- plot_ggm(res_ctrl, nrow(df_ctrl), "Control")
p_vuln <- plot_ggm(res_vuln, nrow(df_vuln), "Vulnerable")

print(p_total)
print(p_ctrl + p_vuln)




# ============================================================================
# MODELO M1 — MEDIACIÓN SIMPLE  grupo → MAIA → diff_effort
# ============================================================================
# Qué hace:
#   SEM con bootstrap percentil-corregido (BCa, 5000 muestras). Estima:
#     a  : efecto de grupo sobre el mediador MAIA_DIRt
#     b  : efecto del mediador sobre diff_effort (controlado por grupo)
#     cp : efecto directo de grupo sobre diff_effort
#     indirect = a*b
#     total    = cp + a*b
#
# Interpretación:
#   El efecto indirecto se considera significativo cuando su IC95% BCa
#   NO incluye cero.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO M1 — grupo → MAIA → diff_effort\n")
cat(strrep("=", 60), "\n", sep = "")

mod1 <- '
  MAIA_DIRt   ~ a*grupo
  diff_effort ~ b*MAIA_DIRt + cp*grupo

  indirect := a*b
  total    := cp + (a*b)
'

set.seed(SEED)
fit1 <- sem(mod1, data = df_full, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen ---\n")
print(summary(fit1, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit1, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




# ============================================================================
# MODELO M2 — MEDIACIÓN SIMPLE  grupo → SASS → diff_effort
# ============================================================================
# Qué hace:
#   Misma especificación que M1 pero usando SASS_DIRt como mediador. Es la
#   "ruta social" equivalente a la ruta interoceptiva de M1.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO M2 — grupo → SASS → diff_effort\n")
cat(strrep("=", 60), "\n", sep = "")

mod2 <- '
  SASS_DIRt   ~ a*grupo
  diff_effort ~ b*SASS_DIRt + cp*grupo

  indirect := a*b
  total    := cp + (a*b)
'

set.seed(SEED)
fit2 <- sem(mod2, data = df_full, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen ---\n")
print(summary(fit2, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit2, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




# ============================================================================
# MODELO M3 — MEDIACIÓN PARALELA  grupo → {MAIA, SASS} → diff_effort
# ============================================================================
# Qué hace:
#   Pone los dos mediadores en paralelo y permite que estén correlacionados.
#   Compara la contribución relativa de la ruta interoceptiva (MAIA) vs la
#   ruta social (SASS) controlando una por la otra.
#
# Efectos definidos:
#   ind_MAIA       = a1*b1
#   ind_SASS       = a2*b2
#   total_indirect = a1*b1 + a2*b2
#   total          = cp + total_indirect
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO M3 — Mediación paralela {MAIA, SASS}\n")
cat(strrep("=", 60), "\n", sep = "")

mod3 <- '
  MAIA_DIRt   ~ a1*grupo
  SASS_DIRt   ~ a2*grupo

  diff_effort ~ b1*MAIA_DIRt + b2*SASS_DIRt + cp*grupo

  MAIA_DIRt ~~ SASS_DIRt

  ind_MAIA       := a1*b1
  ind_SASS       := a2*b2
  total_indirect := (a1*b1) + (a2*b2)
  total          := cp + total_indirect
'

set.seed(SEED)
fit3 <- sem(mod3, data = df_full, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen ---\n")
print(summary(fit3, ci = TRUE, standardized = TRUE))

cat("\n--- Efectos indirectos específicos (IC95% BCa) ---\n")
print(parameterEstimates(fit3, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a1", "a2", "b1", "b2", "cp",
                            "ind_MAIA", "ind_SASS", "total_indirect", "total")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




# ============================================================================
# MODELO M4 — MEDIACIÓN MODERADA (Hayes 14): grupo modera el camino b
# ============================================================================
# Qué hace:
#   Extensión de M2 con interacción SASS × grupo en el camino b. Permite
#   estimar efectos indirectos CONDICIONALES por grupo:
#     ind_control    = a*b              (grupo = 0)
#     ind_vulnerable = a*(b + bW)       (grupo = 1)
#     index_MM       = a*bW             (índice de mediación moderada)
#
# Interpretación:
#   Si el IC95% del index_MM no incluye 0, la fuerza del efecto indirecto
#   vía SASS difiere entre Control y Vulnerable.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO M4 — Hayes 14 (grupo modera camino b vía SASS)\n")
cat(strrep("=", 60), "\n", sep = "")

# Producto precomputado (lavaan no acepta operadores ":" en fórmulas)
df_full$SASS_x_grupo <- df_full$SASS_DIRt * df_full$grupo

mod4 <- '
  SASS_DIRt   ~ a*grupo
  diff_effort ~ b*SASS_DIRt + cp*grupo + bW*SASS_x_grupo

  ind_control    := a*b
  ind_vulnerable := a*(b + bW)
  index_MM       := a*bW
'

set.seed(SEED)
fit4 <- sem(mod4, data = df_full, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen ---\n")
print(summary(fit4, ci = TRUE, standardized = TRUE))

cat("\n--- Efectos indirectos condicionales e índice MM (IC95% BCa) ---\n")
print(parameterEstimates(fit4, boot.ci.type = "bca.simple", level = 0.95) %>%
        filter(label %in% c("a", "b", "cp", "bW",
                            "ind_control", "ind_vulnerable", "index_MM")) %>%
        select(label, est, se, ci.lower, ci.upper, pvalue))




# ============================================================================
# MODELO M5 — MEDIACIÓN SIMPLE  MAIA → SASS → diff_effort  (solo Vulnerables)
# ============================================================================
# Qué hace:
#   Modelo estructuralmente DISTINTO al resto:
#     X = MAIA_DIRt     (predictor)
#     M = SASS_DIRt     (mediador)
#     Y = diff_effort   (resultado)
#   Restringido al subgrupo Vulnerable (grupo != 0).
#
# Hipótesis subyacente:
#   La interocepción (MAIA) influye sobre la adaptación social (SASS), y ésta
#   sobre el esfuerzo diferencial, específicamente en población vulnerable.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("MODELO M5 — MAIA → SASS → diff_effort  (solo Vulnerables)\n")
cat(strrep("=", 60), "\n", sep = "")

df_v <- subset(df_full, grupo != 0)
cat("N (grupo Vulnerable) =", nrow(df_v), "\n")

mod5 <- '
  SASS_DIRt   ~ a*MAIA_DIRt
  diff_effort ~ b*SASS_DIRt + cp*MAIA_DIRt

  indirect := a*b
  total    := cp + a*b
'

set.seed(SEED)
fit5 <- sem(mod5, data = df_v, se = "bootstrap", bootstrap = N_BOOT)

cat("\n--- Resumen ---\n")
print(summary(fit5, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit5, boot.ci.type = "bca.simple", level = 0.95)[
  , c("label", "est", "se", "ci.lower", "ci.upper", "pvalue")])




# ============================================================================
# TABLA CONSOLIDADA — Efectos indirectos de los modelos de mediación
# ============================================================================
# Reúne, en una sola tabla, los efectos indirectos relevantes de M1..M5,
# marcando con "✓" aquellos cuyo IC95% BCa no cruza 0.
# ============================================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("TABLA CONSOLIDADA — Efectos indirectos (M1-M5)\n")
cat(strrep("=", 60), "\n\n", sep = "")

extract_indirect <- function(fit, labels, model_name) {
  pe <- parameterEstimates(fit, boot.ci.type = "bca.simple", level = 0.95)
  pe[pe$label %in% labels,
     c("label", "est", "se", "ci.lower", "ci.upper", "pvalue")] %>%
    mutate(modelo = model_name) %>%
    select(modelo, everything())
}

tabla <- bind_rows(
  extract_indirect(fit1, c("indirect"),                     "M1. Simple vía MAIA"),
  extract_indirect(fit2, c("indirect"),                     "M2. Simple vía SASS"),
  extract_indirect(fit3, c("ind_MAIA", "ind_SASS",
                            "total_indirect"),               "M3. Paralela"),
  extract_indirect(fit4, c("ind_control", "ind_vulnerable",
                            "index_MM"),                     "M4. Hayes 14"),
  extract_indirect(fit5, c("indirect"),                     "M5. MAIA→SASS→diff (Vuln.)")
)

tabla$significativo <- ifelse(sign(tabla$ci.lower) == sign(tabla$ci.upper),
                              "✓ (CI no cruza 0)", "ns")

print(tabla, row.names = FALSE)
