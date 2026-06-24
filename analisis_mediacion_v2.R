
## Mediación simple — diff_effort, MAIA, SASS
## Diego Garrido Cerpa - Viña del Mar 2026
##
## Estructura: MAIA → SASS → diff_effort
##   X = MAIA_DIRt       (predictor)
##   M = SASS_DIRt       (mediador)
##   Y = diff_effort     (resultado)


library(lavaan)

# Datos
df <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)
df <- na.omit(df[, c("MAIA_DIRt", "SASS_DIRt", "diff_effort", "grupo")])

df <- subset(df, grupo != "0")


cat("N =", nrow(df), "\n")


## Modelo

mod <- '
  # Camino a (X -> M)
  SASS_DIRt   ~ a*MAIA_DIRt

  # Camino b (M -> Y) y c-prime (X -> Y directo)
  diff_effort ~ b*SASS_DIRt + cp*MAIA_DIRt

  # Efectos definidos
  indirect := a*b
  total    := cp + a*b
'

set.seed(42)
fit <- sem(mod, data = df, se = "bootstrap", bootstrap = 5000)


## Resultados

cat("\n--- Resumen ---\n")
print(summary(fit, ci = TRUE, standardized = TRUE))

cat("\n--- Efecto indirecto (IC95% BCa) ---\n")
print(parameterEstimates(fit, boot.ci.type = "bca.simple", level = 0.95)[
  , c("label", "est", "se", "ci.lower", "ci.upper", "pvalue")])




####################################################
## DIAGRAMA DEL TRIÁNGULO CON COEFICIENTES

library(ggplot2)

# Extraer estimados, p-valores e IC del modelo
params <- parameterEstimates(fit, boot.ci.type = "bca.simple")
get_v  <- function(lbl, col) params[[col]][params$label == lbl]

a_est   <- get_v("a",  "est");  a_p  <- get_v("a",  "pvalue")
b_est   <- get_v("b",  "est");  b_p  <- get_v("b",  "pvalue")
cp_est  <- get_v("cp", "est");  cp_p <- get_v("cp", "pvalue")
ind_est <- get_v("indirect", "est")
ind_lo  <- get_v("indirect", "ci.lower")
ind_hi  <- get_v("indirect", "ci.upper")

sig_star <- function(p) {
  if (is.na(p))         ""
  else if (p < 0.001)   "***"
  else if (p < 0.01)    "**"
  else if (p < 0.05)    "*"
  else if (p < 0.10)    "†"
  else                  " ns"
}

# Coordenadas del triángulo (apex = SASS arriba)
nodes <- data.frame(
  x    = c(100, 340, 580),
  y    = c(100, 300, 100),
  name = c("MAIA",          "SASS",          "diff_effort"),
  role = c("X (predictor)", "M (mediador)",  "Y (resultado)"),
  fill = c("#DBEAFE",       "#FEF3C7",       "#EDE9FE")
)

# Flechas y posiciones de las etiquetas (en los puntos medios)
edges <- data.frame(
  x    = c(160, 390, 160),
  y    = c(120, 270, 100),
  xend = c(290, 520, 520),
  yend = c(270, 120, 100),
  midx = c(225, 455, 340),
  midy = c(195, 195, 100),
  label = c(sprintf("a = %.3f%s",  a_est,  sig_star(a_p)),
            sprintf("b = %.3f%s",  b_est,  sig_star(b_p)),
            sprintf("c' = %.3f%s", cp_est, sig_star(cp_p)))
)

p_path <- ggplot() +
  # Flechas (capa de fondo)
  geom_segment(data = edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               linewidth = 0.7, color = "gray25") +
  # Cajas de los nodos
  geom_rect(data = nodes,
            aes(xmin = x - 70, xmax = x + 70,
                ymin = y - 32, ymax = y + 32, fill = fill),
            color = "gray30", linewidth = 0.6) +
  scale_fill_identity() +
  # Nombre del nodo
  geom_text(data = nodes, aes(x = x, y = y + 8, label = name),
            size = 4.5, fontface = "bold", color = "gray10") +
  # Rol del nodo
  geom_text(data = nodes, aes(x = x, y = y - 12, label = role),
            size = 3, color = "gray40") +
  # Etiquetas de las flechas con coeficientes (fondo blanco para legibilidad)
  geom_label(data = edges,
             aes(x = midx, y = midy, label = label),
             size = 3.8, label.size = 0.3, fill = "white",
             label.padding = unit(0.3, "lines")) +
  # Caja del efecto indirecto al pie
  annotate("label", x = 340, y = -10,
           label = sprintf("Efecto indirecto a × b = %.3f   IC95%% BCa [%.3f, %.3f]",
                           ind_est, ind_lo, ind_hi),
           size = 4, fill = "#E1F5EE", color = "#085041",
           fontface = "bold",
           label.padding = unit(0.4, "lines")) +
  xlim(0, 680) + ylim(-50, 360) +
  labs(title    = "Mediación: MAIA → SASS → diff_effort",
       subtitle = sprintf("Coeficientes no estandarizados (lavaan, bootstrap 5000, N = %d)",
                          nrow(df)),
       caption  = "*** p<.001   ** p<.01   * p<.05   † p<.10   ns p>.10") +
  theme_void() +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40", size = 11),
        plot.caption  = element_text(color = "gray50", size = 10, hjust = 0.5),
        plot.margin   = margin(15, 15, 15, 15))

print(p_path)

# Guardar
dir.create("plots_mediacion", showWarnings = FALSE)
ggsave("plots_mediacion/mediacion_simple_paths.png", p_path,
       width = 10, height = 6, dpi = 150)
cat("\nGuardado: plots_mediacion/mediacion_simple_paths.png\n")
