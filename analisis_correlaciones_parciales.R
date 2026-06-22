# ============================================================
# GGM - Correlaciones parciales: diff_effort, MAIA_DIRt, SASS_DIRt
# ============================================================

library(ppcor)  # correlaciones parciales

df <- read.csv("dataset_full_final.csv", header = TRUE)
sub <- df[, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")]
sub <- na.omit(sub)

cat("N =", nrow(sub), "\n\n")

# Correlaciones de Pearson simples
cat("=== CORRELACIONES DE PEARSON ===\n")
print(round(cor(sub), 4))

# Correlaciones parciales (aristas GGM)
cat("\n=== CORRELACIONES PARCIALES (aristas GGM) ===\n\n")

r1 <- pcor.test(sub$diff_effort, sub$MAIA_DIRt, sub$SASS_DIRt)
cat("diff_effort — MAIA_DIRt (ctrl SASS):\n")
cat(sprintf("  r = %.4f  |  IC95%% aprox [%.3f, %.3f]  |  p = %.4f\n\n",
            r1$estimate,
            r1$estimate - 1.96 * sqrt((1 - r1$estimate^2) / (nrow(sub) - 3)),
            r1$estimate + 1.96 * sqrt((1 - r1$estimate^2) / (nrow(sub) - 3)),
            r1$p.value))

r2 <- pcor.test(sub$diff_effort, sub$SASS_DIRt, sub$MAIA_DIRt)
cat("diff_effort — SASS_DIRt (ctrl MAIA):\n")
cat(sprintf("  r = %.4f  |  IC95%% aprox [%.3f, %.3f]  |  p = %.4f\n\n",
            r2$estimate,
            r2$estimate - 1.96 * sqrt((1 - r2$estimate^2) / (nrow(sub) - 3)),
            r2$estimate + 1.96 * sqrt((1 - r2$estimate^2) / (nrow(sub) - 3)),
            r2$p.value))

r3 <- pcor.test(sub$MAIA_DIRt, sub$SASS_DIRt, sub$diff_effort)
cat("MAIA_DIRt — SASS_DIRt (ctrl diff_effort):\n")
cat(sprintf("  r = %.4f  |  IC95%% aprox [%.3f, %.3f]  |  p = %.4f\n\n",
            r3$estimate,
            r3$estimate - 1.96 * sqrt((1 - r3$estimate^2) / (nrow(sub) - 3)),
            r3$estimate + 1.96 * sqrt((1 - r3$estimate^2) / (nrow(sub) - 3)),
            r3$p.value))




# ============================================================
# Gráfico de red GGM
# ============================================================

library(ggplot2)
library(ggforce)  # geom_circle

# --- Datos de aristas ---
edges <- data.frame(
  x1    = c(200, 340, 340),
  y1    = c(200, 360, 360),
  x2    = c(480, 200, 480),
  y2    = c(200, 200, 200),
  r     = c(0.264,  0.018,  0.061),
  p_val = c(0.0158, 0.8710, 0.5811),
  label = c("r = 0.264  p = .016 ✓", "r = 0.018  ns", "r = 0.061  ns"),
  sig   = c(TRUE, FALSE, FALSE)
)

# --- Datos de nodos ---
nodes <- data.frame(
  x     = c(200,        480,        340),
  y     = c(200,        200,        360),
  label = c("MAIA_DIRt\nInterocepción",
            "SASS_DIRt\nAdapt. social",
            "diff_effort\nVD"),
  color = c("#7B52AB", "#1D9E75", "#888888")
)

# --- Etiquetas de aristas (punto medio) ---
edge_labels <- data.frame(
  x     = c((200 + 480) / 2, (340 + 200) / 2, (340 + 480) / 2),
  y     = c((200 + 200) / 2, (360 + 200) / 2, (360 + 200) / 2),
  label = edges$label,
  sig   = edges$sig
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
             size = 3.2, label.size = 0.3, fill = "white",
             show.legend = FALSE) +
  geom_point(data = nodes,
             aes(x = x, y = y, color = color),
             size = 28, show.legend = FALSE) +
  scale_color_identity() +
  geom_text(data = nodes,
            aes(x = x, y = y, label = label),
            color = "white", size = 3.5, fontface = "bold",
            lineheight = 0.9) +
  annotate("segment", x = 30, xend = 80, y = 420, yend = 420,
           color = "#1D9E75", linewidth = 2) +
  annotate("text", x = 88, y = 420, hjust = 0, size = 3,
           color = "grey30", label = "Arista significativa (p < .05)") +
  annotate("segment", x = 30, xend = 80, y = 400, yend = 400,
           color = "grey60", linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = 88, y = 400, hjust = 0, size = 3,
           color = "grey30", label = "Arista no significativa") +
  annotate("text", x = 30, y = 378, hjust = 0, size = 2.8,
           color = "grey50", label = "Correlaciones parciales (GGM, N = 84)") +
  xlim(20, 560) + ylim(100, 440) +
  theme_void()




# ============================================================
# ============================================================
# ANÁLISIS POR GRUPO
# Mismo análisis de correlaciones parciales, separado por
# grupo Control (grupo = 0) y Vulnerable (grupo = 1)
# ============================================================
# ============================================================

library(patchwork)  # para combinar gráficos lado a lado


## Función: corre el análisis de correlaciones parciales sobre un subset
## y devuelve un data.frame con las 3 aristas en el ORDEN que espera el gráfico:
##   1) MAIA-SASS   (arista superior)
##   2) diff-MAIA   (diagonal izquierda)
##   3) diff-SASS   (diagonal derecha)

ggm_subgrupo <- function(data, etiqueta) {
  cat("\n", strrep("=", 50), "\n", sep = "")
  cat("GRUPO:", etiqueta, "  |  N =", nrow(data), "\n")
  cat(strrep("=", 50), "\n\n", sep = "")
  
  cat("=== Correlaciones de Pearson ===\n")
  print(round(cor(data), 4))
  
  cat("\n=== Correlaciones parciales ===\n\n")
  n <- nrow(data)
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


## Función: gráfico GGM para un subset (mismo layout que el gráfico original)

plot_ggm <- function(edges_df, n, titulo) {
  edges <- data.frame(
    x1 = c(200, 340, 340), y1 = c(200, 360, 360),
    x2 = c(480, 200, 480), y2 = c(200, 200, 200),
    r     = edges_df$r,
    p_val = edges_df$p_val,
    sig   = edges_df$p_val < 0.05
  )
  edges$label <- ifelse(edges$sig,
    sprintf("r = %.3f  p = %.3f ✓", edges$r, edges$p_val),
    sprintf("r = %.3f  ns",         edges$r))
  
  nodes <- data.frame(
    x = c(200, 480, 340), y = c(200, 200, 360),
    label = c("MAIA_DIRt\nInterocepción",
              "SASS_DIRt\nAdapt. social",
              "diff_effort\nVD"),
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


## Subset por grupo
df_ctrl <- na.omit(df[df$grupo == 0, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")])
df_vuln <- na.omit(df[df$grupo == 1, c("diff_effort", "MAIA_DIRt", "SASS_DIRt")])


## Correr análisis por grupo
res_ctrl <- ggm_subgrupo(df_ctrl, "CONTROL")
res_vuln <- ggm_subgrupo(df_vuln, "VULNERABLE")


## Gráficos lado a lado
p_ctrl <- plot_ggm(res_ctrl, nrow(df_ctrl), "Control")
p_vuln <- plot_ggm(res_vuln, nrow(df_vuln), "Vulnerable")

print(p_ctrl + p_vuln)



# ============================================================
# Comparación de aristas entre grupos (Fisher's z)
# ¿La misma arista tiene fuerza distinta en Control vs Vulnerable?
# Si Z es grande y p < .05, la correlación parcial difiere entre grupos.
# ============================================================

fisher_z_test <- function(r1, n1, r2, n2) {
  z1 <- 0.5 * log((1 + r1) / (1 - r1))
  z2 <- 0.5 * log((1 + r2) / (1 - r2))
  se <- sqrt(1 / (n1 - 3) + 1 / (n2 - 3))
  Z  <- (z1 - z2) / se
  p  <- 2 * (1 - pnorm(abs(Z)))
  c(Z = Z, p = p)
}

n_c <- nrow(df_ctrl); n_v <- nrow(df_vuln)

cat("\n", strrep("=", 50), "\n", sep = "")
cat("COMPARACIÓN ENTRE GRUPOS (Fisher's z)\n")
cat(strrep("=", 50), "\n\n", sep = "")

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
