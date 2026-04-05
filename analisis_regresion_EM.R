################################################################################
## analisis_regresion_EM.R
## Aproximación 4 → 2: Correlaciones bivariadas + Regresión múltiple
## VD: parámetros modelo hiperbólico 2K1B (h_self_k, h_other_k, h_other_self_k)
## Predictores: escalas psicológicas/clínicas
################################################################################

# === Librerías ===
library(tidyverse)
library(corrplot)
library(Hmisc)
library(broom)
library(car)
library(ggpubr)     # ggarrange, stat_cor
library(patchwork)   # combinar ggplots
library(RColorBrewer)

# === 1) Cargar datos ===
df <- read.csv("dataset_completo.csv", stringsAsFactors = FALSE)

# === 2) Definir variables ===
vd_names <- c("h_self_k", "h_other_k", "h_other_self_k")
vd_labels <- c("k Self", "k Other", "k Other - Self")
names(vd_labels) <- vd_names

# Escalas totales (DIRt)
escalas_totales <- c(
  "CSI_DIRt", "GHQ12_DIRt", "IRI_DIRt", "LSNS_DIRt", "MAIA_DIRt",
  "PSQQ_DIRt", "PSS_DIRt", "RMET_DIRt", "SASS_DIRt", "SWBS_DIRt",
  "UCLA_DIRt", "WAST_DIRt", "ASSIST_DIRt"
)

# Etiquetas legibles para escalas totales
escala_labels <- c(
  CSI_DIRt = "Estrés Crónico", GHQ12_DIRt = "Salud General",
  IRI_DIRt = "Empatía", LSNS_DIRt = "Redes Sociales",
  MAIA_DIRt = "Interocepción", PSQQ_DIRt = "Calidad Sueño",
  PSS_DIRt = "Estrés Percibido", RMET_DIRt = "Teoría de la Mente",
  SASS_DIRt = "Adaptación Social", SWBS_DIRt = "Bienestar Social",
  UCLA_DIRt = "Soledad", WAST_DIRt = "Abuso", ASSIST_DIRt = "Consumo Sustancias"
)

# Dimensiones relevantes (DIRd)
escalas_dimensiones <- c(
  "DASS21_depresion_DIRd", "DASS21_ansiedad_DIRd", "DASS21_estres_DIRd",
  "IFS_Total_DIRd",
  "IRI_TomaPerspectiva_DIRd", "IRI_Fantasia_DIRd",
  "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd",
  "LSNS_Familiares_DIRd", "LSNS_Amistados_DIRd",
  "SWBS_IntegracionSocial_DIRd", "SWBS_AceptacionSocial_DIRd",
  "SWBS_ContribucionSocial_DIRd", "SWBS_ActualizacionSocial_DIRd",
  "SWBS_CoherenciaSocial_DIRd"
)

todas_escalas <- c(escalas_totales, escalas_dimensiones)

# === 3) Preparar datos ===
df_analisis <- df %>%
  select(sub, grupo, all_of(vd_names), all_of(todas_escalas)) %>%
  mutate(across(all_of(c(vd_names, todas_escalas)), as.numeric),
         grupo_label = factor(grupo, levels = c(0, 1),
                              labels = c("Control", "Experimental"))) %>%
  drop_na(all_of(vd_names))

cat("N final para análisis:", nrow(df_analisis), "\n")

################################################################################
## PARTE A: CORRELACIONES BIVARIADAS
################################################################################

cat("\n========================================\n")
cat("PARTE A: CORRELACIONES BIVARIADAS\n")
cat("========================================\n")

# --- A.1) Calcular correlaciones de Spearman ---
mat_corr <- df_analisis %>% select(all_of(vd_names), all_of(todas_escalas))
mat_complete <- mat_corr[complete.cases(mat_corr), ]
corr_result <- rcorr(as.matrix(mat_complete), type = "spearman")

r_vals <- corr_result$r[vd_names, todas_escalas]
p_vals <- corr_result$P[vd_names, todas_escalas]

# Tabla resumen
corr_summary <- data.frame()
for (vd in vd_names) {
  for (esc in todas_escalas) {
    corr_summary <- rbind(corr_summary, data.frame(
      VD = vd, Escala = esc,
      rho = round(r_vals[vd, esc], 3),
      p_value = round(p_vals[vd, esc], 4),
      sig = ifelse(p_vals[vd, esc] < 0.001, "***",
                   ifelse(p_vals[vd, esc] < 0.01, "**",
                          ifelse(p_vals[vd, esc] < 0.05, "*",
                                 ifelse(p_vals[vd, esc] < 0.10, ".", "")))),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n--- Correlaciones significativas (p < 0.05) ---\n")
sig <- corr_summary %>% filter(p_value < 0.05) %>% arrange(VD, p_value)
if (nrow(sig) > 0) print(sig, row.names = FALSE) else cat("Ninguna\n")

cat("\n--- Tendencias (p < 0.10) ---\n")
tend <- corr_summary %>% filter(p_value < 0.10 & p_value >= 0.05) %>% arrange(VD, p_value)
if (nrow(tend) > 0) print(tend, row.names = FALSE) else cat("Ninguna\n")

write.csv(corr_summary, "correlaciones_bivariadas_completas.csv", row.names = FALSE)

# --- GRÁFICO A.1: Heatmap de correlaciones (escalas totales) ---
r_totales <- r_vals[, escalas_totales]
p_totales <- p_vals[, escalas_totales]
colnames(r_totales) <- escala_labels[colnames(r_totales)]
colnames(p_totales) <- escala_labels[colnames(p_totales)]
rownames(r_totales) <- vd_labels[rownames(r_totales)]
rownames(p_totales) <- vd_labels[rownames(p_totales)]

png("Fig1_heatmap_correlaciones_totales.png", width = 1100, height = 400, res = 130)
p1 = corrplot(r_totales,
         method = "color", type = "full",
         p.mat = p_totales, sig.level = 0.05,
         insig = "label_sig", pch.cex = 2,
         tl.cex = 0.85, tl.col = "black", tl.srt = 40,
         cl.cex = 0.8, cl.ratio = 0.15,
         addCoef.col = "black", number.cex = 0.75,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlaciones Spearman: Parámetros hiperbólicos vs Escalas (totales)",
         mar = c(0, 0, 2, 0))

print(p1)




# --- GRÁFICO A.2: Heatmap de correlaciones (dimensiones) ---
r_dim <- r_vals[, escalas_dimensiones]
p_dim <- p_vals[, escalas_dimensiones]
rownames(r_dim) <- vd_labels[rownames(r_dim)]
rownames(p_dim) <- vd_labels[rownames(p_dim)]
# Acortar nombres de dimensiones para legibilidad
colnames(r_dim) <- gsub("_DIRd", "", colnames(r_dim))
colnames(p_dim) <- gsub("_DIRd", "", colnames(p_dim))

png("Fig2_heatmap_correlaciones_dimensiones.png", width = 1200, height = 400, res = 130)
p2 = corrplot(r_dim,
         method = "color", type = "full",
         p.mat = p_dim, sig.level = 0.05,
         insig = "label_sig", pch.cex = 2,
         tl.cex = 0.7, tl.col = "black", tl.srt = 45,
         cl.cex = 0.8, cl.ratio = 0.15,
         addCoef.col = "black", number.cex = 0.6,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Correlaciones Spearman: Parámetros hiperbólicos vs Subdimensiones",
         mar = c(0, 0, 2, 0))
print(p2)




# --- GRÁFICO A.3: Scatterplots de las correlaciones significativas ---
sig_all <- corr_summary %>% filter(p_value < 0.10)  # incluir tendencias

if (nrow(sig_all) > 0) {
  plot_list <- list()
  for (i in 1:nrow(sig_all)) {
    vd_var <- sig_all$VD[i]
    esc_var <- sig_all$Escala[i]
    rho_val <- sig_all$rho[i]
    p_val <- sig_all$p_value[i]
    
    p <- ggplot(df_analisis, aes(x = .data[[esc_var]], y = .data[[vd_var]])) +
      geom_point(aes(color = grupo_label), size = 2.5, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8, alpha = 0.2) +
      scale_color_manual(values = c("Control" = "#4393C3", "Experimental" = "#D6604D")) +
      labs(
        x = gsub("_DIRd|_DIRt", "", esc_var),
        y = vd_labels[vd_var],
        title = paste0("rho = ", rho_val, ", p = ", sprintf("%.4f", p_val)),
        color = "Grupo"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(size = 9, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        panel.grid.minor = element_blank()
      )
    plot_list[[paste(vd_var, esc_var)]] <- p
  }
  
  # Combinar scatterplots
  n_plots <- length(plot_list)
  ncols <- min(3, n_plots)
  nrows <- ceiling(n_plots / ncols)
  
  combined <- wrap_plots(plot_list, ncol = ncols, nrow = nrows) +
    plot_annotation(
      title = "Correlaciones significativas y tendencias (p < 0.10)",
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  ggsave("Fig3_scatterplots_significativos.png", combined,
         width = 5 * ncols, height = 4.5 * nrows, dpi = 150, limitsize = FALSE)
  cat("-> Guardado: Fig3_scatterplots_significativos.png\n")
} else {
  cat("No hay correlaciones significativas o tendencias para graficar\n")
}

# --- GRÁFICO A.4: Barplot resumen de rho por VD (todas las escalas totales) ---
corr_totales_df <- corr_summary %>%
  filter(Escala %in% escalas_totales) %>%
  mutate(
    Escala_label = escala_labels[Escala],
    VD_label = vd_labels[VD],
    sig_color = ifelse(p_value < 0.05, "Significativa", 
                       ifelse(p_value < 0.10, "Tendencia", "No sig."))
  )

p_bars <- ggplot(corr_totales_df, aes(x = reorder(Escala_label, rho), y = rho, fill = sig_color)) +
  geom_col(width = 0.7, color = "gray30", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  facet_wrap(~VD_label, ncol = 3) +
  scale_fill_manual(values = c("Significativa" = "#D6604D",
                               "Tendencia" = "#FDB863",
                               "No sig." = "#D9D9D9")) +
  labs(x = "", y = "Spearman rho", fill = "",
       title = "Correlaciones bivariadas: Parámetros hiperbólicos vs Escalas totales") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

print(p_bars)

ggsave("Fig4_barplot_correlaciones_resumen.png", p_bars,
       width = 14, height = 5, dpi = 150)
cat("-> Guardado: Fig4_barplot_correlaciones_resumen.png\n")







################################################################################
## PARTE B: REGRESIONES MÚLTIPLES
################################################################################

cat("\n========================================\n")
cat("PARTE B: REGRESIONES MÚLTIPLES\n")
cat("========================================\n")

# --- B.1) Selección de predictores ---
pred_teoricos <- c(
  "IRI_DIRt", "SWBS_DIRt", "PSS_DIRt", "UCLA_DIRt",
  "LSNS_DIRt", "DASS21_depresion_DIRd", "IFS_Total_DIRd", "RMET_DIRt"
)

pred_empiricos <- corr_summary %>%
  filter(p_value < 0.05) %>%
  pull(Escala) %>%
  unique()

predictores_final <- unique(c(pred_teoricos, pred_empiricos))
cat("\nPredictores seleccionados:\n")
cat(paste(" -", predictores_final), sep = "\n")

# --- B.2) Correr regresiones ---
resultados_reg <- list()

for (vd in vd_names) {
  
  cat("\n--------------------------------------------------\n")
  cat("Regresión para:", vd_labels[vd], "\n")
  cat("--------------------------------------------------\n")
  
  formula_obj <- as.formula(paste(vd, "~ grupo +", paste(predictores_final, collapse = " + ")))
  
  df_reg <- df_analisis %>%
    select(all_of(c(vd, "grupo", "grupo_label", predictores_final))) %>%
    drop_na()
  
  n_pred <- length(predictores_final) + 1
  cat("N =", nrow(df_reg), "| Predictores =", n_pred,
      "| Ratio N/pred =", round(nrow(df_reg) / n_pred, 1), "\n")
  
  modelo <- lm(formula_obj, data = df_reg)
  summ <- summary(modelo)
  
  cat("R² =", round(summ$r.squared, 4),
      "| R² adj =", round(summ$adj.r.squared, 4),
      "| F =", round(summ$fstatistic[1], 2),
      "| p =", round(pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3],
                        lower.tail = FALSE), 4), "\n")
  
  coefs <- tidy(modelo) %>%
    mutate(sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ ""
    ))
  print(coefs %>% select(term, estimate, std.error, statistic, p.value, sig), n = 50)
  
  # VIF
  vif_vals <- tryCatch(vif(modelo), error = function(e) NULL)
  if (!is.null(vif_vals)) {
    cat("\nVIF (>5 = multicolinealidad):\n")
    print(data.frame(variable = names(vif_vals), VIF = round(vif_vals, 2)) %>% arrange(desc(VIF)))
  }
  
  resultados_reg[[vd]] <- list(modelo = modelo, summary = summ, coefs = coefs,
                               df_reg = df_reg, vif = vif_vals)
}

# --- B.3) Exportar coeficientes ---
all_coefs <- bind_rows(
  lapply(names(resultados_reg), function(vd) {
    resultados_reg[[vd]]$coefs %>% mutate(VD = vd)
  })
) %>%
  select(VD, term, estimate, std.error, statistic, p.value, sig) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

write.csv(all_coefs, "regresiones_multiples_resultados.csv", row.names = FALSE)

# --- GRÁFICO B.1: Forest plot de coeficientes por VD ---
coef_plot_data <- all_coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(
    VD_label = vd_labels[VD],
    term_clean = gsub("_DIRt|_DIRd", "", term),
    significant = ifelse(p.value < 0.05, "p < .05",
                         ifelse(p.value < 0.10, "p < .10", "n.s.")),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

p_forest <- ggplot(coef_plot_data, aes(x = estimate, y = reorder(term_clean, estimate),
                                       color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25, linewidth = 0.6) +
  geom_point(size = 2.5) +
  facet_wrap(~VD_label, scales = "free_x", ncol = 3) +
  scale_color_manual(values = c("p < .05" = "#D6604D", "p < .10" = "#FDB863", "n.s." = "#999999")) +
  labs(x = "Coeficiente (IC 95%)", y = "", color = "",
       title = "Regresión múltiple: Coeficientes por VD",
       subtitle = "Controlando por grupo") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )
print(p_forest)
ggsave("Fig5_forest_plot_regresiones.png", p_forest,
       width = 15, height = 6, dpi = 150)
cat("\n-> Guardado: Fig5_forest_plot_regresiones.png\n")


# --- GRÁFICO B.2: Diagnóstico de residuos por modelo ---
png("Fig6_diagnostico_residuos.png", width = 1400, height = 1200, res = 130)
par(mfrow = c(3, 4), mar = c(4, 4, 3, 1))
for (vd in vd_names) {
  mod <- resultados_reg[[vd]]$modelo
  
  # Residuos vs Ajustados
  plot(fitted(mod), residuals(mod), pch = 16, col = "#4393C3AA",
       xlab = "Ajustados", ylab = "Residuos",
       main = paste(vd_labels[vd], "- Residuos vs Ajustados"))
  abline(h = 0, lty = 2, col = "red")
  
  # Q-Q plot
  qqnorm(residuals(mod), pch = 16, col = "#4393C3AA",
         main = paste(vd_labels[vd], "- Q-Q"))
  qqline(residuals(mod), col = "red", lty = 2)
  
  # Histograma de residuos
  hist(residuals(mod), breaks = 15, col = "#4393C3", border = "white",
       main = paste(vd_labels[vd], "- Distribución"),
       xlab = "Residuos")
  
  # Cook's distance
  cooks <- cooks.distance(mod)
  plot(cooks, type = "h", col = "#4393C3", lwd = 2,
       main = paste(vd_labels[vd], "- Cook's D"),
       ylab = "Cook's Distance")
  abline(h = 4 / length(cooks), lty = 2, col = "red")
}
dev.off()
cat("-> Guardado: Fig6_diagnostico_residuos.png\n")





# --- GRÁFICO B.3: VIF barplot ---
if (any(sapply(resultados_reg, function(x) !is.null(x$vif)))) {
  vif_all <- bind_rows(
    lapply(names(resultados_reg), function(vd) {
      v <- resultados_reg[[vd]]$vif
      if (!is.null(v)) {
        data.frame(VD = vd_labels[vd],
                   variable = gsub("_DIRt|_DIRd", "", names(v)),
                   VIF = v)
      }
    })
  )
  
  p_vif <- ggplot(vif_all, aes(x = reorder(variable, VIF), y = VIF, fill = VD)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 0.8) +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "VIF", fill = "Variable Dependiente",
         title = "Factor de Inflación de Varianza (VIF)",
         subtitle = "Línea roja = umbral de multicolinealidad (VIF > 5)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  
  ggsave("Fig7_VIF_multicolinealidad.png", p_vif,
         width = 10, height = 6, dpi = 150)
  cat("-> Guardado: Fig7_VIF_multicolinealidad.png\n")
}

# --- GRÁFICO B.4: R² comparativo entre modelos ---
r2_df <- data.frame(
  VD = vd_labels[vd_names],
  R2 = sapply(resultados_reg, function(x) x$summary$r.squared),
  R2_adj = sapply(resultados_reg, function(x) x$summary$adj.r.squared)
) %>%
  pivot_longer(cols = c(R2, R2_adj), names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = ifelse(Tipo == "R2", "R²", "R² ajustado"))

p_r2 <- ggplot(r2_df, aes(x = VD, y = Valor, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.6, color = "gray30", linewidth = 0.3) +
  geom_text(aes(label = round(Valor, 3)), position = position_dodge(0.6),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("R²" = "#4393C3", "R² ajustado" = "#92C5DE")) +
  labs(x = "", y = "", fill = "",
       title = "Bondad de ajuste de las regresiones múltiples") +
  ylim(0, max(r2_df$Valor) * 1.3) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("Fig8_R2_comparativo.png", p_r2, width = 7, height = 4.5, dpi = 150)
cat("-> Guardado: Fig8_R2_comparativo.png\n")

################################################################################
cat("\n========================================\n")
cat("RESUMEN DE ARCHIVOS GENERADOS\n")
cat("========================================\n")
cat("Datos:\n")
cat("  - correlaciones_bivariadas_completas.csv\n")
cat("  - regresiones_multiples_resultados.csv\n")
cat("Figuras:\n")
cat("  - Fig1_heatmap_correlaciones_totales.png\n")
cat("  - Fig2_heatmap_correlaciones_dimensiones.png\n")
cat("  - Fig3_scatterplots_significativos.png\n")
cat("  - Fig4_barplot_correlaciones_resumen.png\n")
cat("  - Fig5_forest_plot_regresiones.png\n")
cat("  - Fig6_diagnostico_residuos.png\n")
cat("  - Fig7_VIF_multicolinealidad.png\n")
cat("  - Fig8_R2_comparativo.png\n")
cat("========================================\n")
