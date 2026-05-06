

## Correlaciones entre parámetros del modelo parabólico 2K1B y escalas psicológicas/clínicas
# Diego Garrido Cerpa - Viña del Mar 2026


# Librerías

library(tidyverse)
library(Hmisc)


########################################################
## Correlaciones entre slopes de EFFORT escalas psicológicas/clínicas


# Cargar datos
df_effort <- read.csv("Datos/dataset_full_v2.csv", stringsAsFactors = FALSE)

# Filtrar por grupo
df_effort = subset(df_effort, grupo!="1")

# Variables dependientes (slopes de effort)
vd_names <- c("effort_self", "effort_other", "diff_effort")
vd_labels <- c(effort_self = "Effort Self", effort_other = "Effort Other", diff_effort = "Effort Other - Self")


# Escalas (totales y subdimensiones)
# Excluidas: 
# AIM_DIRt (categorica), 
# ASSIST_Total_DIRd (duplicado de ASSIST_DIRt)
# PSQQ y subdimensiones
# AIM y subdimensiones
escalas <- c(
  # Totales
  "CSI_DIRt", "GHQ12_DIRt", "IRI_DIRt", "LSNS_DIRt", "MAIA_DIRt",
  "PSQQ_DIRt", "PSS_DIRt", "RMET_DIRt", "SASS_DIRt", "SWBS_DIRt",
  "UCLA_DIRt", "WAST_DIRt", "ASSIST_DIRt",
  # DASS21
  "DASS21_depresion_DIRd", "DASS21_ansiedad_DIRd", "DASS21_estres_DIRd",
  # IFS (total + 8 subdimensiones)
  "IFS_Total_DIRd",
  "IFS_SeriesMotoras_DIRd", "IFS_InstruccionesConflictivas_DIRd",
  "IFS_ControlInhibitorioMotor_DIRd", "IFS_RepeticionDigitosAtras_DIRd",
  "IFS_MesesAtras_DIRd", "IFS_MemoriaTrabajoVisual_DIRd",
  "IFS_Refranes_DIRd", "IFS_ControlInhibitorioVerbal_DIRd",
  # IRI (4 subdimensiones)
  "IRI_TomaPerspectiva_DIRd", "IRI_Fantasia_DIRd",
  "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd",
  # LSNS
  "LSNS_Familiares_DIRd", "LSNS_Amistados_DIRd",
  # MAIA (8 subdimensiones)
  "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
  "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
  "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd",
  "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd",
  # SWBS (5 subdimensiones)
  "SWBS_IntegracionSocial_DIRd", "SWBS_AceptacionSocial_DIRd",
  "SWBS_ContribucionSocial_DIRd", "SWBS_ActualizacionSocial_DIRd",
  "SWBS_CoherenciaSocial_DIRd",
  # ASSIST (subdimensiones; el total ya esta como ASSIST_DIRt)
  "ASSIST_ConsumoDrogasLegales_DIRd", "ASSIST_ConsumoDrogasIlegales_DIRd"
)

# Preparar datos
df_analisis <- df_effort %>%
  select(all_of(c(vd_names, escalas))) %>%
  mutate(across(everything(), as.numeric))

cat("N por columna:", nrow(df_analisis) - colSums(is.na(df_analisis)), "\n")


# Correlaciones de Spearman
corr <- rcorr(as.matrix(df_analisis), type = "spearman")

corr_df <- expand.grid(VD = vd_names, Escala = escalas, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    rho = corr$r[VD, Escala],
    p_value = corr$P[VD, Escala],
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE ~ ""
    )
  ) %>%
  ungroup()


# Imprimir significativas p < 0.10
cat("\n--- Correlaciones significativas (p < 0.05) ---\n")
corr_df %>% filter(p_value < 0.05) %>% 
  arrange(VD, p_value) %>% 
  print(n = 50)

cat("\n--- Tendencias (p < 0.10) ---\n")
corr_df %>% filter(p_value >= 0.05, p_value < 0.10) %>% 
  arrange(VD, p_value) %>% 
  print(n = 50)


# Gráfico: Barplot de rho por VD (solo significativas y tendencias)
plot_df <- corr_df %>%
  filter(!is.na(p_value), p_value < 0.10) %>%
  mutate(
    Escala_label = gsub("_DIRt|_DIRd", "", Escala),
    VD_label = vd_labels[VD],
    sig_color = case_when(
      p_value < 0.05 ~ "p < .05",
      p_value < 0.10 ~ "p < .10"
    )
  )

if (nrow(plot_df) == 0) {
  cat("\n-> No hay correlaciones con p < 0.10; no se genera grafico.\n")
} else {
  p <- ggplot(plot_df, aes(x = reorder(Escala_label, rho),
                           y = rho, fill = sig_color)) +
    geom_col(width = 0.7, color = "gray30", linewidth = 0.3) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    facet_wrap(~VD_label, ncol = 3) +
    scale_fill_manual(values = c("p < .05" = "#D6604D",
                                 "p < .10" = "#FDB863")) +
    labs(x = "", y = "Spearman rho", fill = "",
         title = "Correlaciones bivariadas: Slopes de Effort vs Escalas",
         subtitle = "Solo correlaciones con p < 0.10") +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    )
  
  ggsave("correlaciones_bivariadas_barplot.png", p, width = 14, height = 7, dpi = 150)
  cat("\n-> Guardado: correlaciones_bivariadas_barplot.png\n")
}











####################################################
## Test de normalidad Shapiro-Wilk

# Cargar datos
df <- read.csv("Datos/dataset_full_v2.csv", stringsAsFactors = FALSE)



# Variables a evaluar, agrupadas por encuesta / dominio
grupos_vars <- list(
  Effort = c("effort_other", "effort_self", "diff_effort"),
  Reward = c("reward_other", "reward_self", "diff_reward")
)

vars_test <- unlist(grupos_vars, use.names = FALSE)
encuesta_lookup <- tibble(
  variable = vars_test,
  encuesta = rep(names(grupos_vars), lengths(grupos_vars))
)

# Aplicar Shapiro-Wilk a cada variable
shapiro_result <- map_dfr(vars_test, function(v) {
  x <- as.numeric(df[[v]])
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3) {
    tibble(variable = v, n = length(x),
           W = NA_real_, p_value = NA_real_, normal_p05 = NA)
  } else {
    sw <- shapiro.test(x)
    tibble(variable   = v,
           n          = length(x),
           W          = unname(sw$statistic),
           p_value    = sw$p.value,
           normal_p05 = sw$p.value > 0.05)
  }
}) %>%
  left_join(encuesta_lookup, by = "variable") %>%
  select(encuesta, variable, n, W, p_value, normal_p05) %>%
  arrange(encuesta, p_value)

# Resumen en consola
print(shapiro_result, n = Inf)

cat("\nVariables normales (p > 0.05):",
    sum(shapiro_result$normal_p05, na.rm = TRUE), "/",
    sum(!is.na(shapiro_result$normal_p05)), "\n")

# Guardar tabla
# write.csv(shapiro_result,
#           "Datos/shapiro_normalidad.csv",
#           row.names = FALSE)




####################################################
# Visualización de las distribuciones (un gráfico por encuesta)
plot_hist <- function(d, titulo, ncol_facet) {
  params <- d %>%
    group_by(variable) %>%
    summarise(mu = mean(valor), sd = sd(valor), .groups = "drop") %>%
    filter(is.finite(mu), is.finite(sd), sd > 0)
  
  curva_normal <- params %>%
    purrr::pmap_dfr(function(variable, mu, sd) {
      x <- seq(mu - 4*sd, mu + 4*sd, length.out = 200)
      tibble(variable = variable, x = x, y = dnorm(x, mu, sd))
    })
  
  ggplot(d, aes(x = valor)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 20, fill = "steelblue",
                   color = "white", alpha = 0.75) +
    geom_line(data = curva_normal, aes(x = x, y = y),
              color = "firebrick", linewidth = 0.6) +
    facet_wrap(~ variable, scales = "free", ncol = ncol_facet) +
    labs(title = titulo, x = NULL, y = "Densidad") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(size = 9))
}

plot_qq <- function(d, titulo, ncol_facet) {
  ggplot(d, aes(sample = valor)) +
    stat_qq(alpha = 0.55, size = 0.8) +
    stat_qq_line(color = "firebrick", linewidth = 0.5) +
    facet_wrap(~ variable, scales = "free", ncol = ncol_facet) +
    labs(title = titulo,
         x = "Cuantiles teóricos (Normal)",
         y = "Cuantiles muestrales") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(size = 9))
}

# Generar histogramas y Q-Q plots, uno por encuesta
for (enc in names(grupos_vars)) {
  vars_enc <- grupos_vars[[enc]]
  d_enc <- df %>%
    select(all_of(vars_enc)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    drop_na(valor) %>%
    mutate(variable = factor(variable, levels = vars_enc))
  
  n_var      <- length(vars_enc)
  ncol_facet <- min(3, n_var)
  ancho      <- max(6, ncol_facet * 3.2)
  alto       <- max(4, ceiling(n_var / ncol_facet) * 2.8)
  
  ggsave(paste0("histogramas_", enc, ".png"),
         plot_hist(d_enc,
                   paste0("Distribuciones - ", enc, " (curva = Normal teórica)"),
                   ncol_facet),
         width = ancho, height = alto, dpi = 150, limitsize = FALSE)
}




####################################################
# Test de normalidad Kolmogorov-Smirnov

# Aplicar K-S a cada variable
ks_result <- map_dfr(vars_test, function(v) {
  x <- as.numeric(df[[v]])
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3 || sd(x) == 0) {
    tibble(variable = v, n = length(x),
           D = NA_real_, p_value = NA_real_, normal_p05 = NA)
  } else {
    ks <- suppressWarnings(
      ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
    )
    tibble(variable   = v,
           n          = length(x),
           D          = unname(ks$statistic),
           p_value    = ks$p.value,
           normal_p05 = ks$p.value > 0.05)
  }
}) %>%
  left_join(encuesta_lookup, by = "variable") %>%
  select(encuesta, variable, n, D, p_value, normal_p05) %>%
  arrange(encuesta, p_value)

# Resumen en consola
print(ks_result, n = Inf)

cat("\nVariables normales segun K-S (p > 0.05):",
    sum(ks_result$normal_p05, na.rm = TRUE), "/",
    sum(!is.na(ks_result$normal_p05)), "\n")

# Guardar tabla
# write.csv(ks_result,
#           "Datos/ks_normalidad.csv",
#           row.names = FALSE)





