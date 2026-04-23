

## Correlaciones entre parámetros del modelo parabólico 2K1B y escalas psicológicas/clínicas

# Librerías

library(tidyverse)
library(Hmisc)

# Cargar datos
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Filtrar por grupo
# df = subset(df, grupo!="0")

# Variables dependientes (modelo parabólico) 
vd_names <- c("p_2k1b_k_self", "p_2k1b_k_other", "p_2k1b_diff_k")
vd_labels <- c(p_2k1b_k_self = "k Self", p_2k1b_k_other = "k Other", p_2k1b_diff_k = "k Other - Self")


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
df_analisis <- df %>%
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
         title = "Correlaciones bivariadas: Parámetros parabólicos 2K1B vs Escalas",
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









































## Correlaciones entre slopes de esfuerzo escalas psicológicas/clínicas


# Cargar datos
df_effort <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Filtrar por grupo
# df_effort = subset(df_effort, grupo!="1")

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
df_analisis <- df %>%
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
         title = "Correlaciones bivariadas: Parámetros parabólicos 2K1B vs Escalas",
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


































# ---------------------------------------------------------------
# Test de normalidad (Shapiro-Wilk)
# ---------------------------------------------------------------
# Con n = 84 sujetos Shapiro-Wilk es preferible a Kolmogorov-Smirnov:
# es más potente para detectar desvíos de la normalidad con n
# pequeño/medio (n < 2000). KS (Lilliefors) conviene sólo con n muy
# grande o cuando se conocen a priori la media y DE poblacionales.


# Cargar datos
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)




# Variables a excluir: identificadores, factores de agrupación y los
# parámetros parabólicos p_2k1b_* (por pedido explícito).
vars_excluir <- c("sub", "grupo",
                  grep("^p_2k1b", names(df), value = TRUE))

vars_test <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of(vars_excluir))

# Aplicar Shapiro-Wilk a cada variable numérica
shapiro_result <- map_dfr(names(vars_test), function(v) {
  x <- vars_test[[v]]
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
  arrange(p_value)

# Resumen en consola (los más alejados de la normalidad primero)
print(shapiro_result, n = Inf)

# Cantidad que cumple / no cumple normalidad a alpha = 0.05
cat("\nVariables normales (p > 0.05):",
    sum(shapiro_result$normal_p05, na.rm = TRUE), "/",
    sum(!is.na(shapiro_result$normal_p05)), "\n")

# Guardar tabla
write.csv(shapiro_result,
          "Datos/shapiro_normalidad.csv",
          row.names = FALSE)




# ---------------------------------------------------------------
# Visualización de las distribuciones
# ---------------------------------------------------------------
# Se generan histogramas (con curva normal teórica) y Q-Q plots
# facetados. Slopes y encuestas van por separado para que los
# slopes queden legibles (son el resultado principal del GLMM).

df_long <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of(vars_excluir)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  drop_na(valor)

vars_slopes <- c("reward_self", "effort_self", "reward_other", "effort_other",
                 "diff_effort", "diff_reward")

df_slopes <- df_long %>% filter(variable %in% vars_slopes)
df_enc    <- df_long %>% filter(!(variable %in% vars_slopes))


plot_hist <- function(d, titulo) {
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
    facet_wrap(~ variable, scales = "free", ncol = 4) +
    labs(title = titulo, x = NULL, y = "Densidad") +
    theme_minimal(base_size = 9) +
    theme(strip.text = element_text(size = 7))
}

plot_qq <- function(d, titulo) {
  ggplot(d, aes(sample = valor)) +
    stat_qq(alpha = 0.55, size = 0.8) +
    stat_qq_line(color = "firebrick", linewidth = 0.5) +
    facet_wrap(~ variable, scales = "free", ncol = 4) +
    labs(title = titulo,
         x = "Cuantiles teóricos (Normal)",
         y = "Cuantiles muestrales") +
    theme_minimal(base_size = 9) +
    theme(strip.text = element_text(size = 7))
}

# Slopes (6 paneles)
ggsave("histogramas_slopes.png",
       plot_hist(df_slopes, "Distribuciones - Slopes GLMM (curva = Normal teórica)"),
       width = 10, height = 6, dpi = 150)

ggsave("qqplots_slopes.png",
       plot_qq(df_slopes, "Q-Q plots - Slopes GLMM"),
       width = 10, height = 6, dpi = 150)

# Encuestas (altura adaptativa al nº de variables)
n_enc <- length(unique(df_enc$variable))
alto  <- max(6, ceiling(n_enc / 4) * 1.8)

ggsave("histogramas_encuestas.png",
       plot_hist(df_enc, "Distribuciones - Encuestas (curva = Normal teórica)"),
       width = 14, height = alto, dpi = 150, limitsize = FALSE)

ggsave("qqplots_encuestas.png",
       plot_qq(df_enc, "Q-Q plots - Encuestas"),
       width = 14, height = alto, dpi = 150, limitsize = FALSE)

