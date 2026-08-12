

## Correlaciones entre parámetros del modelo parabólico 2K1B y escalas psicológicas/clínicas
# Diego Garrido Cerpa - Viña del Mar 2026


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






########################################################
## Correlaciones entre slopes de EFFORT escalas psicológicas/clínicas


# Cargar datos
df_effort <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

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
         title = "Correlaciones bivariadas: Slopes de Effort 2K1B vs Escalas",
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










######################################
## Heatmap


# Cargar datos
df_heat <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)

# Filtrar por grupo (opcional)
# df_heat = subset(df_heat, grupo != "0")

# Crear subdimensiones derivadas del IRI
df_heat <- df_heat %>%
  mutate(
    IRI_Cognitivo = IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd,
    IRI_Afectivo  = IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd
  )

# Variables que entran en la matriz (slopes + escalas de interes).
# Editar libremente esta lista para incluir/excluir variables.
vars_heatmap <- c(
  # Slopes GLMM
  "effort_self", "effort_other", "diff_effort",
  # Escalas de interes
  "IRI_DIRt", "IRI_Cognitivo", "IRI_Afectivo",
  "MAIA_DIRt", "SASS_DIRt",
  "NASA_effort_easy_4", "NASA_effort_hard_4", "NASA_diff",
  
  # Escalas MAIA
  "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
  "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
  "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd", 
  "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd"
  
)

# Etiquetas legibles para los ejes
var_labels <- c(
  effort_self      = "Effort Self",
  effort_other     = "Effort Other",
  diff_effort      = "Effort Other - Self",
  IRI_DIRt         = "IRI Total",
  IRI_Cognitivo    = "IRI Cognitivo",
  IRI_Afectivo     = "IRI Afectivo",
  MAIA_DIRt        = "MAIA Total",
  SASS_DIRt        = "SASS Total",
  NASA_effort_easy_4 = "NASA Effort Easy",
  NASA_effort_hard_4 = "NASA Effort Hard",
  NASA_diff          = "NASA Diff",
  
  MAIA_Percibir_DIRd = "MAIA Percibir",
  MAIA_AusenciaDistraccion_DIRd = "MAIA Ausencia Distracción",
  MAIA_AusenciaPreocupacion_DIRd = "MAIA Ausencia Preocupación",
  MAIA_RegulacionAtencion_DIRd = "MAIA Regulacion de Atención",
  MAIA_ConcienciaEmocional_DIRd = "MAIA Conciencia Emocional",
  MAIA_Autorregulacion_DIRd = "MAIA Autorregulación",
  MAIA_EscuchaCuerpo_DIRd = "MAIA Escucha del cuerpo",
  MAIA_Confianza_DIRd = "MAIA Confianza"
)

# Preparar matriz numerica
mat_heat <- df_heat %>%
  select(all_of(vars_heatmap)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

cat("N por variable:\n")
print(colSums(!is.na(mat_heat)))

# Correlaciones de Spearman (pareadas, descarta NAs por par)
corr_h <- rcorr(mat_heat, type = "pearson")

# Pasar matrices rho y p-values a formato largo
rho_long <- as.data.frame(corr_h$r) %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "rho")

p_long <- as.data.frame(corr_h$P) %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "p_value")

heat_df <- rho_long %>%
  left_join(p_long, by = c("Var1", "Var2")) %>%
  mutate(
    # rcorr deja NA en la diagonal para p; rho diagonal = 1
    p_value = ifelse(Var1 == Var2, NA_real_, p_value),
    sig = case_when(
      is.na(p_value)   ~ "",
      p_value < 0.001  ~ "***",
      p_value < 0.01   ~ "**",
      p_value < 0.05   ~ "*",
      p_value < 0.10   ~ ".",
      TRUE             ~ ""
    ),
    etiqueta = ifelse(Var1 == Var2,
                      "1",
                      paste0(sprintf("%.2f", rho), sig)),
    Var1_lab = factor(var_labels[Var1], levels = var_labels[vars_heatmap]),
    Var2_lab = factor(var_labels[Var2], levels = rev(var_labels[vars_heatmap]))
  )


# Heatmap
p_heat <- ggplot(heat_df, aes(x = Var1_lab, y = Var2_lab, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = etiqueta), size = 3) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1), name = "Pearson"
  ) +
  scale_x_discrete(position = "top") +
  coord_fixed() +
  labs(
    title = "Matriz de correlaciones (Pearson)",
    subtitle = "Asteriscos: *** p<.001, ** p<.01, * p<.05, . p<.10",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0),
    panel.grid = element_blank(),
    legend.position = "right"
  )


print(p_heat)
ggsave("correlaciones_heatmap_experimental.png", p_heat,
       width = 10, height = 9, dpi = 150)
cat("\n-> Guardado: correlaciones_heatmap.png\n")






########################################################
## Correlaciones entre slopes de REWARD y escalas psicológicas/clínicas


# Cargar datos
df_effort <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Filtrar por grupo
df_effort = subset(df_effort, grupo!="1")

# Variables dependientes (slopes de effort)
vd_names <- c("reward_self", "reward_other", "diff_reward")
vd_labels <- c(reward_self = "Reward Self", reward_other = "Reward Other", diff_reward = "Reward Other - Self")


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
corr <- rcorr(as.matrix(df_analisis), type = "pearson")

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
         title = "Correlaciones bivariadas: Slopes de Reward vs Escalas",
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

