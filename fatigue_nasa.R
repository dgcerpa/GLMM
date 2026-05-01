

## Extracción de columnas de Fatiga y NASA desde datos_limpios.csv
## Filtra por sujetos presentes en dataset_full.csv y anexa grupo


## Librerías

library(dplyr)


#########################
## Data

# Referencia de sujetos (ID y grupo) desde dataset_full
ref <- read.csv("dataset_full.csv", stringsAsFactors = FALSE) %>% select(ID = sub, grupo)

# Extrae columnas de interés, calza con ref, calcula NASA_diff y ordena por ID
read.csv("Datos/datos_limpios.csv", stringsAsFactors = FALSE) %>%
  select(ID = ID_check, Fatigue_pre_7, Fatigue_post_7, NASA_effort_easy_4, NASA_effort_hard_4) %>%
  inner_join(ref, by = "ID") %>%
  mutate(NASA_diff = NASA_effort_hard_4 - NASA_effort_easy_4) %>%
  mutate(Fatigue_diff = Fatigue_post_7 - Fatigue_pre_7) %>%
  select(ID, grupo, everything()) %>%
  arrange(ID) %>%
  write.csv("Datos/fatigue_nasa.csv", row.names = FALSE)


#########################
## Integración de fatigue_nasa.csv a dataset_full.csv
## Une las variables de fatiga y NASA al dataset principal por ID/sub

dataset_full <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

fatigue_nasa <- read.csv("Datos/fatigue_nasa.csv", stringsAsFactors = FALSE) %>%
  select(-grupo) %>%
  rename(sub = ID)

dataset_full %>%
  left_join(fatigue_nasa, by = "sub") %>%
  write.csv("dataset_full.csv", row.names = FALSE)





#########################

## Correlaciones entre parámetros del modelo parabólico 2K1B y escalas psicológicas/clínicas

# Librerías

library(tidyverse)
library(Hmisc)

# Cargar datos
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Filtrar por grupo
# df = subset(df, grupo!="1")

# Variables dependientes (modelo parabólico) 
vd_names <- c("NASA_effort_easy_4", "NASA_effort_hard_4", "NASA_diff")
vd_labels <- c(NASA_effort_easy_4 = "NASA Easy", NASA_effort_hard_4 = "NASA Hard", NASA_diff = "NASA Diff")


# Escalas (totales y subdimensiones)
# Excluidas: 
# AIM_DIRt (categorica), 
# ASSIST_Total_DIRd (duplicado de ASSIST_DIRt)
# PSQQ y subdimensiones
# AIM y subdimensiones
escalas <- c("p_2k1b_k_self", "p_2k1b_k_other", "p_2k1b_diff_k",
             "effort_self", "effort_other", "diff_effort",
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
             "ASSIST_ConsumoDrogasLegales_DIRd", "ASSIST_ConsumoDrogasIlegales_DIRd")

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
         title = "Correlaciones bivariadas: NASA vs Escalas y parámetros",
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

