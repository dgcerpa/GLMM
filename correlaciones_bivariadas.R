################################################################################
## analisis_correlaciones_bivariadas.R
## Correlaciones de Spearman entre parámetros del modelo hiperbólico 2K1B
## (h_self_k, h_other_k, h_other_self_k) y escalas psicológicas/clínicas
################################################################################

library(tidyverse)
library(Hmisc)

# --- Cargar datos ---
df <- read.csv("dataset_completo.csv", stringsAsFactors = FALSE)

# --- Variables dependientes (modelo hiperbólico) ---
vd_names <- c("h_self_k", "h_other_k", "h_other_self_k")
vd_labels <- c(h_self_k = "k Self", h_other_k = "k Other", h_other_self_k = "k Other - Self")

# --- Escalas (totales y subdimensiones) ---
escalas <- c(
  # Totales
  "CSI_DIRt", "GHQ12_DIRt", "IRI_DIRt", "LSNS_DIRt", "MAIA_DIRt",
  "PSQQ_DIRt", "PSS_DIRt", "RMET_DIRt", "SASS_DIRt", "SWBS_DIRt",
  "UCLA_DIRt", "WAST_DIRt", "ASSIST_DIRt",
  # Subdimensiones
  "DASS21_depresion_DIRd", "DASS21_ansiedad_DIRd", "DASS21_estres_DIRd",
  "IFS_Total_DIRd",
  "IRI_TomaPerspectiva_DIRd", "IRI_Fantasia_DIRd",
  "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd",
  "LSNS_Familiares_DIRd", "LSNS_Amistados_DIRd",
  "SWBS_IntegracionSocial_DIRd", "SWBS_AceptacionSocial_DIRd",
  "SWBS_ContribucionSocial_DIRd", "SWBS_ActualizacionSocial_DIRd",
  "SWBS_CoherenciaSocial_DIRd"
)

# --- Preparar datos ---
df_analisis <- df %>%
  select(all_of(c(vd_names, escalas))) %>%
  mutate(across(everything(), as.numeric))

mat <- df_analisis[complete.cases(df_analisis), ]
cat("N para correlaciones:", nrow(mat), "\n")

# --- Correlaciones de Spearman ---
corr <- rcorr(as.matrix(mat), type = "spearman")

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

# --- Imprimir significativas ---
cat("\n--- Correlaciones significativas (p < 0.05) ---\n")
corr_df %>% filter(p_value < 0.05) %>% 
  arrange(VD, p_value) %>% 
  print(n = 50)

cat("\n--- Tendencias (p < 0.10) ---\n")
corr_df %>% filter(p_value >= 0.05, p_value < 0.10) %>% 
  arrange(VD, p_value) %>% 
  print(n = 50)

# --- Guardar tabla completa ---
write.csv(corr_df, "correlaciones_bivariadas.csv", row.names = FALSE)

# --- Gráfico: Barplot de rho por VD ---
plot_df <- corr_df %>%
  mutate(
    Escala_label = gsub("_DIRt|_DIRd", "", Escala),
    VD_label = vd_labels[VD],
    sig_color = case_when(
      p_value < 0.05 ~ "p < .05",
      p_value < 0.10 ~ "p < .10",
      TRUE ~ "n.s."
    )
  )

p <- ggplot(plot_df, aes(x = reorder(Escala_label, rho), y = rho, fill = sig_color)) +
  geom_col(width = 0.7, color = "gray30", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  coord_flip() +
  facet_wrap(~VD_label, ncol = 3) +
  scale_fill_manual(values = c("p < .05" = "#D6604D",
                               "p < .10" = "#FDB863",
                               "n.s."    = "#D9D9D9")) +
  labs(x = "", y = "Spearman rho", fill = "",
       title = "Correlaciones bivariadas: Parámetros hiperbólicos 2K1B vs Escalas") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

ggsave("correlaciones_bivariadas_barplot.png", p, width = 14, height = 7, dpi = 150)
cat("\n-> Guardado: correlaciones_bivariadas_barplot.png\n")