library(readxl)
library(tidyverse)

# Cargar y renombrar — Antiguo
antiguo <- read_excel("Comparacion/params_2k1b_all_families.xlsx") %>%
  select(subject_id,
         grupo,
         k_self_parabolico_antiguo  = p_2k1b_k_self,
         k_other_parabolico_antiguo = p_2k1b_k_other,
         beta_parabolico_antiguo    = p_2k1b_beta,
         k_self_lineal_antiguo      = l_2k1b_k_self,
         k_other_lineal_antiguo     = l_2k1b_k_other,
         beta_lineal_antiguo        = l_2k1b_beta,
         k_self_hiperbolico_antiguo  = h_2k1b_k_self,
         k_other_hiperbolico_antiguo = h_2k1b_k_other,
         beta_hiperbolico_antiguo    = h_2k1b_beta)

# Cargar y renombrar — Nuevo
nuevo <- read_csv("Comparacion/2k1b_modelos.csv") %>%
  select(subject_id = ui,
         k_self_parabolico_nuevo  = p_self_k,
         k_other_parabolico_nuevo = p_other_k,
         beta_parabolico_nuevo    = p_beta,
         k_self_lineal_nuevo      = l_self_k,
         k_other_lineal_nuevo     = l_other_k,
         beta_lineal_nuevo        = l_beta,
         k_self_hiperbolico_nuevo  = h_self_k,
         k_other_hiperbolico_nuevo = h_other_k,
         beta_hiperbolico_nuevo    = h_beta)

# Juntar por subject_id
df <- inner_join(antiguo, nuevo, by = "subject_id")

names(df)


write_csv(df, "Comparacion/parametros_2k1b_antiguo_nuevo.csv")
cat("Guardado: parametros_2k1b_antiguo_nuevo.csv |", nrow(df), "sujetos |", ncol(df), "columnas\n")





###############
# Comparación

library(tidyverse)

df <- read_csv("Comparacion/parametros_2k1b_antiguo_nuevo.csv")


# Pares a comparar
pares <- tribble(
  ~antiguo,                      ~nuevo,                      ~label,
  "k_self_hiperbolico_antiguo",  "k_self_hiperbolico_nuevo",  "k_self_hiperbólico",
  "k_other_hiperbolico_antiguo",  "k_other_hiperbolico_nuevo",  "k_other_hiperbolico",
  "beta_hiperbolico_antiguo",    "beta_hiperbolico_nuevo",    "beta_hiperbólico"
)

pares <- tribble(
  ~antiguo,                      ~nuevo,                      ~label,
  "k_self_parabolico_antiguo",  "k_self_parabolico_nuevo",  "k_self_parabolico",
  "k_other_parabolico_antiguo",  "k_other_parabolico_nuevo",  "k_other_parabolico",
  "beta_parabolico_antiguo",    "beta_parabolico_nuevo",    "beta_parabolico"
)

# Correlaciones
cors <- pares %>%
  rowwise() %>%
  mutate(r = cor(df[[antiguo]], df[[nuevo]], use = "complete.obs"),
         p = cor.test(df[[antiguo]], df[[nuevo]])$p.value) %>%
  ungroup()
print(cors %>% select(label, r, p))


cors <- pares %>%
  rowwise() %>%
  mutate(r = cor(df[[antiguo]], df[[nuevo]], use = "complete.obs", method = "spearman"),
         p = cor.test(df[[antiguo]], df[[nuevo]], method = "spearman")$p.value) %>%
  ungroup()
print(cors %>% select(label, r, p))



# Scatterplots
plots <- pmap(pares, function(antiguo, nuevo, label) {
  r_val <- round(cor(df[[antiguo]], df[[nuevo]], use = "complete.obs", method = "spearman"), 3)
  ggplot(df, aes(x = .data[[antiguo]], y = .data[[nuevo]])) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = label, subtitle = paste0("r = ", r_val),
         x = "Modelo Antiguo", y = "Modelo Nuevo") +
    theme_minimal()
})


plots <- pmap(pares, function(antiguo, nuevo, label) {
  r_val <- round(cor(df[[antiguo]], df[[nuevo]], use = "complete.obs"), 3)
  ggplot(df, aes(x = .data[[antiguo]], y = .data[[nuevo]])) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = label, subtitle = paste0("r = ", r_val),
         x = "Modelo Antiguo", y = "Modelo Nuevo") +
    theme_minimal()
})



library(patchwork)
panel <- wrap_plots(plots, ncol = 3)
ggsave("comparacion_parametros.png", panel, width = 12, height = 4, dpi = 150)


