

## Test de normalidad
# Diego Garrido Cerpa - Viña del Mar 2026

# Librerías

library(tidyverse)
library(Hmisc)


####################################################
## Test de normalidad Shapiro-Wilk

# Cargar datos
df <- read.csv("dataset_full_final.csv", stringsAsFactors = FALSE)



# Variables a evaluar, agrupadas por encuesta / dominio
grupos_vars <- list(
  MAIA = c("MAIA_DIRt",
           "MAIA_Percibir_DIRd", "MAIA_AusenciaDistraccion_DIRd",
           "MAIA_AusenciaPreocupacion_DIRd", "MAIA_RegulacionAtencion_DIRd",
           "MAIA_ConcienciaEmocional_DIRd", "MAIA_Autorregulacion_DIRd",
           "MAIA_EscuchaCuerpo_DIRd", "MAIA_Confianza_DIRd"),
  IRI  = c("IRI_DIRt",
           "IRI_TomaPerspectiva_DIRd", "IRI_Fantasia_DIRd",
           "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd"),
  SWBS = c("SWBS_DIRt",
           "SWBS_IntegracionSocial_DIRd", "SWBS_AceptacionSocial_DIRd",
           "SWBS_ContribucionSocial_DIRd", "SWBS_ActualizacionSocial_DIRd",
           "SWBS_CoherenciaSocial_DIRd"),
  SASS   = c("SASS_DIRt"),
  Effort = c("diff_effort", "effort_other"),
  NASA   = c("NASA_diff")
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

# NASA Diff NO es normal
# Effort Other NO es normal

# Guardar tabla
# write.csv(ks_result,
#           "Datos/ks_normalidad.csv",
#           row.names = FALSE)

