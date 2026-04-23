################################################################################
## regresiones_IRI_diff_effort.R
## Regresiones múltiples con diff_effort como VD y la empatía (IRI) como VI,
## bajo tres operacionalizaciones distintas:
##   M1 = IRI total
##   M2 = 4 subescalas (Fantasia, TomaPerspectiva, PreocEmpatica, IncomodidadPers)
##   M3 = 2 compuestos: (Fantasia + TomaPerspectiva) y (PreocEmp + IncomodidadPers)
################################################################################

library(tidyverse)
library(car)        # vif()
library(broom)      # tidy() / glance()
library(performance) # check_model() opcional

# --- Cargar datos ---
df <- read.csv("dataset_full.csv", stringsAsFactors = FALSE)

# Mismo filtro que el script de correlaciones para diff_effort
df <- subset(df, grupo != "1")

# --- Construir compuestos IRI para M3 ---
df <- df %>%
  mutate(
    IRI_Cognitivo = IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd,
    IRI_Afectivo  = IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd
  )

# --- Subset analítico (casos completos en todas las variables usadas) ---
vars_usadas <- c("diff_effort",
                 "IRI_DIRt",
                 "IRI_Fantasia_DIRd", "IRI_TomaPerspectiva_DIRd",
                 "IRI_PreocupacionEmpatica_DIRd", "IRI_IncomodidadPersonal_DIRd",
                 "IRI_Cognitivo", "IRI_Afectivo")

df_mod <- df %>%
  select(all_of(vars_usadas)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

cat("N usado en los modelos:", nrow(df_mod), "\n\n")

################################################################################
## Modelo 1: IRI total
################################################################################
cat("================================================================\n")
cat("Modelo 1: diff_effort ~ IRI_DIRt\n")
cat("================================================================\n")

m1 <- lm(diff_effort ~ IRI_DIRt, data = df_mod)
print(summary(m1))
cat("\nR2 =", summary(m1)$r.squared,
    " | R2 adj =", summary(m1)$adj.r.squared, "\n\n")

################################################################################
## Modelo 2: 4 subescalas IRI
################################################################################
cat("================================================================\n")
cat("Modelo 2: diff_effort ~ Fantasia + TomaPerspectiva +\n")
cat("                        PreocEmpatica + IncomodidadPersonal\n")
cat("================================================================\n")

m2 <- lm(diff_effort ~ IRI_Fantasia_DIRd + IRI_TomaPerspectiva_DIRd +
                        IRI_PreocupacionEmpatica_DIRd + IRI_IncomodidadPersonal_DIRd,
         data = df_mod)
print(summary(m2))
cat("\nR2 =", summary(m2)$r.squared,
    " | R2 adj =", summary(m2)$adj.r.squared, "\n")
cat("\nVIF (multicolinealidad):\n")
print(vif(m2))
cat("\n")

################################################################################
## Modelo 3: 2 compuestos (cognitivo / afectivo)
################################################################################
cat("================================================================\n")
cat("Modelo 3: diff_effort ~ IRI_Cognitivo + IRI_Afectivo\n")
cat("    Cognitivo = Fantasia + TomaPerspectiva\n")
cat("    Afectivo  = PreocEmpatica + IncomodidadPersonal\n")
cat("================================================================\n")

m3 <- lm(diff_effort ~ IRI_Cognitivo + IRI_Afectivo, data = df_mod)
print(summary(m3))
cat("\nR2 =", summary(m3)$r.squared,
    " | R2 adj =", summary(m3)$adj.r.squared, "\n")
cat("\nVIF (multicolinealidad):\n")
print(vif(m3))
cat("\n")

################################################################################
## Tabla resumen de los 3 modelos
################################################################################
cat("================================================================\n")
cat("Resumen comparativo de los 3 modelos\n")
cat("================================================================\n")

resumen <- bind_rows(
  glance(m1) %>% mutate(modelo = "M1: IRI total"),
  glance(m2) %>% mutate(modelo = "M2: 4 subescalas"),
  glance(m3) %>% mutate(modelo = "M3: 2 compuestos")
) %>%
  select(modelo, r.squared, adj.r.squared, statistic, p.value, df, df.residual, AIC, BIC)

print(resumen)

# --- Coeficientes de todos los modelos en una tabla ---
coefs <- bind_rows(
  tidy(m1) %>% mutate(modelo = "M1"),
  tidy(m2) %>% mutate(modelo = "M2"),
  tidy(m3) %>% mutate(modelo = "M3")
) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(modelo, term, estimate, std.error, statistic, p.value, sig)

cat("\nCoeficientes:\n")
print(coefs, n = Inf)

# --- Guardar tablas (descomentar si se quieren exportar) ---
# write.csv(resumen, "regresiones_IRI_resumen.csv", row.names = FALSE)
# write.csv(coefs,   "regresiones_IRI_coeficientes.csv", row.names = FALSE)
