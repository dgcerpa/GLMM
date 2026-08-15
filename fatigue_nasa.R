
## Barplot: NASA_diff entre grupos
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)


## Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

# Mapeo de p-valor a etiqueta de significancia
p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
  if (p < 0.05)  "*"  else if (p < 0.10) "."  else "ns"
}


###################################
## NASA_diff (hard - easy) entre grupos (barplot 2 grupos con un solo corchete)

diff_data <- df %>%
  select(grupo, NASA_diff) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(NASA_diff)) %>%
  mutate(grupo = factor(grupo, levels = c(0, 1),
                        labels = c("Control", "Vulnerable")))

diff_bar <- diff_data %>%
  group_by(grupo) %>%
  summarise(media = mean(NASA_diff), se = sd(NASA_diff) / sqrt(n()), .groups = "drop")

diff_pval  <- t.test(NASA_diff ~ grupo, data = diff_data)$p.value
# y_top incluye tanto el tope del error bar como el máximo individual
diff_y_top <- max(c(diff_bar$media + diff_bar$se, diff_data$NASA_diff), na.rm = TRUE)

diff_sig <- data.frame(
  x_start   = 1,
  x_end     = 2,
  y_bracket = diff_y_top * 1.05,
  y_label   = diff_y_top * 1.08,
  y_top     = diff_y_top,
  etiqueta  = p_a_estrellas(diff_pval)
)


###################################
## NASA-TLX: estadísticos para la sección NASA

nasa <- df %>%
  select(grupo, NASA_effort_easy_4, NASA_effort_hard_4, NASA_diff) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(NASA_diff)) %>%
  mutate(grupo = factor(grupo, levels = c(0, 1), labels = c("Control", "Vulnerable")))

cat("n por grupo:\n"); print(table(nasa$grupo))

## 1) Descriptivos por grupo (easy, hard, diff)
descr <- nasa %>%
  group_by(grupo) %>%
  summarise(n = n(),
            easy_M = mean(NASA_effort_easy_4), easy_SD = sd(NASA_effort_easy_4),
            hard_M = mean(NASA_effort_hard_4), hard_SD = sd(NASA_effort_hard_4),
            diff_M = mean(NASA_diff),          diff_SD = sd(NASA_diff),
            .groups = "drop")
cat("\n-- Descriptivos por grupo --\n"); print(as.data.frame(descr))

cat("\n-- Global --\n")
cat(sprintf("easy: M = %.2f, SD = %.2f\n", mean(nasa$NASA_effort_easy_4), sd(nasa$NASA_effort_easy_4)))
cat(sprintf("hard: M = %.2f, SD = %.2f\n", mean(nasa$NASA_effort_hard_4), sd(nasa$NASA_effort_hard_4)))
cat(sprintf("diff: M = %.2f, SD = %.2f\n", mean(nasa$NASA_diff), sd(nasa$NASA_diff)))

## 2) Manipulation check: hard vs easy (pareado, muestra completa)
cat("\n== Manipulation check: hard vs easy (paired, full sample) ==\n")
print(t.test(nasa$NASA_effort_hard_4, nasa$NASA_effort_easy_4, paired = TRUE))
cat(sprintf("Cohen's dz = %.2f\n", mean(nasa$NASA_diff) / sd(nasa$NASA_diff)))

# hard vs easy por grupo (one-sample: NASA_diff vs 0)
for (g in c("Control", "Vulnerable")) {
  x <- nasa$NASA_diff[nasa$grupo == g]
  cat("\n--", g, ": NASA_diff vs 0 --\n"); print(t.test(x, mu = 0))
}

## 3) Comparación entre grupos del NASA_diff (Welch)
cat("\n== Between-group: NASA_diff ~ grupo (Welch) ==\n")
print(t.test(NASA_diff ~ grupo, data = nasa))
# Cohen's d pooled (manual, sin dependencias)
x0 <- nasa$NASA_diff[nasa$grupo == "Control"]
x1 <- nasa$NASA_diff[nasa$grupo == "Vulnerable"]
sp <- sqrt(((length(x0)-1)*var(x0) + (length(x1)-1)*var(x1)) / (length(x0)+length(x1)-2))
cat(sprintf("Cohen's d (between groups) = %.2f\n", (mean(x0) - mean(x1)) / sp))

## 4) (opcional) comparación entre grupos dentro de cada condición
cat("\n-- Group comparison within each condition --\n")
print(t.test(NASA_effort_easy_4 ~ grupo, data = nasa))
print(t.test(NASA_effort_hard_4 ~ grupo, data = nasa))



## Graph

p_diff <- ggplot(diff_bar, aes(x = grupo, y = media, fill = grupo)) +
  geom_col(width = 0.5, color = "gray30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.15) +
  geom_jitter(data = diff_data, aes(x = grupo, y = NASA_diff, color = grupo),
              inherit.aes = FALSE, width = 0.08, alpha = 0.6, size = 1.8) +
  scale_color_manual(values = c("Control" = "#4393C3", "Vulnerable" = "#D6604D"),
                     guide = "none") +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_end,
                   y = y_bracket, yend = y_bracket)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_start,
                   y = y_bracket, yend = y_bracket - y_top * 0.015)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_end, xend = x_end,
                   y = y_bracket, yend = y_bracket - y_top * 0.015)) +
  geom_text(data = diff_sig, inherit.aes = FALSE,
            aes(x = (x_start + x_end) / 2, y = y_label, label = etiqueta), size = 6) +
  scale_fill_manual(values = c("Control" = "#4393C3", "Vulnerable" = "#D6604D"),
                    guide = "none") +
  labs(x = "", y = "Differences on perceived effort (Hard - Easy)",
       title = "NASA-TLX Difference between groups") +
  theme_classic(base_size = 12)

cat("NASA_diff:  p =", format.pval(diff_pval, digits = 3),
    " (", p_a_estrellas(diff_pval), ")\n", sep = "")
print(p_diff)
ggsave("nasa_diff_por_grupo.png", p_diff, width = 6, height = 6, dpi = 150)

