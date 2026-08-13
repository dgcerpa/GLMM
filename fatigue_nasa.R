
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
                        labels = c("Control", "Experimental")))

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

## Graph

p_diff <- ggplot(diff_bar, aes(x = grupo, y = media, fill = grupo)) +
  geom_col(width = 0.5, color = "gray30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.15) +
  geom_jitter(data = diff_data, aes(x = grupo, y = NASA_diff, color = grupo),
              inherit.aes = FALSE, width = 0.08, alpha = 0.6, size = 1.8) +
  scale_color_manual(values = c("Control" = "#4393C3", "Experimental" = "#D6604D"),
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
  scale_fill_manual(values = c("Control" = "#4393C3", "Experimental" = "#D6604D"),
                    guide = "none") +
  labs(x = "", y = "Diferencia de esfuerzo percibido (Hard - Easy)",
       title = "Diferencia NASA-TLX (hard - easy) entre grupos") +
  theme_classic(base_size = 12)

cat("NASA_diff:  p =", format.pval(diff_pval, digits = 3),
    " (", p_a_estrellas(diff_pval), ")\n", sep = "")
print(p_diff)
ggsave("nasa_diff_por_grupo.png", p_diff, width = 6, height = 6, dpi = 150)

