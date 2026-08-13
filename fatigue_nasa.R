
## Barplots: NASA y Fatigue por grupo
# Diego Garrido Cerpa - Viña del Mar 2026


## Librerías

library(tidyverse)


## Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)


###################################
## Helper: barplot 2 condiciones x grupo con t-test entre grupos por condición

barplot_2x2 <- function(data, cols, labels, title, ylab, outfile) {
  long <- data %>%
    select(grupo, all_of(cols)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(all_of(cols), names_to = "Condicion", values_to = "y") %>%
    filter(!is.na(y)) %>%
    mutate(Condicion = factor(Condicion, levels = cols, labels = labels),
           grupo     = factor(grupo, levels = c(0, 1),
                              labels = c("Control", "Experimental")))

  bar <- long %>%
    group_by(Condicion, grupo) %>%
    summarise(media = mean(y), se = sd(y) / sqrt(n()), .groups = "drop")

  # Etiqueta de significancia por condición (t de Student entre grupos)
  p_a_estrellas <- function(p) {
    if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05)  "*"  else if (p < 0.10) "."  else "ns"
  }

  sig <- long %>%
    group_by(Condicion) %>%
    summarise(p_value = t.test(y ~ grupo)$p.value, .groups = "drop") %>%
    left_join(bar %>% group_by(Condicion) %>%
                summarise(y_top = max(media + se), .groups = "drop"),
              by = "Condicion") %>%
    mutate(etiqueta  = vapply(p_value, p_a_estrellas, character(1)),
           x         = as.numeric(Condicion),
           y_bracket = y_top * 1.05,
           y_label   = y_top * 1.08)

  dodge_off <- 0.8 / 2 / 2

  p <- ggplot(bar, aes(x = Condicion, y = media, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7,
             color = "gray30", linewidth = 0.3) +
    geom_errorbar(aes(ymin = media - se, ymax = media + se),
                  position = position_dodge(width = 0.8), width = 0.2) +
    # Corchete + asterisco de significancia sobre cada condición
    geom_segment(data = sig, inherit.aes = FALSE,
                 aes(x = x - dodge_off, xend = x + dodge_off,
                     y = y_bracket, yend = y_bracket)) +
    geom_segment(data = sig, inherit.aes = FALSE,
                 aes(x = x - dodge_off, xend = x - dodge_off,
                     y = y_bracket, yend = y_bracket - y_top * 0.015)) +
    geom_segment(data = sig, inherit.aes = FALSE,
                 aes(x = x + dodge_off, xend = x + dodge_off,
                     y = y_bracket, yend = y_bracket - y_top * 0.015)) +
    geom_text(data = sig, inherit.aes = FALSE,
              aes(x = x, y = y_label, label = etiqueta), size = 6) +
    scale_fill_manual(values = c("Control" = "#4393C3",
                                 "Experimental" = "#D6604D"),
                      name = "Grupo") +
    labs(x = "", y = ylab, title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  print(sig[, c("Condicion", "p_value", "etiqueta")])
  print(p)
  ggsave(outfile, p, width = 8, height = 6, dpi = 150)
}


###################################
## Barplots

## NASA-TLX easy vs hard por grupo
barplot_2x2(df,
            cols   = c("NASA_effort_easy_4", "NASA_effort_hard_4"),
            labels = c("NASA Easy", "NASA Hard"),
            title  = "Esfuerzo percibido en condición fácil y difícil por grupo",
            ylab   = "Esfuerzo percibido (NASA-TLX)",
            outfile = "nasa_easy_hard_por_grupo.png")

## Fatigue pre vs post por grupo
barplot_2x2(df,
            cols   = c("Fatigue_pre_7", "Fatigue_post_7"),
            labels = c("Fatigue Pre", "Fatigue Post"),
            title  = "Fatiga percibida antes y después de la tarea por grupo",
            ylab   = "Fatiga percibida",
            outfile = "fatigue_pre_post_por_grupo.png")
