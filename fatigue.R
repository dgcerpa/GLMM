
# NASA-TLX perceived effort: 2x2 mixed ANOVA + Figure S1
## Single 2x2 mixed ANOVA (difficulty x group); all t-tests are follow-ups of this ANOVA


###############################################################################
# Clean Environment

rm(list = ls())
options(scipen = 999)


###############################################################################
# Libraries

library(tidyverse)
library(afex)


###############################################################################
# Label parameters

## Group labels and colors
LAB_NONVUL <- "Non-vulnerable"   # non-vulnerable group label
LAB_VUL    <- "Vulnerable"       # vulnerable group label
COL_NONVUL <- "#4393C3"          # blue (original palette of this figure)
COL_VUL    <- "#D6604D"          # red

## Convert p-values to significance stars
p_a_estrellas <- function(p) {
  if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else
  if (p < 0.05)  "*"  else if (p < 0.10) "."  else "ns"
}


###############################################################################
# Import Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

## WIDE format (for descriptives, follow-ups and figure)
wide <- df %>%
  select(sub, grupo, NASA_effort_easy_4, NASA_effort_hard_4, NASA_diff) %>%
  mutate(across(c(grupo, NASA_effort_easy_4, NASA_effort_hard_4, NASA_diff), as.numeric)) %>%
  filter(!is.na(NASA_effort_easy_4), !is.na(NASA_effort_hard_4)) %>%
  mutate(grupo = factor(grupo, levels = c(0, 1), labels = c(LAB_NONVUL, LAB_VUL)),
         sub   = factor(sub))

## LONG format (for the repeated-measures ANOVA)
long <- wide %>%
  pivot_longer(c(NASA_effort_easy_4, NASA_effort_hard_4),
               names_to = "difficulty", values_to = "effort") %>%
  mutate(difficulty = factor(ifelse(difficulty == "NASA_effort_easy_4", "easy", "hard"),
                             levels = c("easy", "hard")))

## Sample size by group
cat("n por grupo:\n"); print(table(wide$grupo))


###############################################################################
# Descriptives by group (easy, hard, diff)

descr <- wide %>%
  group_by(grupo) %>%
  summarise(n = n(),
            easy_M = mean(NASA_effort_easy_4), easy_SD = sd(NASA_effort_easy_4),
            hard_M = mean(NASA_effort_hard_4), hard_SD = sd(NASA_effort_hard_4),
            diff_M = mean(NASA_diff),          diff_SD = sd(NASA_diff),
            .groups = "drop")
cat("\n-- Descriptivos por grupo --\n"); print(as.data.frame(descr))


###############################################################################
# 2x2 mixed ANOVA: difficulty (easy/hard) x group

aov_nasa <- aov_ez(id = "sub", dv = "effort", data = long,
                   within = "difficulty", between = "grupo",
                   anova_table = list(es = "pes"))   # pes = partial eta squared
cat("\n== ANOVA mixto 2x2 (difficulty x group) ==\n")
print(aov_nasa)

## Interaction p-value (for the Figure S1 bracket)
p_int <- aov_nasa$anova_table["grupo:difficulty", "Pr(>F)"]
cat(sprintf("\nInteraccion difficulty x group: p = %.4f (%s)\n",
            p_int, p_a_estrellas(p_int)))


###############################################################################
# ANOVA follow-ups

## Manipulation check: hard vs easy (paired) by group
cat("\n== Manipulation check: hard vs easy (paired) por grupo ==\n")
for (g in levels(wide$grupo)) {
  x <- filter(wide, grupo == g)
  cat("\n--", g, "--\n")
  print(t.test(x$NASA_effort_hard_4, x$NASA_effort_easy_4, paired = TRUE))
}

## Specificity: group within each condition (Welch)
cat("\n== Especificidad: grupo dentro de cada nivel (Welch) ==\n")
cat("\n-- easy-only --\n"); print(t.test(NASA_effort_easy_4 ~ grupo, data = wide))
cat("\n-- hard-only --\n"); print(t.test(NASA_effort_hard_4 ~ grupo, data = wide))

## Cohen's d between groups (difference score)
x0 <- wide$NASA_diff[wide$grupo == LAB_NONVUL]
x1 <- wide$NASA_diff[wide$grupo == LAB_VUL]
sp <- sqrt(((length(x0)-1)*var(x0) + (length(x1)-1)*var(x1)) / (length(x0)+length(x1)-2))
cat(sprintf("\nCohen's d (between groups, difference score) = %.2f\n",
            (mean(x0) - mean(x1)) / sp))


###############################################################################
# Figure S1: difference score (Hard - Easy) by group

## Mean and SE by group
diff_bar <- wide %>%
  group_by(grupo) %>%
  summarise(media = mean(NASA_diff), se = sd(NASA_diff) / sqrt(n()), .groups = "drop")

## Significance bracket (uses the ANOVA interaction p-value, not a separate Welch test)
diff_y_top <- max(c(diff_bar$media + diff_bar$se, wide$NASA_diff), na.rm = TRUE)

diff_sig <- data.frame(
  x_start   = 1,
  x_end     = 2,
  y_bracket = diff_y_top * 1.05,
  y_label   = diff_y_top * 1.08,
  y_top     = diff_y_top,
  etiqueta  = p_a_estrellas(p_int)          # <- star derived from the ANOVA
)

## Plot
p_diff <- ggplot(diff_bar, aes(x = grupo, y = media, fill = grupo)) +
  geom_col(width = 0.5, color = "gray30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.15) +
  geom_jitter(data = wide, aes(x = grupo, y = NASA_diff, color = grupo),
              inherit.aes = FALSE, width = 0.08, alpha = 0.6, size = 1.8) +
  scale_color_manual(values = setNames(c(COL_NONVUL, COL_VUL), c(LAB_NONVUL, LAB_VUL)),
                     guide = "none") +
  scale_fill_manual(values = setNames(c(COL_NONVUL, COL_VUL), c(LAB_NONVUL, LAB_VUL)),
                    guide = "none") +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_end, y = y_bracket, yend = y_bracket)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_start, xend = x_start, y = y_bracket, yend = y_bracket - y_top * 0.015)) +
  geom_segment(data = diff_sig, inherit.aes = FALSE,
               aes(x = x_end, xend = x_end, y = y_bracket, yend = y_bracket - y_top * 0.015)) +
  geom_text(data = diff_sig, inherit.aes = FALSE,
            aes(x = (x_start + x_end) / 2, y = y_label, label = etiqueta), size = 6) +
  labs(x = "", y = "Differences on perceived effort (Hard - Easy)") +
  theme_classic(base_size = 12)

## Print and save figure
print(p_diff)
ggsave("figure4.png", p_diff, width = 6, height = 6, dpi = 300, bg = "white")


###############################################################################
# Explicit test of the interaction: between-group t-test on the difference score (hard - easy)

## In a 2x2 mixed ANOVA, the difficulty x group interaction equals a Student t-test on NASA_diff (t^2 = F, same p)
cat("\n== Difference score (hard - easy) entre grupos ==\n")

## Student t-test (exact equivalent of the ANOVA interaction)
tt_diff <- t.test(NASA_diff ~ grupo, data = wide, var.equal = TRUE)
print(tt_diff)
cat(sprintf("Chequeo equivalencia: t^2 = %.3f vs F interacción = %.3f\n",
            unname(tt_diff$statistic)^2,
            aov_nasa$anova_table["grupo:difficulty", "F"]))

## Mean difference NV - SV with 95% CI (from the Student t-test)
cat(sprintf("Mean difference (NV - SV) = %.2f, 95%% CI [%.2f, %.2f]\n",
            unname(tt_diff$estimate[1] - tt_diff$estimate[2]),
            tt_diff$conf.int[1], tt_diff$conf.int[2]))
