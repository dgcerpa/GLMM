
# NASA-TLX perceived effort: 2x2 mixed ANOVA
## Single 2x2 mixed ANOVA (difficulty x group); all t-tests are follow-ups of this ANOVA
## Run before GLMM_success.R: 'wide' and 'p_int' are reused there for Figure 5A

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

## Group labels
LAB_NONVUL <- "Non-vulnerable"   # non-vulnerable group label
LAB_VUL    <- "Vulnerable"       # vulnerable group label


###############################################################################
# Import Data

df <- read.csv("dataset_final.csv", stringsAsFactors = FALSE)

## WIDE format (for descriptives, follow-ups and Figure 5A in GLMM_success.R)
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

## Interaction p-value (for the Figure 5A bracket in GLMM_success.R)
p_int <- aov_nasa$anova_table["grupo:difficulty", "Pr(>F)"]
cat(sprintf("\nInteraccion difficulty x group: p = %.4f\n", p_int))


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

