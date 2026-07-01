# GLMM

Statistical analysis of the prosocial effort discounting task: GLMMs on trial-by-trial decisions, individual-slope extraction, regressions against psychological scales, partial correlations, mediation, robust models, and network analysis.

## Context

Data-analysis repo for the prosocial effort task comparing a Control group vs. a Vulnerable (Experimental) group. Primary DV is `diff_effort` — the difference (Other − Self) in effort slopes extracted from a decision-level GLMM. Trial-level cleaning happens upstream in `dgcerpa/PET_Data_Cleaning`; this repo takes the cleaned outputs and runs everything downstream. Diego Garrido (analysis), Sebastián Contreras (supervisor), with contributions from Nicolás and José. Private — internal documentation, not for public distribution.

## Analyses

Main dataset consumed by almost every script: `dataset_full_final.csv` (subject-level; 84 rows, ~120 columns including scale totals/subscales, GLMM-derived slopes, computational model parameters, and demographics).

### Main GLMM models

- **`GLMM_Corregido.R`** — Core script. Filters trials, standardizes continuous predictors (z-scores, `c.*` prefix), fits four candidate GLMMs on `decision` (`m1`–`m4`) and four on `success`, compares them via `anova()`, and runs post-hoc slope contrasts with `emtrends` / `emmeans` (FDR-adjusted). Extracts per-subject slopes (`reward_self`, `effort_self`, `reward_other`, `effort_other`, `diff_effort`, `diff_reward`) from per-agent GLMMs (`lmself_rs.sc`, `lmother_rs.sc`) and writes `post_hoc_v2.csv`. Joins slopes with questionnaire scores and computational-model parameters (`params_2k1b_all_families.xlsx`) into `dataset_full_v2.csv`. Also produces the post-hoc figures (slope forest plot, pairwise-contrast plot, predicted-probability curves) under `Figuras/`.

- **`GLMM_DASS.R`** — Extends the decision GLMM with `Fatigue_diff` as a covariate and adds a scale-level LM (`diff_effort ~ MAIA_DIRt * grupo + Fatigue_diff`). Runs partial correlations `SASS_DIRt ~ diff_effort` per group controlling for `Fatigue_diff` (and for `grupo` in the pooled sample) with FDR adjustment. Includes a `residualize()` helper that residualizes MAIA subscales on `Fatigue_diff` within group before refitting the subscale-by-group interaction model.

### Mediation and partial correlations

- **`modelos_unificados.R`** — Consolidated mediation/GGM script; supersedes earlier `analisis_correlaciones_parciales*.R` / `analisis_mediacion*.R` (which it documents as redundant).
  - **CP1**: pairwise partial Pearson correlations across `{diff_effort, MAIA_DIRt, SASS_DIRt}` in the full sample.
  - **CP2**: same partial correlations run separately in Control and Vulnerable.
  - **CP3**: Fisher's z comparison of each edge between groups.
  - **M1**: simple mediation `grupo → MAIA → diff_effort` (lavaan SEM, 5000 BCa bootstraps).
  - **M2**: simple mediation `grupo → SASS → diff_effort`.
  - **M3**: parallel mediation `grupo → {MAIA, SASS} → diff_effort` with correlated mediators.
  - **M4**: moderated mediation (Hayes model 14) — `grupo` moderates the `b` path via SASS; reports conditional indirect effects and the moderated-mediation index.
  - **M5**: simple mediation `MAIA → SASS → diff_effort` restricted to the Vulnerable subgroup.
  - Also renders GGM plots (three-node network, MAIA / SASS / diff_effort) for the full sample and per group using `ggforce` + `patchwork`, plus a consolidated table of indirect effects flagged by whether the BCa CI excludes zero.

### Regressions by instrument

Each script builds a subject-level analytic subset and fits LMs of `diff_effort` (and `effort_other`) on the instrument's total score and its subscales, both without and with a `grupo` interaction. `ggeffects::ggpredict` is used for interaction plots.

- **`regresiones_MAIA.R`** — MAIA total and its 8 subscales (Percibir, AusenciaDistraccion, AusenciaPreocupacion, RegulacionAtencion, ConcienciaEmocional, Autorregulacion, EscuchaCuerpo, Confianza). Interaction plot for MAIA total × group.
- **`regresiones_IFS.R`** — IFS total and its 8 subscales; also includes SASS total models (`diff_effort` and `effort_other`) with the `SASS × grupo` interaction plot.
- **`regresiones_IRI.R`** — IRI total, its 4 subscales, and derived composites `IRI_Cognitivo` (Fantasía + Toma de Perspectiva) and `IRI_Afectivo` (Preocupación Empática + Incomodidad Personal). Includes 2×2 grid plots via `patchwork` for the subscale and composite models, main-effects vs. interaction versions.

### Complementary analyses

- **`correlaciones_bivariadas.R`** — Bivariate Spearman/Pearson correlations between task-derived variables (2K1B model parameters `p_2k1b_k_self` / `k_other` / `diff_k`; effort and reward slopes) and every relevant scale total/subdimension. Emits filtered tables of significant/trend correlations plus barplots. Also produces a heatmap over a curated set of variables (slopes, IRI/MAIA totals, IRI cognitive/affective composites, NASA, MAIA subscales) with rho and significance stars inline.

- **`Normalidad.R`** — Shapiro–Wilk and Kolmogorov–Smirnov tests on selected variables grouped by instrument (MAIA, IRI, SWBS, SASS, Effort, NASA). For each instrument, writes a PNG of faceted histograms overlaid with the theoretical Normal curve and, in the same style, Q-Q plots. Filenames: `histogramas_<INSTRUMENT>.png`.

- **`robust.R`** — Robust LMM on the computational-model rate parameters. For each family (parabolic `p_2k1b_*` and hyperbolic `h_2k1b_*`) and for both the old and new parameter sets, fits: robust LMM (`robustlmm::rlmer`), robust LM without random effects (`MASS::rlm`), standard `lmer` (repeated-measures ANOVA via `lmerTest`), and non-parametric ANOVA (`nparLD::f1.ld.f1`). Model form: `k ~ Agent * grupo + (1|subject_id)`. Uses a helper `pvals_from_t()` to attach p-values from t-statistics.

- **`analisis_garcia.R`** — Network analysis of the residualized data. Residualizes selected MAIA subscales (and, optionally, `diff_effort`) on covariates (`diff_reward`, `AIM_num`, `diff_success`, `Fatigue_pre_7`, `ASSIST_DIRt`, `IFS_Total_DIRd`, `DASS21_depresion_DIRd`) within group. Estimates EBICglasso networks per group with `qgraph` / `bootnet`, plots them side by side, and runs `NetworkComparisonTest::NCT` to compare global structure, global strength, and centrality (strength, betweenness) between Control and Vulnerable with FDR adjustment.

### Data pipeline

- **`fatigue_nasa.R`** — Extracts Fatigue (pre / post / diff) and NASA-TLX effort ratings (easy, hard, diff) from `datos_limpios.csv`, joins with the subject/group reference from `dataset_full_v2.csv`, writes `fatigue_nasa.csv`, and then merges those variables back into `dataset_full_final.csv`. Second half runs the same bivariate-correlation machinery as `correlaciones_bivariadas.R` but with NASA variables as the DVs, and produces a NASA easy-vs-hard barplot by group with t-tests and significance brackets.

- **`Edad.R`** — Merges `Sexo` and `Edad` from `Participantes_Final_ID.csv` into the main dataset (`dataset_full_v2.csv`) and reports demographic descriptives (age mean/SD, sex counts).

### Output folders

- **`Figuras/`** — Created by `GLMM_Corregido.R`. Contains the three post-hoc figures: `posthoc_slopes.png`, `posthoc_contrastes.png`, `posthoc_curvas.png`.

## How to run

Suggested execution order when rebuilding results from scratch:

1. **Upstream cleaning** (in `dgcerpa/PET_Data_Cleaning`) produces `datos_long_glmm_seba.csv`, `datos_limpios.csv`, and the subject-level questionnaire file.
2. **Data pipeline** — run in this order:
   1. `GLMM_Corregido.R` — fits the main GLMMs, extracts per-subject slopes, and writes `dataset_full_v2.csv`.
   2. `Edad.R` — appends `Sexo` and `Edad`.
   3. `fatigue_nasa.R` — appends Fatigue and NASA variables, producing `dataset_full_final.csv` (the file every downstream script reads).
3. **Assumption checks** — `Normalidad.R`.
4. **Descriptive / bivariate** — `correlaciones_bivariadas.R`.
5. **Instrument regressions** — `regresiones_MAIA.R`, `regresiones_IFS.R`, `regresiones_IRI.R` (any order).
6. **GLMM with covariates** — `GLMM_DASS.R`.
7. **Mediation / GGM** — `modelos_unificados.R`.
8. **Robust models** — `robust.R`.
9. **Network analysis** — `analisis_garcia.R`.

Open the project in RStudio (double-click the `.Rproj` file, or `open *.Rproj` from a shell) so working directory and file paths resolve correctly.

## Dependencies

R packages used across the scripts:

- Data / plotting: `tidyverse`, `dplyr`, `ggplot2`, `readxl`, `patchwork`, `ggforce`, `ggcorrplot`, `corrplot`
- Mixed models: `lme4`, `lmerTest`, `lmtest`
- Model tooling: `car`, `broom`, `performance`, `ggeffects`, `emmeans`
- Correlations / SEM: `Hmisc`, `ppcor`, `lavaan`
- Robust / non-parametric: `robustlmm`, `sfsmisc`, `MASS`, `nparLD`
- Networks: `mgm`, `huge`, `bootnet`, `NetworkComparisonTest`, `corpcor`, `psychonetrics`, `qgraph`, `psych`, `skimr`
- Namespace management: `conflicted`
