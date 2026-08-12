# GLMM

Statistical analysis of the prosocial effort discounting task: generalized linear mixed models (GLMMs) on trial-by-trial decisions and task success, extraction of individual effort/reward slopes, regressions of those slopes against psychological scales, an empirical (model-free) replication of the key regressions, mediation analysis, and publication figures.

## Context

Data-analysis repository for the prosocial effort task comparing a **Control** group against a **Vulnerable (Experimental)** group. The primary dependent variable is `diff_effort` — the difference (Other − Self) in effort slopes extracted from a decision-level GLMM. Trial-level cleaning happens upstream (in `dgcerpa/PET_Data_Cleaning`); this repository takes the resulting exports and runs everything downstream. Diego Garrido (analysis), Sebastián Contreras (supervisor), with contributions from Nicolás and José. Private — internal documentation supporting a manuscript in preparation, not for public distribution.

## Data

Two versioned datasets in the repository root feed the analyses:

- **`data_glmm_filtered.csv`** — trial-level (long format). One row per retained trial, with the decision (`decision`), success (`success`), agent (`agent`: 0 = Self, 1 = Other), group (`grupo`: 0 = Control, 1 = Vulnerable), and continuous predictors z-scored under a `c.*` prefix (`c.reward`, `c.effort`, …). Produced by `Limpieza_Datos.R`.
- **`dataset_final.csv`** — subject-level. One row per participant, holding questionnaire totals and subscales (MAIA, IRI, IFS, SASS, …), the GLMM-derived per-subject slopes (`reward_self`, `effort_self`, `reward_other`, `effort_other`, `diff_effort`, `diff_reward`), and demographics (`Edad`, `Sexo`). This is the file every subject-level script reads.

## Scripts

All analysis scripts live in the repository root and are grouped below by role.

### Data preparation

- **`Limpieza_Datos.R`** — Entry point of the pipeline. Reads the raw long-format trial export, drops subjects with more than 25% omissions on either Self or Other trials, coerces variable types, z-scores every continuous predictor (`c.*` prefix), removes omitted trials (`decision == 2`), and writes the filtered trial-level dataset (`data_glmm_filtered.csv`). It then fits per-agent random-slope GLMMs (`lmself_rs.sc`, `lmother_rs.sc`; `decision ~ c.reward + c.effort + (1 + c.reward + c.effort | sub)`), extracts each subject's `reward`/`effort` slopes, computes the Other − Self differences (`diff_effort`, `diff_reward`), and writes the per-subject slopes to `post_hoc_v2.csv`.

### Main GLMM models

- **`GLMM_Decision.R`** — Decision-level GLMM. Fits four candidate models (`m1`–`m4`) of `decision` on the `reward × agent × effort × grupo` structure with varying random-effects specifications, compares them via `anova()`, and inspects the retained model with `car::Anova`, `performance::r2_nakagawa`, and singularity checks. Post-hoc effort slopes are contrasted by group within agent and by agent within group using `emtrends` (FDR-adjusted), and `ggpredict` renders the predicted-probability curves of accepting the offer as a function of effort, faceted by group.

- **`GLMM_Success.R`** — Success-level GLMM, restricted to accepted trials (`decision == 1`). Fits four candidate models (`m1`–`m4`) of `success` on the `reward × agent × effort × grupo` structure, compares and inspects them as above, and runs post-hoc contrasts with `emmeans` / `emtrends`. Produces a grouped bar plot of the predicted probability of success by group and agent with 95% confidence intervals.

### Instrument regressions

Each script builds a subject-level analytic subset from `dataset_final.csv` and fits linear models of `diff_effort` (and `effort_other`) on an instrument's total score and its subscales, both without and with a `grupo` interaction; group slopes are followed up with `emtrends` and interaction effects are visualized with `ggeffects::ggpredict`.

- **`regresiones_MAIA.R`** — MAIA total and its 8 subscales (Percibir, AusenciaDistracción, AusenciaPreocupación, RegulaciónAtención, ConcienciaEmocional, Autorregulación, EscuchaCuerpo, Confianza), with `Fatigue_diff` included as a covariate in the subscale models.
- **`regresiones_IFS.R`** — IFS total and its 8 subscales, plus SASS total models (`diff_effort` and `effort_other`), including the `SASS × grupo` interaction with type-II tests and a mean-centered refit.
- **`regresiones_IRI.R`** — IRI total, its 4 subscales, and the derived composites `IRI_Cognitivo` (Fantasía + Toma de Perspectiva) and `IRI_Afectivo` (Preocupación Empática + Incomodidad Personal), including standalone Empathic-Concern and Perspective-Taking × group models.
- **`regresiones_empiricas.R`** — Model-free replication of the headline regressions. Recomputes effort sensitivity directly from the trial data (`data_glmm_filtered.csv`) as the per-subject OLS slope of `decision` on the raw effort level within each agent, forming `diff_effort_emp = slope_other − slope_self`. It then refits the three regressions reported in the manuscript (MAIA, SASS, and IRI Empathic Concern, each × group) on this empirical measure and saves the three-panel figure `figure3_empirical.png`.

### Mediation

- **`Modelo_mediacion.R`** — Mediation of the group effect. Fits per-group mediation models with `X = MAIA`, `M = diff_effort`, `Y = SASS`, controlling for `Fatigue_diff`, and estimates indirect effects via bootstrapping (`mediation::mediate`, 5000 resamples). Includes a moderated-mediation specification testing whether the paths differ between Control and Vulnerable, path diagrams (`DiagrammeR`), and the `X → M`, `M → Y`, `X → Y` scatterplots by group (`patchwork`). *Note:* the script currently points to an external working directory and slopes input; adapt the path and input file to this repository's layout before running.

### Demographics

- **`Edad.R`** — Reports demographic descriptives (age mean/SD and sex counts) per group from `dataset_final.csv`.

### Figures

- **`figures.py`** — Python (matplotlib / seaborn / statsmodels) generation of the manuscript figures.
  - **Figure 2** — Observed, model-free probability of accepting the work offer by group, beneficiary (Self/Other), and effort level (1–4), aggregated per participant and averaged across participants with ±1.96 SEM error bars, rendered as two facets (Control | Vulnerable). Saved as `figure2.png`.
  - **Figure 3** — Three OLS interaction panels (`diff_effort ~ scale × grupo`) for IRI Empathic Concern, MAIA, and SASS, with 95% confidence bands per group. Saved as `figure3.png`.

  Figure assets are collected under `Figuras/`.

## How to run

Open the project in RStudio (double-click `GLMM.Rproj`) so the working directory and relative paths resolve correctly. Suggested execution order when rebuilding results from scratch:

1. **`Limpieza_Datos.R`** — produces `data_glmm_filtered.csv` (trial-level) and `post_hoc_v2.csv` (per-subject slopes). The subject-level slopes are consolidated with questionnaire and demographic data into `dataset_final.csv`, which the downstream scripts consume.
2. **Trial-level models** — `GLMM_Decision.R`, then `GLMM_Success.R`.
3. **Instrument regressions** — `regresiones_MAIA.R`, `regresiones_IFS.R`, `regresiones_IRI.R` (any order), and `regresiones_empiricas.R` for the model-free replication.
4. **Mediation** — `Modelo_mediacion.R` (after adapting its paths).
5. **Demographics** — `Edad.R`.
6. **Figures** — `figures.py` (Figures 2 and 3); `figure3_empirical.png` is produced by `regresiones_empiricas.R`.

## Dependencies

**R packages**

- Data / plotting: `tidyverse`, `readxl`, `ggplot2`, `patchwork`, `ggcorrplot`, `corrplot`
- Mixed models: `lme4` (`lmerTest` for the mediation script's namespace preferences)
- Model tooling: `car`, `broom`, `performance`, `ggeffects`, `emmeans`
- Mediation / SEM: `mediation`, `lavaan`, `boot`, `corpcor`, `psych`, `skimr`, `MASS`, `purrr`, `tidyr`
- Diagrams: `DiagrammeR`, `semPlot`
- Namespace management: `conflicted`

**Python packages** (for `figures.py`)

- `pandas`, `numpy`, `matplotlib`, `seaborn`, `statsmodels`
