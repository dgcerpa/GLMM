"""
Figuras 2 y 3 - Traducción de figuras_script.R a Python
Diego Garrido Cerpa - 2026

CAMBIO 2026-08 (revisión Sebastián):
- Figura 2 se rehace desde DATOS REALES (proporción empírica de aceptar por
  celda sub × grupo × agent × effort_level), no desde predicciones del GLMM.
- Se elimina el panel B (raincloud de diff_effort).
- La figura queda como un único panel con dos facets Control | Vulnerable.

Dependencias:
    pandas numpy matplotlib seaborn statsmodels

Figure 2. Observed probability of accepting the work offer by group and beneficiary. 
Model-free, trial-level acceptance rates aggregated per participant, 
then averaged across participants within each combination of group 
(Control, Vulnerable), beneficiary (Self, Other), and effort level (1–4). 
Error bars are ±1.96 SEM across participants 
(approximate 95% confidence intervals). 
In controls, acceptance on Other trials decreased with effort, 
reproducing the classical prosocial-apathy pattern. 
In the vulnerable group, this effort-related decline in acceptance was absent.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
import statsmodels.api as sm
import statsmodels.formula.api as smf

# ---------------------------------------------------------------------------
# 0. Estética global consistente entre Figura 2 y Figura 3
# ---------------------------------------------------------------------------

# Paleta: coral para Control, turquesa para Vulnerable.
COL_CONTROL    = "#E76F51"
COL_VULNERABLE = "#2A9D8F"
GROUP_PALETTE  = {"Control": COL_CONTROL, "Vulnerable": COL_VULNERABLE}

# Paleta secundaria para Self vs Other en el panel A de Figura 2
COL_SELF  = "#8AA624"
COL_OTHER = "#6A4C93"
AGENT_PALETTE = {"Self": COL_SELF, "Other": COL_OTHER}


mpl.rcParams.update({
    "font.family":       "DejaVu Sans",
    "font.size":         12,
    "axes.titlesize":    12,
    "axes.titleweight":  "bold",
    "axes.labelsize":    11,
    "axes.linewidth":    0.8,
    "axes.edgecolor":    "#333333",
    "axes.facecolor":    "white",
    "figure.facecolor":  "white",
    "axes.grid":         True,
    "grid.color":        "#e5e5e5",
    "grid.linewidth":    0.6,
    "axes.spines.top":   False,
    "axes.spines.right": False,
    "legend.frameon":    False,
    "legend.fontsize":   10,
    "xtick.labelsize":   10,
    "ytick.labelsize":   10,
})


# ---------------------------------------------------------------------------
# 1. Carga de datos
# ---------------------------------------------------------------------------

PATH_LONG = "data_glmm_filtered.csv"   # trial-level, ya z-scored
PATH_FULL = "dataset_final.csv"          # dataset completo por sujeto

df_long = pd.read_csv(PATH_LONG)
# Excluir omisiones y limpiar índice
df_long = df_long.query("decision != 2").copy()
if "Unnamed: 0" in df_long.columns:
    df_long = df_long.drop(columns="Unnamed: 0")

df_full = pd.read_csv(PATH_FULL)


# ===========================================================================
# FIGURA 2 -- Panel único: proporción empírica de aceptar por nivel de esfuerzo
# ===========================================================================
#
# Agregación:
#   1. Proporción de aceptar por sujeto × grupo × agente × nivel de esfuerzo.
#   2. Promedio y SEM a través de sujetos dentro de cada celda (grupo × agente
#      × nivel).
#
# c.effort viene z-scored con 4 valores únicos que corresponden 1:1 a los 4
# niveles crudos de la tarea; los mapeamos de vuelta a 1..4 para el eje X.

effort_map = {v: i + 1 for i, v in enumerate(sorted(df_long["c.effort"].unique()))}
df_long["effort_level"] = df_long["c.effort"].map(effort_map)

# Paso 1: proporción de aceptar por sujeto en cada celda
by_subject = (
    df_long
    .groupby(["sub", "grupo", "agent", "effort_level"], as_index=False)["decision"]
    .mean()
    .rename(columns={"decision": "p_accept"})
)

# Paso 2: media y SEM a través de sujetos por celda
by_cell = (
    by_subject
    .groupby(["grupo", "agent", "effort_level"], as_index=False)["p_accept"]
    .agg(mean="mean", std="std", n="count")
)
by_cell["sem"] = by_cell["std"] / np.sqrt(by_cell["n"])
# Barras de error como IC 95% aproximado
by_cell["lo"] = by_cell["mean"] - 1.96 * by_cell["sem"]
by_cell["hi"] = by_cell["mean"] + 1.96 * by_cell["sem"]

by_cell["Agent"] = by_cell["agent"].map({0: "Self", 1: "Other"})
by_cell["Group"] = by_cell["grupo"].map({0: "Control", 1: "Vulnerable"})

# ---------------------------------------------------------------------------
# Construcción de la Figura 2 (un único panel con dos facets)
# ---------------------------------------------------------------------------

fig2, (ax_c, ax_v) = plt.subplots(
    1, 2, sharey=True, figsize=(6.5, 3.8),
    gridspec_kw={"wspace": 0.08},
)

for ax, group_label in zip((ax_c, ax_v), ("Control", "Vulnerable")):
    sub = by_cell[by_cell["Group"] == group_label]
    for agent_label in ("Self", "Other"):
        s = sub[sub["Agent"] == agent_label].sort_values("effort_level")
        color = AGENT_PALETTE[agent_label]
        # línea fina entre niveles
        ax.plot(s["effort_level"], s["mean"],
                color=color, linewidth=1.4, alpha=0.9, zorder=2)
        # barras de error ± 1.96 SEM
        ax.errorbar(s["effort_level"], s["mean"],
                    yerr=1.96 * s["sem"],
                    fmt="o", color=color, ecolor=color,
                    markersize=6, markeredgecolor="white",
                    markeredgewidth=0.8,
                    elinewidth=1.2, capsize=3, capthick=1.0,
                    label=agent_label, zorder=3)
    ax.set_title(group_label, fontsize=11, fontweight="normal")
    ax.set_xlabel("")
    ax.set_xticks([1, 2, 3, 4])
    ax.set_xlim(0.6, 4.4)
    ax.set_ylim(0.5, 1.02)
    ax.set_yticks([0.5, 0.6, 0.7, 0.8, 0.9, 1.0])
    ax.set_yticklabels(["50%", "60%", "70%", "80%", "90%", "100%"])

ax_c.set_ylabel("P(accept work offer)")
plt.setp(ax_v.get_yticklabels(), visible=False)

# Título del panel, alineado sobre los dos facets
ax_c.text(1.04, 1.10, "Observed probability of accepting the work offer",
          transform=ax_c.transAxes, ha="center", va="bottom",
          fontsize=12, fontweight="bold")

# Etiqueta X compartida, centrada bajo los dos facets
fig2.supxlabel("Effort level", fontsize=11, y=-0.01)

# Leyenda de Agent abajo (por debajo del supxlabel)
handles_a = [plt.Line2D([0], [0], marker="o", color=AGENT_PALETTE[a],
                        linewidth=1.4, markersize=6,
                        markeredgecolor="white", markeredgewidth=0.8,
                        label=a)
             for a in ("Self", "Other")]
fig2.legend(handles=handles_a, loc="lower center",
            bbox_to_anchor=(0.5, -0.10), ncol=2, frameon=False)

fig2.savefig("figure2.png", dpi=600, bbox_inches="tight")
plt.close(fig2)


# ===========================================================================
# FIGURA 3 -- Tres OLS: diff_effort ~ scale * grupo  (SIN CAMBIOS)
# ===========================================================================

df_mod = df_full[[
    "grupo", "diff_effort",
    "IRI_PreocupacionEmpatica_DIRd", "MAIA_DIRt", "SASS_DIRt"
]].apply(pd.to_numeric, errors="coerce").dropna()

specs = [
    dict(
        var="IRI_PreocupacionEmpatica_DIRd",
        xlabel="IRI",
        title="Empathic Concern by Group",
        xticks=[15, 20, 25],
    ),
    dict(
        var="MAIA_DIRt",
        xlabel="MAIA",
        title="Interoceptive Awareness by Group",
        xticks=[75, 100, 125],
    ),
    dict(
        var="SASS_DIRt",
        xlabel="SASS",
        title="Social Adaptation by Group",
        xticks=[35, 45, 55],
    ),
]

fig3, axes3 = plt.subplots(1, 3, figsize=(10.5, 3.6), sharey=True)
for ax, spec, tag in zip(axes3, specs, ("A", "B", "C")):
    var = spec["var"]
    model = smf.ols(f"diff_effort ~ {var} * grupo", data=df_mod).fit()

    x_range = np.linspace(df_mod[var].min(), df_mod[var].max(), 100)
    for g_num, g_label in [(0, "Control"), (1, "Vulnerable")]:
        newd = pd.DataFrame({var: x_range, "grupo": g_num})
        pred = model.get_prediction(newd).summary_frame(alpha=0.05)
        color = GROUP_PALETTE[g_label]
        ax.fill_between(x_range,
                        pred["mean_ci_lower"], pred["mean_ci_upper"],
                        color=color, alpha=0.20, linewidth=0)
        ax.plot(x_range, pred["mean"],
                color=color, linewidth=1.8, label=g_label)

    ax.set_title(spec["title"], fontsize=11)
    ax.set_xlabel(spec["xlabel"])
    ax.set_xticks(spec["xticks"])
    ax.set_ylim(-0.6, 0.6)
    ax.set_yticks(np.arange(-0.6, 0.61, 0.2))
    ax.set_xlim(x_range.min(), x_range.max())
    ax.margins(x=0, y=0)
    ax.text(-0.10, 1.06, tag, transform=ax.transAxes,
            fontsize=14, fontweight="bold", va="bottom", ha="left")

axes3[0].set_ylabel("Effort Difference")

handles_g = [plt.Line2D([0], [0], color=GROUP_PALETTE[g], linewidth=2, label=g)
             for g in ("Control", "Vulnerable")]
fig3.legend(handles=handles_g, loc="lower center",
            bbox_to_anchor=(0.54, -0.06), ncol=2, frameon=False)

fig3.tight_layout()
fig3.savefig("figure3.png", dpi=600, bbox_inches="tight")
plt.close(fig3)

print("Listo: figure2.png y figure3.png generados.")
