"""
Figuras 2 y 3 - Traducción de figuras_script.R a Python
Diego Garrido Cerpa - 2026

Dependencias:
    pandas numpy matplotlib seaborn statsmodels pymer4
    (pymer4 requiere R + paquete lme4 instalado en el sistema)

Alternativa sin pymer4: ver bloque `# ALTERNATIVA` abajo (exportar predicciones desde R).
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.patches import Patch
import seaborn as sns
import statsmodels.api as sm
import statsmodels.formula.api as smf

# ---------------------------------------------------------------------------
# 0. Estética global consistente entre Figura 2 y Figura 3
# ---------------------------------------------------------------------------

# Paleta: coral para Control, turquesa para Vulnerable.
# Elegidos con contraste alto en escala de grises y para daltonismo (deuteranopia).
COL_CONTROL    = "#E76F51"   # coral / salmón
COL_VULNERABLE = "#2A9D8F"   # turquesa
GROUP_PALETTE  = {"Control": COL_CONTROL, "Vulnerable": COL_VULNERABLE}

# Paleta secundaria para Self vs Other en el panel A de Figura 2
#COL_SELF  = "#264653"        # azul oscuro desaturado
#COL_OTHER = "#E9C46A"        # amarillo mostaza
COL_SELF  = "#8AA624" # turquesa
COL_OTHER = "#6A4C93"   # coral / salmón
AGENT_PALETTE = {"Self": COL_SELF, "Other": COL_OTHER}


# rcParams — fondo blanco, grid gris claro, base font 12, sans-serif consistente.
mpl.rcParams.update({
    "font.family":       "DejaVu Sans",   # cambiar a "Arial" si está disponible
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

PATH_LONG = "datos_long_glmm_filtrados.csv"   # trial-level, ya z-scored
PATH_IND  = "post_hoc_v2.csv"                 # BLUPs por sujeto (indv_dataset)
PATH_FULL = "dataset_full_final.csv"          # dataset completo por sujeto

df_long = pd.read_csv(PATH_LONG)
# Excluir omisiones y limpiar índice
df_long = df_long.query("decision != 2").copy()
if "Unnamed: 0" in df_long.columns:
    df_long = df_long.drop(columns="Unnamed: 0")

df_full = pd.read_csv(PATH_FULL)
df_ind  = pd.read_csv(PATH_IND)


# ===========================================================================
# FIGURA 2 -- Panel A: curvas predichas del GLMM binomial
# ===========================================================================
#
# Modelo (idéntico al R original):
#   decision ~ c.reward*agent*grupo + c.effort*agent*grupo
#              + (1 + c.effort + c.reward | sub)
#   family = binomial(logit)
#
# Se ajusta con pymer4 (wrapper de lme4). Ver notas al final para diferencias
# funcionales vs R puro.
# Leer predicciones exportadas desde R (ggpredict sobre m4 en RStudio)
grid = pd.read_csv("pred_eff.csv").rename(columns={
    "x": "c.effort",
    "predicted": "pred_prob",
    "conf.low": "lo",
    "conf.high": "hi",
    "group": "agent",
    "facet": "grupo",
})
grid["Agent"] = grid["agent"].astype(int).map({0: "Self", 1: "Other"})
grid["Group"] = grid["grupo"].astype(int).map({0: "Control", 1: "Vulnerable"})

# ===========================================================================
# FIGURA 2 -- Panel B: raincloud plot de diff_effort por grupo
# ===========================================================================

# Traer grupo al dataset individual (indv_dataset no lo trae)
df_ind = df_ind.merge(
    df_full[["sub", "grupo"]], on="sub", how="left"
)
df_ind["Group"] = df_ind["grupo"].map({0: "Control", 1: "Vulnerable"})


def half_violin(ax, y, x_center, color, width=0.35, side="right", nbins=200):
    """Half-violin construida con KDE de scipy. side = 'right' | 'left'."""
    from scipy.stats import gaussian_kde
    y = np.asarray(y)
    y = y[~np.isnan(y)]
    kde = gaussian_kde(y)
    y_grid = np.linspace(y.min() - 0.05, y.max() + 0.05, nbins)
    dens = kde(y_grid)
    dens = dens / dens.max() * width  # normalizar
    if side == "right":
        xs = x_center + dens
        ax.fill_betweenx(y_grid, x_center, xs, color=color, alpha=0.45,
                         linewidth=0)
        ax.plot(xs, y_grid, color=color, linewidth=0.8)
    else:
        xs = x_center - dens
        ax.fill_betweenx(y_grid, xs, x_center, color=color, alpha=0.45,
                         linewidth=0)
        ax.plot(xs, y_grid, color=color, linewidth=0.8)


# ---------------------------------------------------------------------------
# Construcción de la Figura 2 (dos paneles, ratio 1.6:1)
# ---------------------------------------------------------------------------

fig2 = plt.figure(figsize=(9.5, 4.2))
gs = fig2.add_gridspec(1, 2, width_ratios=[1.6, 1], wspace=0.25)

# --- Panel A: dos facets Control | Vulnerable dentro de un subgridspec -----
gs_a = gs[0, 0].subgridspec(1, 2, wspace=0.08)
ax_a0 = fig2.add_subplot(gs_a[0, 0])
ax_a1 = fig2.add_subplot(gs_a[0, 1], sharey=ax_a0)

for ax, group_label in zip((ax_a0, ax_a1), ("Control", "Vulnerable")):
    sub = grid[grid["Group"] == group_label]
    for agent_label in ("Self", "Other"):
        s = sub[sub["Agent"] == agent_label].sort_values("c.effort")
        color = AGENT_PALETTE[agent_label]
        ax.fill_between(s["c.effort"], s["lo"], s["hi"],
                        color=color, alpha=0.20, linewidth=0)
        ax.plot(s["c.effort"], s["pred_prob"],
                color=color, linewidth=1.8, label=agent_label)
    ax.set_title(group_label, fontsize=11, fontweight="normal")
    ax.set_xlabel("")
    # Mapear ticks de la escala z-score a los niveles originales de esfuerzo (2, 3, 4).
    # Como c.effort se centró y escaló, necesitamos los valores z de esos niveles.
    # Reemplazá EFFORT_MEAN y EFFORT_SD por los valores reales de tu dataset.
    EFFORT_MEAN = 3.0     # media original de effort
    EFFORT_SD   = 1.0     # SD original de effort
    tick_levels = [2, 3, 4]
    tick_z = [(lvl - EFFORT_MEAN) / EFFORT_SD for lvl in tick_levels]
    ax.set_xticks(tick_z)
    ax.set_xticklabels([str(l) for l in tick_levels])
    ax.set_ylim(0.7, 1.0)
    ax.set_yticks([0.7, 0.8, 0.9, 1.0])
    ax.set_yticklabels(["70%", "80%", "90%", "100%"])

ax_a0.set_ylabel("P(accept work offer)")
plt.setp(ax_a1.get_yticklabels(), visible=False)

# Título global del panel A, alineado sobre los dos facets
ax_a0.text(1.04, 1.12, "Predicted probabilities of decision",
           transform=ax_a0.transAxes, ha="center", va="bottom",
           fontsize=12, fontweight="bold")

# Leyenda de Agent bajo el panel A
handles_a = [plt.Line2D([0], [0], color=AGENT_PALETTE[a], linewidth=2, label=a)
             for a in ("Self", "Other")]
fig2.legend(handles=handles_a, loc="lower center",
            bbox_to_anchor=(0.32, -0.02), ncol=2, frameon=False)

# --- Panel B: raincloud -----------------------------------------------------
ax_b = fig2.add_subplot(gs[0, 1])
positions = {"Control": 0, "Vulnerable": 1}

for group, x_center in positions.items():
    ydata = df_ind.loc[df_ind["Group"] == group, "diff_effort"].dropna().values
    color = GROUP_PALETTE[group]
    # half-violin a la derecha
    half_violin(ax_b, ydata, x_center + 0.12, color, width=0.30, side="right")
    # boxplot fino a la izquierda-centro
    bp = ax_b.boxplot(
        ydata, positions=[x_center - 0.05], widths=0.12,
        patch_artist=True, showfliers=False,
        medianprops=dict(color="white", linewidth=1.5),
        boxprops=dict(facecolor=color, alpha=0.8, linewidth=0.6),
        whiskerprops=dict(color=color, linewidth=0.8),
        capprops=dict(color=color, linewidth=0.8),
    )
    # jitter de puntos individuales
    rng = np.random.default_rng(42 + x_center)
    jitter = rng.uniform(-0.05, 0.05, size=len(ydata))
    ax_b.scatter(np.full_like(ydata, x_center - 0.22) + jitter, ydata,
                 s=14, color=color, edgecolor="white",
                 linewidth=0.5, alpha=0.75)

ax_b.axhline(0, linestyle="--", linewidth=0.8, color="grey")
ax_b.set_xticks([0, 1])
ax_b.set_xticklabels(["Control", "Vulnerable"])
ax_b.set_xlim(-0.55, 1.55)
ax_b.set_ylabel("Effort Difference (Other - Self)")
ax_b.set_xlabel("")

# Tags de panel A, B en negrita arriba a la izquierda
for ax, tag in [(ax_a0, "A"), (ax_b, "B")]:
    ax.text(-0.15, 1.08, tag, transform=ax.transAxes,
            fontsize=14, fontweight="bold", va="bottom", ha="left")

fig2.savefig("figure2.png", dpi=600, bbox_inches="tight")
plt.close(fig2)


# ===========================================================================
# FIGURA 3 -- Tres OLS: diff_effort ~ scale * grupo
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
    # OLS con interacción con grupo
    model = smf.ols(f"diff_effort ~ {var} * grupo", data=df_mod).fit()

    # Predicciones para cada grupo por separado
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
    # Grid termina donde terminan los datos
    ax.set_xlim(x_range.min(), x_range.max())
    ax.margins(x=0, y=0)
    # Tag en negrita
    ax.text(-0.10, 1.06, tag, transform=ax.transAxes,
            fontsize=14, fontweight="bold", va="bottom", ha="left")

axes3[0].set_ylabel("Effort Difference")

# Leyenda de grupo colectada abajo del conjunto
handles_g = [plt.Line2D([0], [0], color=GROUP_PALETTE[g], linewidth=2, label=g)
             for g in ("Control", "Vulnerable")]
fig3.legend(handles=handles_g, loc="lower center",
            bbox_to_anchor=(0.54, -0.06), ncol=2, frameon=False)

fig3.tight_layout()
fig3.savefig("figure3.png", dpi=600, bbox_inches="tight")
plt.close(fig3)

print("Listo: figure2.png y figure3.png generados.")


# ===========================================================================
# ALTERNATIVA sin pymer4 para el Panel A de Figura 2
# ===========================================================================
# Si preferís evitar la dependencia de R en Python, exportá desde R:
#
#   pred_eff <- ggpredict(m4, terms = c("c.effort [all]", "agent", "grupo"))
#   write.csv(as.data.frame(pred_eff), "pred_eff.csv", row.names = FALSE)
#
# Luego en Python reemplazá el bloque de pymer4 por:
#
#   grid = pd.read_csv("pred_eff.csv").rename(columns={
#       "x": "c.effort", "predicted": "pred_prob",
#       "conf.low": "lo", "conf.high": "hi",
#       "group": "agent", "facet": "grupo",
#   })
#   grid["Agent"] = grid["agent"].map({0: "Self", 1: "Other"})
#   grid["Group"] = grid["grupo"].map({0: "Control", 1: "Vulnerable"})
#
# Esto te garantiza reproducibilidad exacta con el ggplot original.
