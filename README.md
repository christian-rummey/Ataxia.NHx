
# use .Rprofile in 
# C:\Users\ChristianRummey\OneDrive - CDS\Documents


# Ataxia.NHx

Common Analyses for Natural History Studies in Ataxias - Need to merge in R.FRDA.NH

## PSM Matching: Analyses that provide Context

**1) year-to-year change predictability.R**

-   computes adjacent 1-year annualized changes (prior year vs. subsequent year).

-   fits regressions to check if prior-year change predicts subsequent-year change.

-   stats (slope, R², correlation, p) + scatterplots with identity line and LM fit. -\> year-to-year change is noisy, slopes near zero → regression-to-the-mean dominates.

**2) year-to-year change predictability.R**

-   not cleaned

**3) year1 change classification.R**

-   computes mean change from baseline by study, visit, and group

-   plots trajectories stratified by year-1 classification: improved/stable/decline.

## Change from Baseline Analyses

**4) 5y-Change from Baseline by Age Group.R**

-   Fits MMRM (mixed model for repeated measures) with baseline as covariate

-   Models:
    - Per age group: `aval ~ bl + age_grp * factor(avisitn) + (1 | sjid)`
    - Overall "all": `aval ~ bl + factor(avisitn) + (1 | sjid)` (no group stratification)

-   Extracts estimated marginal means (emmeans) for each age group at each visit

-   Optional "all" group representing overall population estimate (toggle: `include_all_group`)

-   Plots discrete visit-based change from baseline with error bars

-   Outputs combined plots (patchwork) with fixed y-axis limits per parameter

-   Color palette: black for "all", distinct colors for age groups

**5) 5y-Change from Baseline by Age Group with Slopes.R**

-   Extends script #4 by adding slope model overlay

-   Slope models with random intercepts AND slopes:
    - Per age group: `aval ~ bl + age_grp * tm. + (1 + tm. | sjid)`
    - Overall "all": `aval ~ bl + tm. + (1 + tm. | sjid)` (no group stratification)

-   Uses continuous time (`tm.` in years) instead of discrete visits

-   Overlays smooth slope lines (semi-transparent) on top of discrete visit estimates

-   Shows both model-based trajectories (slopes) and visit-specific estimates (points + error bars)

-   Useful for visualizing overall trends vs. discrete measurement points

-   Settings controlled in "1. PREPARE DATA" section:
    - `include_all_group`: Toggle to include/exclude "all" group
    - `limits.mFARS`, `limits.USS`: Fixed y-axis limits per parameter
