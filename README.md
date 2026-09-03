# trcpetc

`trcpetc` is an R package for common clinical and epidemiological data-analysis
tasks. It provides tools for:

- creating descriptive baseline tables (Table 1), including overall and
  grouped summaries;
- preparing and summarizing Kaplan-Meier and competing-risks analyses; and
- formatting outputs for reports, manuscripts, and presentations.

The package includes the example dataset `cardio_data` and its data dictionary
`cardio_data_dictionary`.

The package was developed at University Health Network (UHN) by Jessica Weiss,
Rong Huang, and Brigitte Mueller. It builds on work from `fanetc`, originally
written by Chun-Po Steve Fan at UHN:
<https://github.com/fanstev1/fanetc>.

The package currently declares the MIT License. This licensing choice is
pending confirmation with UHN Digital.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("TRCHR-CP/trcpetc", build_vignettes = TRUE)
```

Then load the package:

```r
library(trcpetc)
```

## Descriptive tables

Use `table_one()` to summarize numeric, logical, and factor variables. When a
grouping variable is supplied, standardized mean differences (SMDs) are
reported by default. Use `stat_test = "pval"` for p-values or
`stat_test = "none"` to omit between-group statistics. Use `kable_table_one()`
to turn the result into a report-ready table.

```r
library(dplyr)

table <- cardio_data %>%
  mutate(SurgeryType = factor_order(SurgeryType)) %>%
  table_one(
    group = Sex,
    datadic = cardio_data_dictionary,
    var_name = "VariableName",
    var_desp = "Label",
    include_overall = "all"
  )

kable_table_one(table, caption = "Baseline characteristics by sex")
```

### SMD calculation

The grouped `table_one()` output reports unweighted standardized mean
differences by default. For continuous variables, the SMD is the absolute
difference in group means divided by the square root of the average within-
group variances. For logical variables, the group mean is the proportion of
`TRUE` values and the Bernoulli variance $p(1-p)$ is used.

For factor variables, `table_one()` uses the Mahalanobis-distance approach of
Yang and Dalton (2012). It constructs each group's complete vector of category
proportions and the corresponding full $k x k$ multinomial covariance matrix.
Because this covariance matrix is singular, its Moore-Penrose generalized
inverse is calculated with `MASS::ginv()`. With more than two groups, the
reported SMD is the arithmetic mean of the pairwise distances. This matches
the unweighted factor SMD produced by `tableone::CreateTableOne()` and
`tableone::ExtractSmd()` for the same data.

SMDs are effect-size measures and do not depend on hypothesis-test p-values.
Use `stat_test = "pval"` when p-values are preferred, or
`stat_test = "none"` to omit between-group statistics.

Useful preparation functions include:

- `factor_order()` orders factor levels by frequency;
- `check_box_convert()` converts checkbox-style columns to logical variables;
- `format_pvalue()` formats p-values for display.

When a logical variable has no observed values in a group, its summary is
shown as `0 (-)` rather than `0 (NaN%)`. Observed zero values are still shown
as `0 (0%)`.

When a table object is needed for use in R Markdown or Quarto, first request
the data-frame output and then apply `kable_table_time_to_event()`. Its
`caption` is passed to `kableExtra`, allowing the document format to provide
automatic table numbering.

```r
km_table <- summarize_km(km, kable_output = FALSE)
kable_table_time_to_event(km_table, caption = "Overall survival")
```

## Survival and competing risks

Create analysis-ready time and event variables with
`construct_surv_cmprisk_var()`. Without competing event dates it prepares
survival data; with competing event dates it prepares competing-risks data.
`estimate_cif_km()` then estimates either a Kaplan-Meier curve or cumulative
incidence functions based on the event variable supplied.

The time axis in `show_surv()` and `show_cif()` preserves decimal values, so
year-scale analyses do not need to be converted to months just to avoid
rounded axis labels.

```r
survival_data <- construct_surv_cmprisk_var(
  cardio_data,
  patid = PatientID,
  idx_dt = SurgeryDate,
  evt_dt = DeathDate,
  end_dt = LastVisitDate,
  units = "months",
  adm_cnr_time = 24
)

km <- estimate_cif_km(survival_data, evt = evt, evt_time = evt_time,
                      group = Sex)

summarize_km(km, times = seq(0, 24, by = 3),
             time_lab = "Time since surgery (months)")
show_surv(km, x_lab = "Time since surgery (months)")
```

For competing risks, pass competing event dates through `...`:

```r
competing_risk_data <- construct_surv_cmprisk_var(
  cardio_data,
  patid = PatientID,
  idx_dt = SurgeryDate,
  evt_dt = TransplantDate,
  end_dt = LastVisitDate,
  death_dt = DeathDate,
  units = "months"
)

cif <- estimate_cif_km(competing_risk_data, evt = evt, evt_time = evt_time)
summarize_cif(cif, times = seq(0, 24, by = 3))
show_cif(cif, evt_type = 1)
```

## Function reference

The generated help pages are the authoritative reference for arguments and
return values. The exported functions are grouped below by purpose.

### Descriptive tables and formatting

| Function | Purpose |
| --- | --- |
| `table_one()` | Create descriptive summaries overall or by group. |
| `kable_table_one()` | Format a `table_one()` result as a report-ready table. |
| `check_box_convert()` | Prepare checkbox-style variables for descriptive tables. |
| `factor_order()` | Order factor levels by descending frequency. |
| `format_pvalue()` | Format p-values for presentation. |

### Survival and competing risks

| Function | Purpose |
| --- | --- |
| `construct_surv_cmprisk_var()` | Create time and event variables for survival or competing-risks analyses. |
| `estimate_cif_km()` | Estimate Kaplan-Meier curves or cumulative incidence functions. |
| `summarize_km()` | Create tabular Kaplan-Meier summaries. |
| `summarize_cif()` | Create tabular cumulative-incidence summaries. |
| `kable_table_time_to_event()` | Format a Kaplan-Meier or CIF summary as a report-ready table. |
| `show_surv()` | Plot Kaplan-Meier curves with numbers at risk. |
| `show_cif()` | Plot cumulative-incidence curves with numbers at risk. |
| `median_time_to_event()` | Estimate the time to a specified event probability. |

### Regression and multiple imputation

The model-summary functions report the factor-level omnibus p-value by default.
Use `pval = "both"` to retain that omnibus p-value and add the individual
coefficient p-values comparing each level with the reference level.

| Function | Purpose |
| --- | --- |
| `summarize_coxph()` | Summarize a Cox proportional-hazards model with omnibus p-values by default. |
| `summarize_mi_coxph()` | Summarize multiply imputed Cox models with omnibus p-values by default. |
| `summarize_mi_glm()` | Summarize multiply imputed GLMs with omnibus p-values by default. |
| `generate_mi_glm_termplot_df()` | Prepare multiply imputed GLM results for term plots. |
| `calculate_type3_mi()` | Calculate Type III tests across multiply imputed models. |

### Utilities

| Function | Purpose |
| --- | --- |
| `updateWorksheet()` | Update an Excel worksheet with package output. |

## Vignette

The introductory vignette contains longer examples and details about the
descriptive-table and time-to-event workflows:

```r
vignette("trcpetc-intro", package = "trcpetc")
```

The source is available at
[`vignettes/trcpetc-intro.Rmd`](vignettes/trcpetc-intro.Rmd).

## Development

To run the test suite from a local clone:

```r
install.packages(c("devtools", "testthat"))
devtools::test()
```

## References

Yang, D. and Dalton, J. E. (2012). A unified approach to measuring the effect
size between two groups using SAS. *SAS Global Forum*, 335, 1-6.

Li, L. and Greene, T. (2013). A weighting analogue to pair matching in
propensity score analysis. *International Journal of Biostatistics*, 9(2),
215-234.

Austin, P. C. and Stuart, E. A. (2015). Moving towards best practice when
using inverse probability of treatment weighting (IPTW) using the propensity
score to estimate causal treatment effects in observational studies.
*Statistics in Medicine*, 34(28), 3661-3679.