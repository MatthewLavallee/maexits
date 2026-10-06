# maexitsv2: Medicare Advantage plan exits

An R package that measures how many Medicare Advantage (MA) enrollees lose their plan each year, and why. It builds its data from public CMS files: plan-by-county enrollment, the MA landscape, and the Part C&D Plan Crosswalk, which says what becomes of each plan the next January. Terminations and service-area reductions are kept apart from renewals and consolidations.

![Share of Medicare Advantage enrollees in plans that CMS terminated or cut from their county, by year: 3.0% in 2018–19, 0.4% in 2021–22, 7.0% in 2024–25, 9.9% in 2025–26 and 10.4% in 2026–27](man/figures/exits-by-year.png)

<details>
<summary>Figure data</summary>

Share of September MA enrollment (%) in plans CMS terminated or cut from the county the next January.

| Transition | Plan terminated | Service-area reduction | Terminated or SAR | Terminated or SAR, December enrollment |
|---|--:|--:|--:|--:|
| 2018–19 | 0.59 | 2.43 | 3.02 | 2.93 |
| 2019–20 | 0.76 | 0.59 | 1.35 | 1.28 |
| 2020–21 | 0.46 | 0.29 | 0.75 | 0.71 |
| 2021–22 | 0.25 | 0.18 | 0.43 | 0.42 |
| 2022–23 | 1.07 | 0.17 | 1.23 | 1.18 |
| 2023–24 | 1.18 | 0.22 | 1.40 | 1.35 |
| 2024–25 | 5.03 | 2.00 | 7.03 | 6.90 |
| 2025–26 | 7.46 | 2.44 | 9.90 | 9.62 |
| 2026–27 | 8.01 | 2.43 | 10.43 | not out yet |

Drawn from the `exits` table by [`tools/readme-figure.R`](tools/readme-figure.R).
</details>

## Install and load

```r
# install.packages("remotes")
remotes::install_github("MatthewLavallee/maexits")
library(maexitsv2)

maexits_catalog()                                  # every dataset and variable

# One row per December plan x county; downloads once, then cached
d <- maexits_data("displacement", years = 2025)    # December 2025 -> January 2026
d[, sum(dec_enrollment[lost_coverage]) / sum(dec_enrollment)]   # share who lost their plan
d[lost_coverage == TRUE, .(people = sum(dec_enrollment)), by = outcome]

# Share in plans CMS terminated or cut from the county, every year on September enrollment
e <- maexits_data("exits")
e[, .(share = sum(sep_enrollment[exit_type != "none"]) / sum(sep_enrollment)), keyby = dec_year]

maexits_data("county_panel", years = 2024:2025, states = "MD")
maexits_data("plan_details", plans = "H2001", years = 2026)
cms_enrollment("2026-09", states = "MD", ma_only = TRUE)  # any CMS month since Dec 2019
```

Loaders filter by `plans`, `states`, `counties`, `years` (`months` for enrollment) and `variables`. A transition is indexed by its December: `dec_year` 2025 is December 2025 to January 2026.

## Datasets

| Dataset | One row per | Contents |
|---|---|---|
| `exits` | December plan × county | whether CMS terminated the plan or cut the county from its service area the next January; December and September enrollment (September covers the newest year before its December file is out) |
| `pdp_exits` | December standalone Part D plan × state | whether CMS terminated the drug plan the next January; December and September enrollment |
| `displacement` | December plan × county | January outcome, lost coverage, whether the plan, contract or parent left, the county's plans next January, plan details |
| `plan_details` | plan × segment × year | organization and parent, premiums, deductible, out-of-pocket maximum, star ratings, SNP details |
| `county_panel` | county × year | exit rate, displaced enrollment, December and January enrollment, 2018 penetration, benchmark |
| `plan_county` | crosswalk link × county × year | crosswalk status, December and January enrollment, exit flags |
| `landscape` | plan × segment × county × year | service areas (CY2016 on), plan type, SNP type |
| `enrollment` | plan × county × month | the December and January enrollment the pipeline uses |

Each dataset has a help page (`?displacement`), and [DATA_DICTIONARY.md](DATA_DICTIONARY.md) defines every column.

- **Counting.** `displacement` and `exits` have one row per plan-county, and `pdp_exits` one per plan-state, so sums count each enrollee once. `plan_county` has one row per crosswalk link, so a plan's enrollment can repeat across rows; its `_once` columns, and `county_panel`'s, count each plan-county once. The `counting` column of `maexits_catalog()` flags every such column.
- **Suppressed counts.** CMS suppresses counts under 11. In `displacement`, `exits`, `pdp_exits`, `plan_county` and `county_panel` each suppressed cell counts as 10, and the `_low` columns count it as 1. `enrollment` leaves them out of its totals and counts them in `n_suppressed`.
- **Plan universe.** Individual-market MA plans and SNPs; standalone drug plans, Medicare-Medicaid Plans and employer group plans are excluded. `pdp_exits` covers individual-market standalone drug plans, and `enrollment` and `cms_enrollment()` hold every plan in the CMS files.

## Building the data

The tables come from the GitHub release [`data-2018-2025`](https://github.com/MatthewLavallee/maexits/releases/tag/data-2018-2025) and are rebuilt from the CMS and NBER files with `run_data_pipeline()`. [PIPELINE.md](PIPELINE.md) covers the source files, the build, the yearly update and `check_cms_releases()`, which reports when CMS posts next year's files.

MIT license.
