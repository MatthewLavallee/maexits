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
e <- maexits_data("exit_sept")
e[, .(share = sum(sep_enrollment[exit_type != "none"]) / sum(sep_enrollment)), keyby = dec_year]

maexits_data("county_panel", years = 2024:2025, states = "MD")
maexits_data("plan_info", plans = "H2001", years = 2026)
cms_enrollment("2026-09", states = "MD", ma_only = TRUE)  # any CMS month since Dec 2019
```

Loaders filter by `plans`, `states`, `counties`, `years` and `variables` (`cms_enrollment()` by month). A transition is indexed by its December: `dec_year` 2025 is December 2025 to January 2026.

## Datasets

| Dataset | One row per | Contents |
|---|---|---|
| `county_panel` | county × year | exit rate, displaced enrollment, December, January, February, March and April enrollment, 2018 penetration, benchmark |
| `plan_county` | crosswalk link × county × year | crosswalk status, December through April enrollment, exit flags |
| `displacement` | December plan × county | January outcome, lost coverage, whether the plan, contract or parent left, the county's plans next January, the successor's and the county's January–April enrollment, plan details |
| `exits` | December plan × county | whether CMS terminated the plan or cut the county from its service area the next January, on December enrollment |
| `exit_sept` | December plan × county | the same exits on September enrollment, every year including the newest (out before its December file) |
| `pdp_exits` | December standalone Part D plan × state | whether CMS terminated the drug plan the next January; December and September enrollment |
| `plan_info` | plan × segment × year | organization and parent, premiums, deductible, out-of-pocket maximum, star ratings, SNP details, Part B giveback and supplemental benefits (dental, vision, hearing, OTC allowance, meals, transportation, fitness, and more), CY2018 on |

Each dataset has a help page (`?displacement`), and [DATA_DICTIONARY.md](DATA_DICTIONARY.md) defines every column.

- **Counting.** `displacement`, `exits` and `exit_sept` have one row per plan-county, and `pdp_exits` one per plan-state, so sums count each enrollee once. `plan_county` has one row per crosswalk link, so a plan's enrollment can repeat across rows; its `_once` columns, and `county_panel`'s, count each plan-county once. The `counting` column of `maexits_catalog()` flags every such column.
- **Suppressed counts.** CMS suppresses counts under 11. Each suppressed cell counts as 10, and the `_low` columns count it as 1.
- **Plan universe.** Individual-market MA plans and SNPs; standalone drug plans, Medicare-Medicaid Plans and employer group plans are excluded. `pdp_exits` covers individual-market standalone drug plans, and `cms_enrollment()` holds every plan in the CMS files.
- **Spring months.** February, March and April enrollment follow the January plans through the MA Open Enrollment Period (January–March, changes effective the next month), so April is the first month after it. These columns are NA for a transition until CMS posts the month.

## Building the data

The tables come from the GitHub release [`data-2018-2025`](https://github.com/MatthewLavallee/maexits/releases/tag/data-2018-2025) and are rebuilt from the CMS and NBER files with `run_data_pipeline()`. [PIPELINE.md](PIPELINE.md) covers the source files, the build, the yearly update and `check_cms_releases()`, which reports when CMS posts next year's files.

MIT license.
