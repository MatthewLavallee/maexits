# Building the data

How the tables are built from the raw CMS and NBER files, and how to update them each year. For loading the processed data, see the [README](README.md).

## Repository structure

```text
maexitsv2/
├── DESCRIPTION                  # R package metadata (data.table + here; see Reproducing)
├── README.md, PIPELINE.md, DATA_DICTIONARY.md
├── NAMESPACE                    # exported functions
├── R/
│   ├── config.R                # years covered + exact CMS input files (edit this each cycle)
│   ├── validate.R              # check_inputs() and the fail-loud checks every run performs
│   ├── data_build.R            # make_enrollment(), make_landscape(), make_analytic()
│   ├── data_augment.R          # FIPS, penetration, benchmark, augment_analytic(), make_county_panel(), run_data_pipeline()
│   ├── plan_details.R          # make_plan_details(): premiums, MOOP, stars, SNP details by plan and year
│   ├── displacement.R          # make_displacement(): one row per December plan x county, with its January outcome
│   ├── preliminary.R           # run_preliminary() / backtest_preliminary(): estimate before Dec/Jan CPSC
│   ├── release_watch.R         # check_cms_releases(), schedule_release_check(): know when next cycle's files are out
│   ├── data_api.R              # maexits_data(), cms_enrollment(): load and filter data
│   ├── catalog.R, datasets.R   # the variable key: maexits_catalog() and the ?plan_county-style help pages
│   ├── release.R               # prepare_data_release(): package derived tables for a GitHub data release
│   ├── analysis.R              # descriptive + geographic analysis functions (no file writes)
│   ├── globals.R, maexitsv2-package.R   # package plumbing
├── LICENSE, LICENSE.md          # MIT
├── tests/testthat/             # failure-mode tests + opt-in full-rebuild regression test
├── provenance/                 # md5 manifest of raw/ and the derived tables (data release data-2018-2025)
├── man/                         # roxygen2-generated .Rd documentation; man/figures holds the README figure
├── tools/readme-figure.R        # draws the README figure from the displacement table
├── trunk/
│   └── derived/
│       └── county_panel.csv    # analysis-ready county × year panel (committed uncompressed, ~6 MB)
│                               # other derived/*.csv are gitignored — restore from the data release's derived_data.zip
└── raw/                        # GITIGNORED (~4.3 GB public CMS/NBER inputs) — see Data Sources
```

Note: `raw/` and most of `trunk/derived/` are gitignored (see `.gitignore`); only `trunk/derived/county_panel.csv` is tracked uncompressed.

## Data

There are three ways to obtain the analysis data, in order of convenience:

**(a) Quickest — download the derived tables from the data release.** From the repository root:

```bash
gh release download data-2018-2025 -R MatthewLavallee/maexits -p derived_data.zip
unzip derived_data.zip
```

(Or download `derived_data.zip` from the release page on GitHub.)

This restores the full set of derived tables to `trunk/derived/` (`december_enrollment.csv`, `january_enrollment.csv`, `landscape.csv`, `analytictable.csv`, `analytictable_augmented.csv`, `county_panel.csv`, `plan_details.csv`, `displacement.csv`) and lets you skip the ~4.3 GB raw rebuild entirely.

**(b) County panel only.** `trunk/derived/county_panel.csv` (the analysis-ready county × year panel, ~6 MB) is committed uncompressed and is available immediately after cloning — no unzip required. This is sufficient for all geographic-analysis functions.

**(c) Full rebuild from raw.** Download the public CMS/NBER inputs into `raw/` following the layout in **Data Sources** and **Recreating the `raw/` directory** below, then run `run_data_pipeline()` (see **Reproducing the pipeline**).

## Data Sources

All raw inputs are PUBLIC files from CMS and NBER. The `raw/` directory (~4.3 GB) is gitignored; download the files below and recreate the folder layout shown in the next section. (To skip the rebuild entirely, download `derived_data.zip` from the data release and unzip it at the repo root to restore `trunk/derived/*.csv`; the analysis-ready `trunk/derived/county_panel.csv` is also committed uncompressed.)

| Dataset | Used for | Source (page title) | URL | Repo folder |
|---|---|---|---|---|
| CPSC Monthly Enrollment by Contract/Plan/State/County | `make_enrollment()` Dec & Jan enrollment by contract×plan×county; also the SSA↔FIPS crosswalk for FIPS and benchmark joins | Monthly Enrollment by Contract/Plan/State/County \| CMS | https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract/plan/state/county | `raw/december enrollment/`, `raw/january enrollment/` |
| MA / Part D Landscape Source Files | `make_landscape()` MA + SNP plan service areas, plan types, SNP flags (CY2016–CY2026; PDP dropped); `make_plan_details()` premiums, deductible, MOOP, star ratings, SNP details | Benefits Data \| CMS (Landscape Source Files, within MA/Part D Contract and Enrollment Data) | https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/benefits-data | `raw/landscape/CY2016` … `raw/landscape/CY2026` |
| Plan and Premium Information for Medicare Plans Offering Part D Coverage (CY2018–CY2024) | `make_plan_details()` Part C and Part D premiums, low-income premium, Part C/D star ratings (CY2024) | Benefits Data \| CMS (same page as the landscape files) | https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/benefits-data | the zip (CY2018–2023) or csvs (CY2024) registered in `MAEXITS_PARTD_REPORT_FILES` under `raw/landscape/CY<year>/` |
| Part C&D Plan Crosswalk files | `make_analytic()` / `augment_analytic()` classify each Dec(N-1)→Jan(N) transition by crosswalk STATUS/DESCRIPTION (renewals, consolidations, SAR/SAE, terminations) | Plan Crosswalks \| CMS (landing); per-year e.g. "2025 Part C&D Plan Crosswalk \| CMS" | https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/plan-crosswalks | `raw/plan crosswalk/` |
| MA State/County Penetration (2018-12) | `add_penetration()` 2018 county MA penetration → `penetration_2018`, `eligibles_2018`, penetration quartiles | MA State/County Penetration \| CMS | https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/ma-state/county-penetration | `raw/penetration/State_County_Penetration_MA_2018_12/` |
| NBER Medicare Advantage Ratebook (county benchmark, Parts A&B, 0% bonus) | `add_benchmark()` county benchmark capitation rate by year → `benchmark` | Medicare Advantage Ratebook Data \| NBER | https://www.nber.org/research/data/medicare-advantage-ratebook-data | `raw/ratebook/` |
| CMS Ratebooks & Supporting Data (underlying source of the NBER ratebook) | Reference / provenance for the NBER county benchmark rates | Ratebooks & Supporting Data \| CMS | https://www.cms.gov/medicare/payment/medicare-advantage-rates-statistics/ratebooks-supporting-data | (not downloaded directly; NBER repackages these into `countyrateYYYY.csv`) |

Notes on verification:
- All CMS and NBER URLs above were web-verified as live, current canonical pages (June 2026). The NBER ratebook page was confirmed to list `countyrate2018.csv … countyrate2026.csv` (the build reads 2018-2025 today; `countyrate2026.csv` is needed once `dec_year` 2026 is added), and to link to the CMS "Ratebooks & Supporting Data" page (modern URL `…/medicare-advantage-rates-statistics/ratebooks-supporting-data`; the legacy `…/MedicareAdvtgSpecRateStats/Ratebooks-and-Supporting-Data.html` link on NBER redirects there).
- The Plan Crosswalks and Benefits Data CMS pages return HTTP 403 to automated fetchers but were confirmed via search to be the correct canonical landing pages; the crosswalk legacy landing URL `https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Plan-Crosswalks` also resolves. Per-year crosswalks live at `…/plan-crosswalks/<YEAR>-part-cd-plan-crosswalk`.

## Recreating the `raw/` directory

The pipeline reads files by exact folder/filename patterns. The tree below shows the layout confirmed against `R/data_build.R`, `R/data_augment.R`, and the existing `raw/` listing. Year coverage was confirmed from the code, not assumed:

- Enrollment: for each crosswalk year N in `MAEXITS_XWALK_YEARS` (`R/config.R`, currently 2019-2026) the build needs **December enrollment for N-1** and **January enrollment for N**, i.e. December 2018-2025 and January 2019-2026. The SSA↔FIPS lookup used by `add_fips()` and `add_benchmark()` is `MAEXITS_FIPS_LOOKUP_FILE` (`raw/january enrollment/CPSC_Enrollment_2025_01/CPSC_Enrollment_Info_2025_01.csv`), so that file must be present.
- Landscape: `make_landscape()` reads **CY2016 through the last crosswalk year** (CY2026 today; PDP rows dropped).
- Crosswalk: one registered file per year in `MAEXITS_XWALK_FILES` → crosswalk years **2019-2026**.
- Ratebook: `add_benchmark()` reads `countyrate<Y>.csv` for every `dec_year` Y in the table → **2018-2025** today; a missing year stops the build.
- Penetration: single file, **2018-12** only.

```text
raw/
├── december enrollment/
│   ├── CPSC_Enrollment_2018_12/CPSC_Enrollment_Info_2018_12.csv   (+ CPSC_Contract_Info_2018_12.csv)
│        (every December and January folder also needs its CPSC_Contract_Info_YYYY_MM.csv: organizations and parents for make_plan_details() and make_displacement())
│   ├── CPSC_Enrollment_2019_12/CPSC_Enrollment_Info_2019_12.csv
│   ├── CPSC_Enrollment_2020_12/CPSC_Enrollment_Info_2020_12.csv
│   ├── CPSC_Enrollment_2021_12/CPSC_Enrollment_Info_2021_12.csv
│   ├── CPSC_Enrollment_2022_12/CPSC_Enrollment_Info_2022_12.csv
│   ├── CPSC_Enrollment_2023_12/CPSC_Enrollment_Info_2023_12.csv
│   ├── CPSC_Enrollment_2024_12/CPSC_Enrollment_Info_2024_12.csv
│   └── CPSC_Enrollment_2025_12/CPSC_Enrollment_Info_2025_12.csv
│        (file matcher is recursive on pattern "CPSC_Enrollment_Info"; year parsed from filename _YYYY_)
│
├── january enrollment/
│   ├── CPSC_Enrollment_Info_2018_01.csv   (top level; not needed by any transition, but read, and part of the recorded january_enrollment.csv)
│   ├── CPSC_Enrollment_2019_01/CPSC_Enrollment_Info_2019_01.csv
│   ├── CPSC_Enrollment_2020_01/CPSC_Enrollment_Info_2020_01.csv
│   ├── CPSC_Enrollment_2021_01/CPSC_Enrollment_Info_2021_01.csv
│   ├── CPSC_Enrollment_2022_01/CPSC_Enrollment_Info_2022_01.csv
│   ├── CPSC_Enrollment_2023_01/CPSC_Enrollment_Info_2023_01.csv
│   ├── CPSC_Enrollment_2024_01/CPSC_Enrollment_Info_2024_01.csv
│   ├── CPSC_Enrollment_2025_01/CPSC_Enrollment_Info_2025_01.csv   <-- REQUIRED: SSA<->FIPS lookup (MAEXITS_FIPS_LOOKUP_FILE)
│   └── CPSC_Enrollment_2026_01/CPSC_Enrollment_Info_2026_01.csv
│
├── landscape/
│   ├── CY2016/ … CY2023/        (each: MA and SNP CSVs; reader greps filenames for "MA"/"SNP",
│   │                             skip=5 for MA, skip=4 for SNP, skip=6 for SNP in CY2023;
│   │                             CY2018-CY2023 also hold the Part D Plan and Premium report zip
│   │                             registered in MAEXITS_PARTD_REPORT_FILES)
│   ├── CY2024/csv version/
│   │   ├── CY2024_Landscape_MA_20240723.csv
│   │   ├── CY2024_Landscape_SNP_20240710.csv
│   │   └── sanctioned plans/
│   │       ├── CY2024_Landscape_MA_sanctioned_20240628.csv
│   │       ├── CY2024_Landscape_SNP_sanctioned_20240628.csv
│   │       └── CY2024_Plan_Premium_Report_sanctioned_20240628.csv   (+ CY2024_Plan_Premium_Report_20240723.csv one level up)
│   ├── CY2025/CY2025_Landscape_202506.1.csv     (single combined CSV)
│   └── CY2026/CY2026_Landscape_202509.csv        (single combined CSV)
│
├── plan crosswalk/
│   ├── PlanCrosswalk2019_10012018.txt
│   ├── PlanCrosswalk2020_10022019.txt
│   ├── PlanCrosswalk2021_10222020.txt
│   ├── PlanCrosswalk2022_10042021.txt
│   ├── PlanCrosswalk2023_10032022.txt
│   ├── PlanCrosswalk2024_09282023.txt   <-- the 2024 crosswalk
│   ├── PlanCrosswalk2024_10012024.txt   <-- the 2025 crosswalk (CMS named it "2024"; resolver picks 10012024)
│   └── PlanCrosswalk2026_10012025.txt   <-- the 2026 crosswalk
│        (.txt is read; .xlsx and *_readme.pdf may sit alongside but are not read)
│
├── penetration/
│   └── State_County_Penetration_MA_2018_12/
│       └── State_County_Penetration_MA_2018_12.csv
│
└── ratebook/
    ├── countyrate2018.csv
    ├── countyrate2019.csv
    ├── countyrate2020.csv
    ├── countyrate2021.csv
    ├── countyrate2022.csv
    ├── countyrate2023.csv
    ├── countyrate2024.csv   (CMS reformatted this year: reader uses skip=3 and a "0% Bonus" column)
    └── countyrate2025.csv
```

Filename specifics enforced by the code (set in `R/config.R`):
- **Years covered:** `MAEXITS_XWALK_YEARS` (currently `2019:2026`). Coverage is never inferred from what is on disk.
- **Enrollment:** files are found recursively under each month folder and must be named `CPSC_Enrollment_Info_YYYY_MM.csv`. The month must match the folder (`12` in `december enrollment`, `01` in `january enrollment`), each year may appear only once, and all files must share one header. Anything else stops the build. The FIPS and SSA lookups use `MAEXITS_FIPS_LOOKUP_FILE` (the January 2025 file).
- **Crosswalks:** each year's exact file is registered in `MAEXITS_XWALK_FILES`. The 2025 crosswalk is registered as `PlanCrosswalk2024_10012024.txt`, which is how CMS named it. The status vocabulary is checked against `MAEXITS_XWALK_STATUS_CLASS`.
- **Landscape:** CY2016-CY2023 are read by globbing `*.csv` under each `CYxxxx/` folder, grepping filenames for `MA` vs `SNP`, and checking each file's columns. CY2024 uses its four fixed files. CY2025 and later use the file registered in `MAEXITS_LANDSCAPE_FILES`. CY2026 is pinned to the `202509` vintage behind the committed tables.
- The `raw/landscape/2006-2024` and `CY2013`-`CY2015` folders on disk are not read by the current pipeline (it starts at CY2016) and are not required.
- `provenance/raw_manifest_data-2018-2025.csv` records the size and md5 of every raw file behind the committed derived tables (data release `data-2018-2025`).

Repo distribution reminder: `raw/` is gitignored. To run from raw inputs, recreate the tree above and call `run_data_pipeline()`. To skip the ~4.3 GB rebuild, download `derived_data.zip` from the data release and unzip it at the repo root to restore `trunk/derived/*.csv`; `trunk/derived/county_panel.csv` is also committed uncompressed.

## Reproducing the pipeline

This is an R package (see `DESCRIPTION`). The build relies on **`data.table`** (all tables are `data.table`s) and **`here`** (all paths are resolved from the project root via `here()`), so commands must be run with the working directory at — or anywhere inside — the repository, and with the `raw/` tree in place.

Load the package functions and run the full build with `run_data_pipeline()`:

```r
# from the repository root
library(data.table)
library(here)

# load the package (or source the R/ files directly)
devtools::load_all(".")
# or, without devtools:
#   for (f in list.files("R", full.names = TRUE)) source(f)

# Check that every input exists, then run the full build: enrollment ->
# landscape -> analytic table -> FIPS/penetration -> benchmark -> augment
# -> county panel -> plan details -> displacement.
check_inputs()
panel <- run_data_pipeline(save = TRUE, verbose = TRUE)

# Trial run that writes somewhere else:
# run_data_pipeline(out_dir = "/tmp/maexits_trial")
```

`run_data_pipeline()` checks its inputs first, passes in-memory tables between steps, and runs the validation checks in `R/validate.R` along the way. It writes nothing until the whole run has succeeded. Then it writes all eight tables to `out_dir` (default `trunk/derived/`) together, so a failed run never leaves a mix of old and new files. A full build takes about 6 minutes and produces no warnings.

Individual steps can also be run on their own. Each function reads its inputs from `trunk/derived/` when its argument is `NULL`. Pass the landscape and the benchmarked table along explicitly, because `add_benchmark()` does not write a file:

```r
dec <- make_enrollment("december enrollment")         # december_enrollment.csv
jan <- make_enrollment("january enrollment")          # january_enrollment.csv
ls  <- make_landscape()                               # landscape.csv (MA + SNP; PDP dropped)
at  <- make_analytic(ls, dec, jan)                    # analytictable.csv
at  <- add_fips_and_penetration(at)                   # analytictable.csv (+ fips, 2018 penetration)
at  <- add_benchmark(at)                              # + NBER county benchmark (in memory)
at  <- augment_analytic(at, landscape = ls)           # analytictable_augmented.csv (sar_dropped, forced, role)
panel <- make_county_panel(at)                        # county_panel.csv (county x dec_year)
details <- make_plan_details()                        # plan_details.csv (plan x segment x contract year)
ds  <- make_displacement(at, landscape = ls, plan_details = details, panel = panel)  # displacement.csv
```

## Annual update (each new cycle)

Crosswalk year N covers the December N-1 → January N transition. The next cycle is **crosswalk year 2027**, which is `dec_year` 2026. The timing below is when CMS posted each file in past years (checked against the CMS RSS feeds and web archives in September 2026). `check_cms_releases()` tells you when each one is out (see below). For each input, the table gives where it goes and what to change.

| When | Input | Put it in | Then |
|---|---|---|---|
| Any time (NBER posts it in April of N-2) | NBER `countyrate<N-1>.csv` | `raw/ratebook/` | Check its header has `ssa_code`/`parts_0_bonus` (or CMS's `0% ... Bonus` layout) and that code `01000` is present |
| ~Sep 24 – Oct 1 | CY N landscape (`CY<N>_Landscape_<yyyymm>.csv`) | `raw/landscape/CY<N>/` | Register the exact path in `MAEXITS_LANDSCAPE_FILES`. Use the initial release and record its readme date and md5; don't swap in later reissues. The file's `Contract Year` must equal N. Renamed or missing required columns fail loudly; added columns are ignored. Add new column names to `.LANDSCAPE_COLUMNS` in `data_build.R`. |
| ~Oct 1 – 7 (Nov 4 in 2025) | N Part C&D Plan Crosswalk (`PlanCrosswalk<label>_<MMDDYYYY>.txt`) | `raw/plan crosswalk/` (never rename CMS files) | Register the exact filename in `MAEXITS_XWALK_FILES`. The label may not equal N; CMS named the 2025 file "2024". Read the readme's status table: new statuses stop the build until added to `MAEXITS_XWALK_STATUS_CLASS` and the role rules. |
| Oct – Nov | A pre-exit CPSC month, e.g. September N-1 | `raw/monthly enrollment/` (**never** the december/january folders) | Run the preliminary estimate (below) |
| ~Dec 11–14 | December N-1 CPSC | `raw/december enrollment/CPSC_Enrollment_<N-1>_12/` | Extract it exactly once. It is not read until N is in `MAEXITS_XWALK_YEARS`. Optionally rerun the preliminary estimate with this file as the proxy, for a December-based exit estimate before January arrives. |
| ~Jan 8 – Feb 13 | January N CPSC | `raw/january enrollment/CPSC_Enrollment_<N>_01/` | Add N to `MAEXITS_XWALK_YEARS`, run `check_inputs()`, then do a trial `run_data_pipeline(out_dir = ...)`. Compare its `dec_year` ≤ N-2 rows with the committed tables (the regression test does this for 2018–2025), then run into `trunk/derived/`. |
| After the January run | — | — | Build the release files with `prepare_data_release()`, zip `trunk/derived/*.csv` as `derived_data.zip`, and publish both as a new data release; point `MAEXITS_DATA_RELEASE` at it. Record the new inputs and outputs in `provenance/` (raw manifest rows and a `derived_md5_<release>.txt`). Commit. |
| Late Jan – Feb | Possible CMS reissue of the December CPSC or the crosswalk | Replace in place | Compare md5s with `provenance/`. If one changed, rerun and diff. |

`check_inputs()` lists everything that is still missing. Each validation check stops with a `[validate]` message that says what to fix. The thresholds (share of imputed values, January/December totals, SAR-dropped share, benchmark coverage, crosswalk content) come from the 2018–2025 data and are documented in `R/validate.R`.

### Knowing when the files are out

`check_cms_releases()` checks whether each file the next cycle needs is out: the crosswalk and landscape for the first year not yet registered in `R/config.R`, the December and January CPSC files, and the NBER ratebook. It reads the CMS Plan Crosswalks and CPSC RSS feeds, the landscape page and NBER's ratebook folder, and sends HEAD requests to the file links. It never downloads anything, and it never follows redirects, because CMS redirects some usual file addresses to different files.

```r
check_cms_releases()                       # a few seconds
#>   2027 Part C&D Plan Crosswalk      not yet   not listed yet (the list ends at 2026)
#>   CY2027 MA landscape               not yet   not listed yet (the page lists CY2026 Landscape (202609) (ZIP))
#>   December 2026 CPSC enrollment     not yet   not out yet (latest CPSC month: 2026-09)
#>   January 2027 CPSC enrollment      not yet   not out yet (latest CPSC month: 2026-09)
#>   NBER ratebook countyrate2026.csv  released  countyrate2026.csv is on NBER

schedule_release_check()                   # check daily at 17:45 until every file is out
schedule_release_check(targets = "crosswalk")
release_check_status()                     # what it has found; acknowledge = TRUE marks it seen
unschedule_release_check()
```

Statuses: `released` (listed and downloadable), `hint` (a file is up at the usual address but not listed), `listed` (listed, file not downloadable yet), `anomaly` (a file is there but its name, type or size is unexpected), `not_yet`, `unknown` (CMS or NBER could not be reached) and `broken`. `broken` means a page loaded but did not show last year's file either, so the page has probably changed and the checker needs updating.

On macOS, `schedule_release_check()` installs a launchd job in `~/Library/LaunchAgents`, so nothing needs to stay open. The job runs every day at the chosen time and at login, which catches up days the Mac was off. It runs the **installed** package, so install it first (`remotes::install_github("MatthewLavallee/maexits")` or `devtools::install()`). Each finding raises one desktop notification and stays in `release_check_status()` and the package startup message until you acknowledge it. A file later than in any past year and a checker that cannot reach CMS for three runs are flagged the same way. The job removes itself once every file is out, or on 31 March. Notifications come from "Script Editor"; if none appear, allow them in System Settings > Notifications. On other systems the function prints a cron or Task Scheduler line to add.

### Preliminary estimate (before December/January enrollment)

Once the crosswalk and landscape for year N are out (~Oct 1), the exiting plans are known. Exit exposure needs only a pre-exit enrollment month:

```r
run_preliminary(2027, "monthly enrollment/CPSC_Enrollment_2026_09/CPSC_Enrollment_Info_2026_09.csv")
# -> trunk/derived/preliminary/county_panel_prelim_xw2027_src202609.csv (never overwrites final tables)
```

Only exit measures are produced (displaced enrollment, exit rate, exit group). All January-dependent columns are `NA`. The crosswalk and landscape must be registered in `R/config.R`, but N does not need to be added to `MAEXITS_XWALK_YEARS` yet.

Score a proxy month against a completed year before quoting it with `backtest_preliminary()`, which compares the `_once` measures (each plan-county counted once). Report the state error bands with any state figure. With an 11-month-stale proxy (January of the same year), the backtests gave:
- **2025:** 10.18% vs 9.89% final, county correlation 0.994, high-exit classification agreeing for 96.6% of counties.
- **2024:** 7.25% vs 7.22% final.
- **Worst states:** by exit rate, Idaho 2025 (56.1% vs 50.7%, 5.4 pp) and Maryland 2024 (6.7% vs 10.2%, 3.5 pp). By displaced enrollment, small bases swing most: Puerto Rico 2025 +92.6%, Hawaii 2025 +69.7%, DC 2024 −41.5%, Maryland 2024 −38.5%. Take state error bands from `backtest_preliminary()$state`, not from these examples.

## Tests

```r
testthat::test_dir("tests/testthat")                      # failure-mode tests (seconds)
Sys.setenv(MAEXITS_FULL_REBUILD = "true")                  # + full rebuild from raw/ (~5 min),
testthat::test_dir("tests/testthat")                      #   checked against provenance/derived_md5_data-2018-2025.txt
```

## Outputs & data dictionary

`run_data_pipeline()` writes the following to `trunk/derived/`:

| File | Grain | Description |
|---|---|---|
| `december_enrollment.csv` | contract × plan × county | Aggregated CPSC December enrollment |
| `january_enrollment.csv` | contract × plan × county | Aggregated CPSC January enrollment |
| `landscape.csv` | contract × plan × segment × county × year | Harmonized MA + SNP service areas, plan types, SNP flags (CY2016-CY2026) |
| `analytictable.csv` | crosswalk link × county × `dec_year` | Core analytic table: crosswalk `status`, successor IDs, `dec_enrollment`, `jan_enrollment` with `_low` bounds and sources, plan and SNP type, `fips`, 2018 penetration (no `benchmark` — it is added in-memory and written only to `analytictable_augmented.csv`) |
| `analytictable_augmented.csv` | crosswalk link × county × `dec_year` | Adds `benchmark`, the exit flags (`sar_dropped`, `forced`, `forced_county`, `forced_reason`), `role`, and the counting columns (`prev_links`, `dec_enrollment_once`, `dec_enrollment_split`, ...) |
| `plan_details.csv` | contract year × plan × segment | Plan characteristics from the landscape files, the Part D Plan and Premium reports and CPSC Contract Info (`make_plan_details()`) |
| `displacement.csv` | December plan × county | One row per December plan-county: outcome in January, lost coverage, scope flags, alternatives, plan details (`make_displacement()`) |
| `county_panel.csv` | county × `dec_year` | Analysis-ready panel: exit rate, displaced enrollment, enrollment change, incumbent and new-entrant enrollment, exit group, `pen_quartile`, each summed over links and counted once per plan-county (`_once`) (committed uncompressed) |

Field-level definitions, status categories, and derived-variable formulas are documented in [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md). Per-function reference documentation is in `man/` (roxygen2-generated `.Rd` files).

## Analysis functions

Defined in `R/analysis.R`. Each takes a `data.table` as its first argument (passing `NULL` triggers a read from the standard `trunk/derived/` CSV path); none write files — they return `data.table`s for the caller to print, format, or pass to `kable`/`flextable`.

**Plan-level summaries** (operate on `analytictable.csv`):
- `summarize_exits()` — terminated / non-renewed plans and their December enrollment
- `summarize_sar()` — service-area reductions and their enrollment change
- `summarize_sae()` — service-area expansions, split into existing vs. expansion counties
- `summarize_new_plans()` — new plans and initial contracts
- `summarize_stable()` — stable renewals (renewal + consolidated renewal)
- `summarize_by_status()` — cross-tabulations of plan counts and enrollment by status

**Geographic analysis** (operate on `county_panel.csv`):
- `tabulate_state_exits()` — enrollment-weighted exit rates by state and year, plus a national summary
- `tabulate_exit_dynamics()` — mean/median enrollment change and share of counties losing enrollment by exit-exposure group
- `tabulate_penetration_impacts()` — unweighted and enrollment-weighted impacts by 2018 MA penetration quartile
- `test_penetration_hypothesis()` — correlations between 2018 penetration and enrollment change, overall and conditional on having exits

`run_all_analyses()` runs all of the above and returns the results as a named list.

## Notes / caveats

- **Suppressed counts.** CMS suppresses enrollment counts below 11, reported as `*`. Each suppressed cell counts as **10**; the `_low` columns count it as 1. A plan-county with no CMS record counts as 0, and `dec_src` / `jan_src` say which case applies. Terminated links and service-area reductions that dropped the county have `jan_enrollment` 0.
- **`dec_year` convention.** Each transition is indexed by `dec_year`, the December (year N-1) side of a Dec(N-1)→Jan(N) crosswalk. The build covers crosswalk years 2019-2026, i.e. `dec_year` 2018-2025. The benchmark for `dec_year` Y is the *concurrent* calendar-year-Y capitation rate, not the rate announced for the following plan year.
- **Crosswalk links.** Rows of the plan-level tables are crosswalk links, so some enrollment values repeat across rows; see "Counting" in the [README](README.md). `multi_status` flags plan-segment-counties with more than one row.
- **Plan universe.** Individual-market MA plans and SNPs in the CMS landscape, on both sides of each transition. Standalone drug plans, Medicare-Medicaid Plans and employer group plans are excluded.
