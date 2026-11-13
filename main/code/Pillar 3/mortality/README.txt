Mortality pipeline — SOWC 2026, Pillar 3
========================================

Replicates Wagner et al. 2018 (Lancet 392:857-65), "Armed conflict and child
mortality in Africa", on a wider evidence base: global rather than Africa-only,
UCDP GED through 2024 rather than 2015, and DHS plus MICS rather than DHS alone.

This pipeline is deliberately structured differently from the education,
child_marriage and nutrition pipelines in this folder. Those compute per-survey
design-based prevalence contrasts (svymean / svyttest inside a per-survey
AnalyzeSurvey()), which answers "is prevalence different inside vs outside
conflict zones in survey X". Wagner asks a within-cluster-over-time question,
which needs one pooled child-level regression across all surveys with cluster
fixed effects. So conflict exposure and covariates are computed once against the
union of all clusters, and the deliverable is a single master table with one row
per birth — not a per-survey results CSV.


Run order
---------

  1 mortality_prep_conflict_events.R      -> mortality_ged_events.rds
  2 mortality_build_dhs_births.R          -> mortality_dhs_births.rds
  3 mortality_build_mics_births.R         -> mortality_mics_births.rds
  4 mortality_build_cluster_inventory.R   -> mortality_clusters.rds
  5 mortality_build_cluster_conflict.R    -> mortality_cluster_conflict.rds
  6 mortality_build_cluster_covariates.R  -> mortality_cluster_covariates.rds
    6a_nightlights.js                     -> run in Google Earth Engine, feeds 6
  7 mortality_build_master.R              -> mortality_master.rds
  8 mortality_validate.R                  -> mortality_validation.csv

Scripts 2 and 3 must run before 4; 4 before both 5 and 6.


Status
------

Script 1 runs today and has been verified: 355,726 events, 1989-2024,
3,953,462 battle deaths.

Scripts 2-8 are written but blocked on data that is not yet on disk (below).
All eight parse cleanly. The two pieces of non-obvious logic have been tested
against real data rather than assumed:

  - the 50 km spatial join (script 5), checked on the 2,079 Nigeria MICS 2021
    clusters; the vectorised aggregation was confirmed to give output identical
    to the per-cluster version, twelve times faster
  - the chronicity streak counter (script 7), checked against a hand-worked
    example including gap resets and censoring at 5


Data still needed
-----------------

BLOCKING

  1. IPUMS-DHS extract, BIRTHS unit of analysis, saved as
     data/IPUMS extracts/DHS/births_mortality.dta
     The existing births.dta was slimmed to four columns with no GPS and no
     survival variables, so it cannot be reused. Variable list is in the
     col_select at the top of script 2.

     Samples: all 142 are confirmed available. IPUMS-DHS names births-unit
     samples with a "br" suffix (306 exist in total; "ir" is women, "kr"
     children, "pr" members). Every survey in get_from_ipums_dhs.csv resolves to
     one, with no gaps — the resolved list is in this folder as
     ipums_dhs_births_samples.csv, and its ipums_sample column can be pasted
     straight into the extract builder.

  2. IPUMS-MICS extract, Births unit, saved as
     data_derived/ipums_mics_extract_births.dta
     Exactly 15 samples — the only MICS surveys with both a Births unit and GPS:
     COM 2022, SWZ 2021, FJI 2021, GMB 2018, GHA 2017, HND 2019, LAO 2017,
     LSO 2018, MDG 2018, MWI 2019, NGA 2016, NGA 2021, PSE 2019, SLE 2017,
     ZWE 2019.
     GEO 2018, THA 2022, LAO 2023 and XKX 2013/2019 have GPS folders on disk but
     no Births sample in IPUMS, so they cannot enter this analysis.

  3. IPUMS-MICS HH characteristics re-pull adding FJI 2021, MWI 2019, PSE 2019.
     The current extract covers 13 samples and overlaps our 15 on only 12, and
     it is the only source of imicsstratum.

  4. DHS Geospatial Covariate files, one per survey, from
     spatialdata.dhsprogram.com/covariates/ -> data/DHS GeoCovariates/
     Only the DEGURBA_* columns are used. MICS equivalents are already on disk.

CONTROLS (script 6 skips each section whose input is absent, so it stays
runnable as these arrive)

  5. CHIRPS v2.0 annual precipitation -> data/CHIRPS/chirps-v2.0.<YYYY>.tif
     https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_annual/tifs/

  6. TerraClimate tmax and tmin -> data/TerraClimate/
     http://thredds.northwestknowledge.net/thredds/fileServer/TERRACLIMATE_ALL/data/

  7. Harmonized DMSP-VIIRS night lights, via 6a_nightlights.js in Google Earth
     Engine -> data_derived/mortality_nightlights_gee.csv
     Needs a free GEE account. Running it there avoids downloading tens of GB.

  8. WFP VAM global food prices -> data/WFP/wfp_food_prices_global.csv
     https://data.humdata.org/dataset/global-wfp-food-prices
     Coverage is thin before ~2005, so early birth cohorts will carry NA.

  9. McGuirk & Burke 2020 JPE replication package, for the food-price
     sensitivity check. Africa-only.


Missing R packages
------------------

Under R 4.6 the following are not installed anywhere on the library path,
although the other Pillar 3 pipelines import them — R was likely upgraded
without migrating the library:

  furrr, future     script 5 falls back to sequential (~12 min) without them
  survey            not used here, but the other Pillar 3 pipelines need it
  DHS.rates         needed for the chmort cross-check in script 8 section 3
  labelled          used by the other Pillar 3 pipelines


Two things to confirm before trusting the output
------------------------------------------------

Both are marked CONFIRM in the code.

  - IPUMS-DHS KIDALIVE and KIDAGEDIEDIMP sentinel conventions (script 2). The
    guards are deliberately loose; check them against the codebook once the
    extract lands.
  - IPUMS-MICS birth-history variable names (script 3). Adjust the rename map at
    the top of that script rather than the code below it.

The right-censoring rule is the easiest place to corrupt this silently: died_u1
must be NA, not 0, for children born under 12 months before interview. Script 8
checks for it two ways — against DHS-published IMR, and against Wagner's own
Table 2 descriptives.
