/*
 * Project: SOWC 2026
 * Goal: Extract annual night light luminosity at each survey cluster
 * Author: Rouslan Karimov
 * Organization: UNICEF
 * Created: 31 July 2026
 *
 * Run this in the Google Earth Engine Code Editor (https://code.earthengine.google.com).
 * It exports one row per cluster-year to Google Drive; download the CSV to
 * data_derived/mortality_nightlights_gee.csv, which script 6 then reads.
 *
 * BEFORE RUNNING — upload the cluster points as a GEE table asset:
 *   1. In R, after script 4 has run:
 *        clusters <- readRDS(here("data_derived", "mortality_clusters.rds"))
 *        clusters |>
 *          sf::st_drop_geometry() |>
 *          dplyr::select(cluster_uid, gpslat, gpslong) |>
 *          readr::write_csv(here("data_derived", "mortality_clusters_for_gee.csv"))
 *   2. In GEE: Assets > New > CSV file, with gpslong as x and gpslat as y.
 *   3. Put the resulting asset path in CLUSTERS below.
 *
 * Data: harmonized DMSP-VIIRS annual night lights (Li et al. 2020, Scientific
 * Data), served by the awesome-gee-community-catalog. DMSP covers 1992-2013 and
 * the DMSP-like simulated VIIRS product continues it to 2021, on a consistent
 * scale — which is the whole point of using the harmonized series rather than
 * splicing raw DMSP to raw VIIRS.
 */

var CLUSTERS = 'users/YOUR_USERNAME/mortality_clusters_for_gee';  // <- edit
var START_YEAR = 1995;
var END_YEAR   = 2021;   // harmonized series currently ends 2021 in this catalog

var clusters = ee.FeatureCollection(CLUSTERS);

var dmsp  = ee.ImageCollection('projects/sat-io/open-datasets/Harmonized_NTL/dmsp');
var viirs = ee.ImageCollection('projects/sat-io/open-datasets/Harmonized_NTL/viirs');
var ntl   = dmsp.merge(viirs);

/*
 * Sampled as the mean over a 5 km buffer rather than the value at the exact
 * point. DHS displaces cluster coordinates by up to 2 km in urban areas and
 * 5 km in rural ones, so the recorded point is not where the households are;
 * a buffer that matches the displacement radius is closer to the truth than
 * a single ~1 km pixel.
 */
var buffered = clusters.map(function (f) {
  return f.buffer(5000);
});

var years = ee.List.sequence(START_YEAR, END_YEAR);

var rows = ee.FeatureCollection(years.map(function (y) {
  y = ee.Number(y);

  var img = ntl
    .filter(ee.Filter.calendarRange(y, y, 'year'))
    .mean()
    .rename('ntl');

  return img.reduceRegions({
    collection: buffered,
    reducer: ee.Reducer.mean(),
    scale: 1000
  }).map(function (f) {
    return ee.Feature(null, {
      cluster_uid: f.get('cluster_uid'),
      year: y,
      ntl: f.get('mean')
    });
  });
})).flatten();

Export.table.toDrive({
  collection: rows,
  description: 'mortality_nightlights_gee',
  fileFormat: 'CSV',
  selectors: ['cluster_uid', 'year', 'ntl']
});
