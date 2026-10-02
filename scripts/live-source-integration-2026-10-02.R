# Reproducible live source smoke test. This is not field validation.
# Run from the greenR package root with network access:
#   Rscript scripts/live-source-integration-2026-10-02.R /tmp/greenR-source-audit
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else file.path(tempdir(), "greenR-source-audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)

boundary <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(
  xmin = 8.53, ymin = 47.37, xmax = 8.531, ymax = 47.371), crs = 4326)))
period <- "2025-06-01/2025-08-31"
cache <- file.path(out_dir, "cache")
ndvi <- greenR:::.uh_satellite_mosaic(boundary, period, cache, "ndvi")
lst <- greenR:::.uh_satellite_mosaic(boundary, period, cache, "lst")
chm <- greenR:::.fetch_meta_chm(boundary, cache, use_cache = FALSE)
gba <- greenR:::.fetch_gba_buildings(boundary, cache, use_cache = FALSE)

terra::writeRaster(chm, file.path(out_dir, "zurich_chm.tif"), overwrite = TRUE)
sf::st_write(gba, file.path(out_dir, "zurich_buildings.gpkg"), quiet = TRUE,
             delete_dsn = TRUE)
ndvi_meta <- ndvi[setdiff(names(ndvi), "raster")]
lst_meta <- lst[setdiff(names(lst), "raster")]
ndvi_meta$source_index_path <- file.path("cache", "ndvi", basename(ndvi_meta$source_index_path))
lst_meta$source_index_path <- file.path("cache", "lst", basename(lst_meta$source_index_path))
metrics <- list(
  timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  bbox_wgs84 = unname(as.numeric(sf::st_bbox(boundary))),
  datetime_query = period,
  ndvi = ndvi_meta,
  lst = lst_meta,
  chm = list(valid_pixels = sum(is.finite(terra::values(chm))),
             max_height_m = max(terra::values(chm), na.rm = TRUE)),
  gba = list(building_count = nrow(gba), sources = unique(gba$source %||% "unrecorded")))
jsonlite::write_json(metrics, file.path(out_dir, "live_source_metrics.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(jsonlite::toJSON(metrics, auto_unbox = TRUE, pretty = TRUE), "\n")
