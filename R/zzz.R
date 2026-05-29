
#' @useDynLib greenR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom tidyterra geom_spatraster
#' @importFrom stats ave rnorm runif
NULL

utils::globalVariables(
  c(
    "height", "..count..", "distance", "Threshold", "Coverage", "Type", "direction",
    "mean_coverage", "highway", "h3_index", "L",
    "action_class", "area", "area_m2", "area_pct", "bar_label", "bbox",
    "cooling_deficit_score", "heat_exposure_score", "hex_id", "id", "len",
    "lst_mean_c", "mean_horizon_deg", "need_high", "opportunity_bin",
    "opportunity_high", "plantability_score", "planting_opportunity_score",
    "priority_score", "quadrant", "street_id", "svf", "svf_mean",
    "tree_need_score", "x", "y"
  )
)
