#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// Horizon ray casting over an absolute-height obstruction raster.
//
// Conventions
//   * directions_deg are compass azimuths: 0 = north, 90 = east, clockwise.
//     A ray at azimuth a advances by (dx, dy) = (sin a, cos a) * distance.
//   * obs_values is the raster in row-major order (row 0 = top / ymax).
//   * Rays are traversed cell by cell (exact DDA) up to max(distances); cells that are
//     NA or lie outside the raster stop the ray; if this
//     happens before the last distance the direction is counted as truncated,
//     because the horizon beyond that point is unknown (it is NOT sky).
//   * The horizon elevation angle per direction is max(0, atan(max tan)), so a
//     terrain drop below the observer never counts as extra sky beyond the
//     horizontal plane (horizontal-surface sky-view factor).
//   * SVF = mean(cos^2(horizon)) over directions (Johnson & Watson 1984; Oke 1987).
//   * If the observer's own cell is higher than the observer, the observer is
//     inside an obstacle (e.g. a sample point on a building footprint) and SVF
//     is returned as NA together with a flag.
//
// [[Rcpp::export]]
List svf_raycast_cpp(NumericMatrix coords, NumericVector obs_values,
                     NumericVector observer_z, NumericVector directions_deg,
                     NumericVector distances, double xmin, double ymax,
                     double xres, double yres, int ncol, int nrow,
                     bool return_raw_angles) {
  const int n_points = coords.nrow();
  const int n_dir = directions_deg.size();
  const double max_dist = distances.size() ? distances[distances.size() - 1] : 0.0;
  const double deg = M_PI / 180.0;

  NumericVector svf_vec(n_points), mean_horizon(n_points), max_horizon(n_points), truncated_share(n_points);
  LogicalVector inside(n_points);
  NumericMatrix horizon_mat = return_raw_angles ? NumericMatrix(n_points, n_dir) : NumericMatrix(0, 0);

  std::vector<double> ux(n_dir), uy(n_dir);
  for (int j = 0; j < n_dir; ++j) {
    const double a = directions_deg[j] * deg;
    ux[j] = std::sin(a);
    uy[j] = std::cos(a);
  }

  auto cell_value = [&](double x, double y, bool &ok) -> double {
    const double fc = std::floor((x - xmin) / xres);
    const double fr = std::floor((ymax - y) / yres);
    if (fc < 0 || fr < 0 || fc >= ncol || fr >= nrow) { ok = false; return NA_REAL; }
    const double v = obs_values[(long)fr * ncol + (long)fc];
    ok = !ISNAN(v);
    return v;
  };

  for (int i = 0; i < n_points; ++i) {
    Rcpp::checkUserInterrupt();
    const double x0 = coords(i, 0), y0 = coords(i, 1), z0 = observer_z[i];
    if (ISNAN(x0) || ISNAN(y0) || ISNAN(z0)) {
      svf_vec[i] = mean_horizon[i] = max_horizon[i] = truncated_share[i] = NA_REAL;
      inside[i] = NA_LOGICAL;
      if (return_raw_angles) for (int j = 0; j < n_dir; ++j) horizon_mat(i, j) = NA_REAL;
      continue;
    }
    bool ok0 = false;
    const double own = cell_value(x0, y0, ok0);
    inside[i] = ok0 && own > z0;

    double sum_cos2 = 0.0, sum_h = 0.0, max_h = 0.0;
    int n_trunc = 0;
    // own cell indices
    const long c0 = (long)std::floor((x0 - xmin) / xres);
    const long r0 = (long)std::floor((ymax - y0) / yres);
    for (int j = 0; j < n_dir; ++j) {
      // Exact grid traversal (Amanatides & Woo 1987): every cell crossed by the ray is
      // visited once, and its height is seen at the distance where the ray ENTERS it,
      // which is the steepest view of a flat-topped cell. No step size is involved.
      double max_tan = 0.0;
      bool truncated = false;
      const double dx = ux[j], dy = uy[j];
      long col = c0, row = r0;
      const int step_c = dx > 0 ? 1 : -1;
      const int step_r = dy > 0 ? -1 : 1;           // rows count downwards
      double t_max_c, t_max_r, t_delta_c, t_delta_r;
      if (std::fabs(dx) < 1e-12) { t_max_c = INFINITY; t_delta_c = INFINITY; }
      else {
        const double bx = xmin + (dx > 0 ? (col + 1) : col) * xres;
        t_max_c = (bx - x0) / dx; t_delta_c = xres / std::fabs(dx);
      }
      if (std::fabs(dy) < 1e-12) { t_max_r = INFINITY; t_delta_r = INFINITY; }
      else {
        const double by = ymax - (dy > 0 ? row : (row + 1)) * yres;
        t_max_r = (by - y0) / dy; t_delta_r = yres / std::fabs(dy);
      }
      while (true) {
        double t;
        if (t_max_c < t_max_r) { t = t_max_c; col += step_c; t_max_c += t_delta_c; }
        else { t = t_max_r; row += step_r; t_max_r += t_delta_r; }
        if (t > max_dist) break;
        if (col < 0 || row < 0 || col >= ncol || row >= nrow) { truncated = true; break; }
        const double v = obs_values[row * ncol + col];
        if (ISNAN(v)) { truncated = true; break; }
        const double tt = (v - z0) / std::max(t, 1e-9);
        if (tt > max_tan) max_tan = tt;
      }
      if (truncated) ++n_trunc;
      const double h = std::atan(max_tan);
      const double c = std::cos(h);
      sum_cos2 += c * c;
      sum_h += h;
      if (h > max_h) max_h = h;
      if (return_raw_angles) horizon_mat(i, j) = h;
    }
    truncated_share[i] = (double)n_trunc / n_dir;
    if (inside[i] == TRUE) {
      svf_vec[i] = mean_horizon[i] = max_horizon[i] = NA_REAL;
    } else {
      svf_vec[i] = sum_cos2 / n_dir;
      mean_horizon[i] = sum_h / n_dir / deg;
      max_horizon[i] = max_h / deg;
    }
  }

  List out = List::create(
    Named("svf") = svf_vec,
    Named("mean_horizon") = mean_horizon,
    Named("max_horizon") = max_horizon,
    Named("truncated_share") = truncated_share,
    Named("inside_obstacle") = inside);
  if (return_raw_angles) out["horizon_mat"] = horizon_mat;
  return out;
}
