#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List svf_raycast_cpp(NumericMatrix coords, NumericVector obs_values, 
                     NumericVector observer_z, NumericVector directions_deg, 
                     NumericVector distances, double xmin, double ymax, 
                     double xres, double yres, int ncol, int nrow, 
                     bool return_raw_angles) {
  int n_points = coords.nrow();
  int n_directions = directions_deg.size();
  int n_steps = distances.size();
  
  NumericVector svf_vec(n_points);
  NumericVector mean_horizon(n_points);
  NumericVector max_horizon(n_points);
  
  NumericMatrix horizon_mat;
  if (return_raw_angles) {
    horizon_mat = NumericMatrix(n_points, n_directions);
  }
  
  // Precalculate dx and dy offsets
  std::vector<double> dx(n_directions * n_steps);
  std::vector<double> dy(n_directions * n_steps);
  for (int j = 0; j < n_directions; ++j) {
    double theta = directions_deg[j] * M_PI / 180.0;
    double cos_t = std::cos(theta);
    double sin_t = std::sin(theta);
    for (int k = 0; k < n_steps; ++k) {
      dx[j * n_steps + k] = cos_t * distances[k];
      dy[j * n_steps + k] = sin_t * distances[k];
    }
  }
  
  for (int i = 0; i < n_points; ++i) {
    double x0 = coords(i, 0);
    double y0 = coords(i, 1);
    double obs_z = observer_z[i];
    
    double sum_cos2 = 0.0;
    double sum_horizon = 0.0;
    double max_h = 0.0;
    
    for (int j = 0; j < n_directions; ++j) {
      double max_ang = 0.0;
      int idx_base = j * n_steps;
      
      for (int k = 0; k < n_steps; ++k) {
        double x = x0 + dx[idx_base + k];
        double y = y0 + dy[idx_base + k];
        
        int col = std::floor((x - xmin) / xres);
        int row = std::floor((ymax - y) / yres);
        
        if (col >= 0 && col < ncol && row >= 0 && row < nrow) {
          int cell_idx = row * ncol + col;
          double val = obs_values[cell_idx];
          
          if (!NumericVector::is_na(val)) {
            double ang = std::atan2(val - obs_z, distances[k]);
            if (ang > max_ang) {
              max_ang = ang;
            }
          }
        }
      }
      
      sum_cos2 += std::cos(max_ang) * std::cos(max_ang);
      sum_horizon += max_ang;
      if (max_ang > max_h) {
        max_h = max_ang;
      }
      
      if (return_raw_angles) {
        horizon_mat(i, j) = max_ang;
      }
    }
    
    svf_vec[i] = sum_cos2 / n_directions;
    mean_horizon[i] = (sum_horizon / n_directions) * 180.0 / M_PI;
    max_horizon[i] = max_h * 180.0 / M_PI;
  }
  
  if (return_raw_angles) {
    return List::create(
      Named("svf") = svf_vec,
      Named("mean_horizon") = mean_horizon,
      Named("max_horizon") = max_horizon,
      Named("horizon_mat") = horizon_mat
    );
  } else {
    return List::create(
      Named("svf") = svf_vec,
      Named("mean_horizon") = mean_horizon,
      Named("max_horizon") = max_horizon
    );
  }
}
