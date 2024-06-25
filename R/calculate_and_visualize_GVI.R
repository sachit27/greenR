#' Calculate and Visualize Green View Index (GVI) from an image
#'
#' This function reads an image, performs superpixel segmentation (using the SuperpixelImageSegmentation library), calculates the Green View Index (GVI),
#' and returns a list containing the segmented image, the green pixels image, and the calculated GVI.
#'
#' @param image_path The path of the image file to be processed.
#' @return A list containing the Green View Index (GVI), the segmented image, and the green pixels image.
#' @importFrom OpenImageR readImage
#' @importFrom SuperpixelImageSegmentation Image_Segmentation
#' @examples
#' \dontrun{
#' # Example usage with an image located at the specified path
#' result <- calculate_and_visualize_GVI("/path/to/your/image.png")
#' }
#' @export
calculate_and_visualize_GVI <- function(image_path) {

  # Read the image
  image <- OpenImageR::readImage(image_path)

  # Initialize Image Segmentation
  init <- SuperpixelImageSegmentation::Image_Segmentation$new()

  # Perform Superpixel Segmentation
  spx <- init$spixel_segmentation(input_image = image,
                                  superpixel = 600,
                                  AP_data = TRUE,
                                  use_median = TRUE,
                                  sim_wL = 3,
                                  sim_wA = 10,
                                  sim_wB = 10,
                                  sim_color_radius = 10,
                                  verbose = TRUE)

  # Internal Function to Check if Pixel is Green
  is_green_pixel <- function(R, G, B) {
    if (G > 0.2 && G > R && G > B) {
      return(TRUE)
    }
    return(FALSE)
  }

  # Calculate Green View Index (GVI)
  segmented_image <- spx$AP_image_data
  total_pixels <- 0
  green_pixels <- 0

  for (i in 1:dim(segmented_image)[1]) {
    for (j in 1:dim(segmented_image)[2]) {
      total_pixels <- total_pixels + 1
      R <- segmented_image[i, j, 1]
      G <- segmented_image[i, j, 2]
      B <- segmented_image[i, j, 3]

      if (is_green_pixel(R, G, B)) {
        green_pixels <- green_pixels + 1
      }
    }
  }

  GVI <- green_pixels / total_pixels
  message(paste("Green View Index: ", GVI))

  # Visualize Green Pixels
  visualized_image <- array(0, dim = c(dim(segmented_image)[1], dim(segmented_image)[2], 3))

  for (i in 1:dim(segmented_image)[1]) {
    for (j in 1:dim(segmented_image)[2]) {
      R <- segmented_image[i, j, 1]
      G <- segmented_image[i, j, 2]
      B <- segmented_image[i, j, 3]

      ExG <- (G - R) + (G - B)

      if (G < 0.9 && R < 0.6 && B < 0.6 && ExG > 0.05) {
        visualized_image[i, j, ] <- c(0, 1, 0)
      } else {
        visualized_image[i, j, ] <- c(0, 0, 0)
      }
    }
  }

  return(list(GVI = GVI, segmented_image = spx$AP_image_data, green_pixels_image = visualized_image))
}
