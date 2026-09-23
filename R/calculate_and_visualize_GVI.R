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
                                  verbose = FALSE)

  segmented_image <- spx$AP_image_data
  dims <- dim(segmented_image)
  if (length(dims) != 3L || dims[3] < 3L || any(dims[1:2] == 0L))
    stop("Segmentation did not return a nonempty RGB image.", call. = FALSE)
  red <- segmented_image[, , 1]
  green <- segmented_image[, , 2]
  blue <- segmented_image[, , 3]
  green_mask <- is.finite(red) & is.finite(green) & is.finite(blue) &
    green > 0.2 & green > red & green > blue
  GVI <- mean(green_mask)
  message(paste("Green View Index: ", GVI))

  visualized_image <- array(0, dim = c(dims[1], dims[2], 3))
  visualized_image[, , 2] <- as.numeric(green_mask)

  return(list(GVI = GVI, segmented_image = spx$AP_image_data, green_pixels_image = visualized_image))
}
