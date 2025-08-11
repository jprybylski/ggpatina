
#' @keywords internal
#' @noRd
._ggpatina_pkg <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
img_wh <- function(img) {
  info <- magick::image_info(img)[1, ]
  c(w = info$width, h = info$height)
}

#' Scale an image's alpha channel (0..1)
#'
#' @keywords internal
#' @noRd
set_alpha <- function(img, alpha = 1) {
  alpha <- max(0, min(1, alpha))
  magick::image_fx(img, expression = sprintf("u*%f", alpha), channel = "alpha")
}
