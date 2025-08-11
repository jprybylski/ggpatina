#' Old journal scan (sepia/mono, soft dither, paper, rotating top shadow)
#' Shadow is drawn in a top margin first, then the whole page is rotated.
#'
#' @param img magick image
#' @param paper optional path/URL to paper texture
#' @param sepia logical
#' @param dither logical
#' @param dither_strength 0..1 blend amount (softer texture than full dither)
#' @param tilt_deg degrees to rotate the page (small skew)
#' @param rotate_bg background color used when flattening after rotate
#' @param pad_top_px top margin in pixels (NULL = ~2% of height)
#' @param shadow_strength 0..1 peak opacity at the very top of the margin
#' @param saturation If not sepia toned, color saturation
#' @export
scanify_journal <- function(
  img, paper = NULL, sepia = TRUE, dither = TRUE, dither_strength = 0.30,
  tilt_deg = 0.6, rotate_bg = "white", pad_top_px = NULL, shadow_strength = 0.35,
  saturation = 10
) {
  stopifnot(inherits(img, "magick-image"))
  out <- img

  # --- tone ---
  if (isTRUE(sepia)) {
    g   <- magick::image_convert(out, colorspace = "gray")
    out <- magick::image_colorize(g, opacity = 35, color = "#704214")
  } else {
    out <- magick::image_modulate(out, saturation = saturation)
  }
  out <- magick::image_contrast(out)
  out <- magick::image_blur(out, 0, 0.4)

  # --- soft ordered dither (blend, not replace) ---
  if (isTRUE(dither) && dither_strength > 0) {
    ggray <- magick::image_convert(out, colorspace = "gray")
    dith  <- magick::image_ordered_dither(ggray, "h4x4a")
    dith  <- magick::image_fx(dith, sprintf("%f", max(0, min(1, dither_strength))), channel = "alpha")
    out   <- magick::image_composite(out, dith, operator = "soft_light")
  }

  # dims after tone/dither
  info <- magick::image_info(out)[1, ]
  w <- info$width; h <- info$height

  # --- add top margin and draw shadow IN the margin (behind the content) ---
  pad <- if (is.null(pad_top_px)) max(8L, round(0.02 * h)) else as.integer(pad_top_px)
  if (pad > 0) {
    # extend canvas upward; fill with page background to avoid transparency artifacts
    out <- magick::image_extent(out, sprintf("%dx%d", w, h + pad),
                                gravity = "south")

    # optional paper texture (covers whole page including margin)
    if (!is.null(paper)) {
      tex <- magick::image_read(paper)
      tex <- magick::image_resize(tex, sprintf("%dx%d!", w, h + pad))
      out <- magick::image_composite(out, tex, operator = "multiply")
    }

    # vertical alpha gradient (opaque at top -> transparent downward)
    shadow <- magick::image_read("gradient:black-transparent")
    shadow <- magick::image_rotate(shadow, 90)  # make vertical
    shadow <- magick::image_flip(shadow)        # strong at top edge
    shadow <- magick::image_resize(shadow, sprintf("%dx%d!", w, pad))
    shadow <- magick::image_fx(shadow, sprintf("u*%f", max(0, min(1, shadow_strength))), channel = "alpha")

    # composite shadow into the TOP MARGIN (not over the plot)
    out <- magick::image_composite(out, shadow, operator = "over", gravity = "north")
  } else if (!is.null(paper)) {
    # no margin: still let users add paper
    tex <- magick::image_read(paper)
    tex <- magick::image_resize(tex, sprintf("%dx%d!", w, h))
    out <- magick::image_composite(out, tex, operator = "multiply")
  }

  # --- rotate the ENTIRE page (so the shadow rotates with it) and flatten ---
  if (isTRUE(is.finite(tilt_deg)) && tilt_deg != 0) {
    out <- magick::image_rotate(out, tilt_deg)
  }
  out <- magick::image_background(out, rotate_bg, flatten = TRUE)

  out
}




#' @keywords internal
#' @noRd
img_wh <- function(img) {
  info <- magick::image_info(img)[1, ]
  c(w = info$width, h = info$height)
}
#' @keywords internal
#' @noRd
set_alpha <- function(img, alpha = 1) {
  magick::image_fx(img, sprintf("%f", max(0, min(1, alpha))), channel = "alpha")
}
