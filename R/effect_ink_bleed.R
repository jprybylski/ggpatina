#' Ink bleed effect
#'
#' Simulates ink bleeding into paper fibers.
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_read("logo:")
#'   patina_ink_bleed(img)
#' }
#'
#' @param img magick image (from as_magick)
#' @param radius feather (sigma) in px for the bleed falloff
#' @param expand dilation (px) to push the bleed outward from strokes
#' @param strength 0..1 overall opacity of the bleed layer
#' @param bleed_color bleed/tint color
#' @param paper_texture optional texture laid UNDER the plot
#' @export
patina_ink_bleed <- function(img,
                             radius = 2, expand = 1,
                             strength = 0.5,
                             bleed_color = "#2a2a2a",
                             paper_texture = NULL) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]

  # 1) Mask of dark ink (white where strokes are)
  g     <- magick::image_convert(img, colorspace = "gray")
  ink   <- magick::image_negate(g)                          # dark -> bright
  ink   <- magick::image_threshold(ink, type = "white", threshold = "55%")

  # 2) Push outward + feather
  if (expand > 0) ink <- magick::image_morphology(ink, "Dilate", kernel = paste0("Disk:", expand))
  halo <- magick::image_blur(ink, radius = 0, sigma = radius)

  # 3) Colored bleed layer with halo as its alpha
  bleed <- magick::image_blank(w, h, bleed_color)
  bleed <- magick::image_composite(bleed, halo, operator = "copy_opacity")
  # scale alpha by 'strength'
  bleed <- magick::image_fx(bleed, expression = sprintf("u*%f", strength), channel = "alpha")

  # 4) Optional paper underlay (very subtle) then bleed OVER the plot
  out <- img
  if (!is.null(paper_texture)) {
    tex <- magick::image_read(paper_texture)
    tex <- magick::image_resize(tex, paste0(w, "x", h, "!"))
    tex <- magick::image_modulate(tex, brightness = 102, saturation = 100)
    tex <- magick::image_fx(tex, "u*0.25", channel = "alpha")  # ~25% strength
    out <- magick::image_composite(tex, out, operator = "multiply")
  }

  # Multiply makes dark, slightly colored halos around strokes
  out <- magick::image_composite(out, bleed, operator = "multiply")
  magick::image_blur(out, radius = 0, sigma = 0.3)
}
