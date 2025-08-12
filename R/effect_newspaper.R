#' Old newspaper effect
#'
#' Creates a light newsprint look with crisp ink, subtle dots, and optional
#' paper texture.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_blank(200, 200, "white")
#'   patina_newspaper(img)
#' }
#' }
#'
#' @param img magick image (rasterized plot)
#' @param halftone 0..1 strength of dot pattern (applied to background only)
#' @param dot_map dithering map (e.g. "h6x6o","h4x4a")
#' @param ink_spread px radius of outward bleed around dark strokes
#' @param ink_strength 0..1 strength of bleed halo
#' @param paper optional path/URL to paper texture; if NULL a tint is used
#' @param paper_tint hex color of newsprint
#' @param paper_blend 0..1 blend of paper texture/tint
#' @param misregister_px integer pixel C/M plate offset (0 = off)
#' @param exposure overall brightness multiplier (~0.95–1.15)
#' @param highlight_lift 0..1 brighten only very light backgrounds
#' @param line_gain 0..1 deepen only the ink strokes after effects
#' @export
patina_newspaper <- function(
  img,
  halftone = 0.25,
  dot_map = "h6x6o",
  ink_spread = 1.0,
  ink_strength = 0.22,
  paper = NULL,
  paper_tint = "#eee8d6",
  paper_blend = 0.20,
  misregister_px = 0L,
  exposure = 1.08,
  highlight_lift = 0.22,
  line_gain = 0.22
) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]

  # Base: grayscale + gentle paper tint (keeps page light)
  g    <- magick::image_convert(img, colorspace = "gray")
  base <- magick::image_colorize(g, opacity = 14, color = paper_tint)

  # Masks: ink (bright on strokes) and background (bright on paper)
  ink <- magick::image_negate(g)
  ink <- magick::image_threshold(ink, type = "white", threshold = "50%")
  ink <- magick::image_blur(ink, 0, 1.0)
  bg  <- magick::image_negate(ink)

  # Halftone dots only in background (so lines/text stay clean)
  out <- base
  if (halftone > 0) {
    d <- magick::image_ordered_dither(g, dot_map)
    # limit halftone to paper: copy alpha from bg * halftone
    a <- magick::image_fx(bg, sprintf("u*%f", halftone), channel = "alpha")
    d <- magick::image_composite(d, a, operator = "copy_opacity")
    out <- magick::image_composite(out, d, operator = "soft_light")
  }

  # Ink spread: slight multiply halo around strokes
  if (ink_spread > 0 && ink_strength > 0) {
    halo <- magick::image_morphology(ink, "Dilate", kernel = sprintf("Disk:%g", ink_spread))
    halo <- magick::image_blur(halo, 0, ink_spread)
    halo <- magick::image_colorize(halo, opacity = 100, color = "#1a1a1a")
    halo <- set_alpha(halo, ink_strength)
    out  <- magick::image_composite(out, halo, operator = "multiply")
  }

  # Optional tiny C/M misregistration (overlay so it doesn't darken)
  if (misregister_px > 0) {
    c_t <- magick::image_colorize(out, opacity = 16, color = "#00bcd4")
    m_t <- magick::image_colorize(out, opacity = 16, color = "#ff4081")
    c_t <- .im_roll(c_t,  misregister_px, 0)
    m_t <- .im_roll(m_t, -misregister_px, 0)
    out <- magick::image_composite(out, c_t, operator = "overlay")
    out <- magick::image_composite(out, m_t, operator = "overlay")
  }

  # Paper texture/tint (soft and non-darkening by default)
  if (!is.null(paper) || paper_blend > 0) {
    tex <- if (!is.null(paper)) {
      t <- magick::image_read(paper)
      magick::image_resize(t, sprintf("%dx%d!", w, h))
    } else {
      magick::image_blank(w, h, paper_tint)
    }
    tex <- set_alpha(tex, paper_blend)
    out <- magick::image_composite(out, tex, operator = "soft_light")
  }

  # Brighten only very light backgrounds (keeps page from going gray)
  if (highlight_lift > 0) {
    lum   <- magick::image_convert(out, colorspace = "gray")
    mask  <- magick::image_threshold(lum, type = "white", threshold = "86%")
    white <- magick::image_blank(w, h, "white")
    white <- magick::image_composite(white, mask, operator = "copy_opacity")
    white <- set_alpha(white, highlight_lift)
    out   <- magick::image_composite(out, white, operator = "screen")
  }

  # Overall exposure tweak (gentle)
  if (!isTRUE(all.equal(exposure, 1))) {
    out <- magick::image_modulate(out, brightness = round(100 * exposure))
  }

  # Re-ink: deepen strokes only (helps grid/lines pop)
  if (line_gain > 0) {
    a_ink <- magick::image_fx(ink, sprintf("u*%f", line_gain), channel = "alpha")
    black <- magick::image_blank(w, h, "black")
    black <- magick::image_composite(black, a_ink, operator = "copy_opacity")
    out   <- magick::image_composite(out, black, operator = "multiply")
  }

  # Micro soften so dots feel printed
  out <- magick::image_blur(out, 0, 0.3)
  out
}
