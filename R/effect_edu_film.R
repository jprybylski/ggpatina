#' Old educational film preset (warm/green cast, big grain, dust & scratches)
#' @param img magick image
#' @param warmth 0..1 warm/green cast (0=none)
#' @param grain 0..1 coarse film grain
#' @param vignette 0..1 edge darkening
#' @param jitter small rotation in degrees to mimic gate weave
#' @param dust 0..1 dust amount scaler (0 = none)
#' @param dust_n integer, override number of dust specks (NULL = auto by size × dust)
#' @param dust_size_px length-2 numeric, min/max speck radius in pixels
#' @param seed optional integer; if given, seeds RNG for reproducible dust/scratches
#' @param scratches integer number of vertical scratches to draw (0 = none)
#' @export
patina_edu_film <- function(
  img,
  warmth = 0.35,
  grain = 0.45,
  vignette = 0.22,
  jitter = 0.4,
  dust = 0.25,
  dust_n = NULL,
  dust_size_px = c(0.6, 2.2),
  seed = NULL,
  scratches = 6L
) {
  stopifnot(inherits(img, "magick-image"))
  info <- magick::image_info(img)[1, ]; w <- info$width; h <- info$height

  # optional reproducibility
  if (!is.null(seed)) {
    if (requireNamespace("withr", quietly = TRUE)) {
      withr::local_seed(seed)
    } else {
      set.seed(seed)
    }
  }

  out <- img

  # warm/green cast
  if (warmth > 0) {
    out <- magick::image_colorize(out, opacity = round(100 * warmth * 0.30), color = "#d2c69a")
    out <- magick::image_colorize(out, opacity = round(100 * warmth * 0.18), color = "#7fa37a")
  }

  # coarse grain (soft-light)
  if (grain > 0) {
    base  <- magick::image_blank(w, h, "gray50")
    n     <- magick::image_noise(base, "gaussian")
    n     <- magick::image_blur(n, 0, 1.1)
    n     <- set_alpha(n, grain * 0.7)
    out   <- magick::image_composite(out, n, operator = "soft_light")
  }

  # ---- DISCRETE DUST SPECKS -------------------------------------------------
  if (dust > 0) {
    # default count scales with pixels; tuned so dust=0.25 ~ a few dozen specks at 1200x800
    if (is.null(dust_n)) {
      dust_n <- max(0L, round(0.00004 * w * h * dust))
    }
    if (dust_n > 0) {
      spex <- magick::image_blank(w, h, "transparent")
      dev  <- magick::image_draw(spex)
      oldpar <- graphics::par(xaxs = "i", yaxs = "i", mar = c(0,0,0,0))
      on.exit(graphics::par(oldpar), add = TRUE)

      # circular specks
      n_circ <- round(dust_n * 0.85)
      if (n_circ > 0) {
        x <- runif(n_circ, 0, w); y <- runif(n_circ, 0, h)
        r <- runif(n_circ, dust_size_px[1], dust_size_px[2])
        a <- runif(n_circ, 0.15, 0.50)
        col <- grDevices::rgb(1, 1, 1, alpha = a)
        graphics::symbols(x, y, circles = r, inches = FALSE, bg = col, fg = NA, add = TRUE)
      }

      # a few hair-like flecks (thin short segments)
      n_hair <- max(0L, dust_n - n_circ)
      if (n_hair > 0) {
        x0 <- runif(n_hair, 0, w); y0 <- runif(n_hair, 0, h)
        len <- runif(n_hair, 4, 14)
        ang <- runif(n_hair, 0, 2*pi)
        x1 <- x0 + len * cos(ang); y1 <- y0 + len * sin(ang)
        col <- grDevices::rgb(1,1,1, alpha = runif(n_hair, 0.08, 0.25))
        lwd <- runif(n_hair, 0.4, 1.1)
        graphics::segments(x0, y0, x1, y1, col = col, lwd = lwd)
      }

      grDevices::dev.off()
      spex <- magick::image_blur(dev, 0, 0.5)
      out  <- magick::image_composite(out, spex, operator = "screen")
    }
  }

  # scratches
  if (scratches > 0) {
    scr <- magick::image_blank(w, h, "transparent")
    dev <- magick::image_draw(scr)
    for (i in seq_len(scratches)) {
      x   <- sample.int(w, 1)
      col <- grDevices::rgb(1, 1, 1, alpha = runif(1, 0.10, 0.22))
      lwd <- runif(1, 0.6, 1.6)
      graphics::segments(x0 = x, y0 = 0, x1 = x + rnorm(1, 0, 1.6), y1 = h,
                         col = col, lwd = lwd)
    }
    grDevices::dev.off()
    scr <- magick::image_blur(dev, 0, 0.5)
    out <- magick::image_composite(out, scr, operator = "screen")
  }

  # vignette
  if (vignette > 0) {
    vig <- magick::image_read("radial-gradient:rgba(0,0,0,0)-black")
    vig <- magick::image_resize(vig, sprintf("%dx%d!", w, h))
    vig <- set_alpha(vig, vignette)
    out <- magick::image_composite(out, vig, operator = "multiply")
  }

  # gate weave (small jitter)
  if (!isTRUE(all.equal(jitter, 0))) {
    out <- magick::image_rotate(out, jitter)
    out <- magick::image_background(out, "black", flatten = TRUE)
    out <- magick::image_crop(out, sprintf("%dx%d+0+0", w, h))
  }

  # slight soften
  magick::image_blur(out, 0, 0.4)
}
