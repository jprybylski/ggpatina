#' Old educational film preset
#'
#' Adds a warm/green cast, heavy grain, dust, scratches, and slight weave for a
#' classroom 16mm look.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_blank(200, 200, "white")
#'   patina_edu_film(img)
#' }
#' }
#' @param img magick image
#' @param warmth 0..1 warm/green cast (0=none)
#' @param grain 0..1 coarse film grain
#' @param vignette 0..1 edge darkening
#' @param jitter small rotation in degrees to mimic gate weave
#' @param dust 0..1 dust amount scaler (0 = none)
#' @param dust_n integer, override number of dust specks (NULL = auto by size × dust)
#' @param dust_size_px length-2 numeric, min/max speck radius in pixels
#' @param dust_polarity one of "white","black","mixed"
#' @param dust_mix 0..1 fraction of black specks when dust_polarity="mixed"
#' @param seed optional integer to make dust/scratches reproducible
#' @param scratches integer number of vertical scratches to draw (0 = none)
#' @return A `magick-image` with an educational film look.
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
  dust_polarity = c("white","black","mixed"),
  dust_mix = 0.5,
  seed = NULL,
  scratches = 6L
) {
  stopifnot(inherits(img, "magick-image"))
  dust_polarity <- match.arg(dust_polarity)
  info <- magick::image_info(img)[1, ]; w <- info$width; h <- info$height

  if (!is.null(seed)) {
    if (requireNamespace("withr", quietly = TRUE)) withr::local_seed(seed) else set.seed(seed)
  }

  out <- img

  # warm/green cast
  if (warmth > 0) {
    out <- magick::image_colorize(out, opacity = round(100 * warmth * 0.30), color = "#d2c69a")
    out <- magick::image_colorize(out, opacity = round(100 * warmth * 0.18), color = "#7fa37a")
  }

  # coarse grain
  if (grain > 0) {
    base  <- magick::image_blank(w, h, "gray50")
    n     <- magick::image_noise(base, "gaussian")
    n     <- magick::image_blur(n, 0, 1.1)
    n     <- set_alpha(n, grain * 0.7)
    out   <- magick::image_composite(out, n, operator = "soft_light")
  }

  # ----- discrete dust specks (white, black, or mixed) -----------------------
  if (dust > 0) {
    if (is.null(dust_n)) dust_n <- max(0L, round(0.00004 * w * h * dust))

    draw_specks <- function(color = c("white","black"), n = dust_n) {
      color <- match.arg(color)
      if (n <= 0) return(NULL)
      lay <- magick::image_blank(w, h, "transparent")
      dev <- magick::image_draw(lay)
      oldpar <- graphics::par(xaxs = "i", yaxs = "i", mar = c(0,0,0,0))
      on.exit(graphics::par(oldpar), add = TRUE)

      # circular specks
      n_c <- round(n * 0.85)
      if (n_c > 0) {
        x <- runif(n_c, 0, w); y <- runif(n_c, 0, h)
        r <- runif(n_c, dust_size_px[1], dust_size_px[2])
        a <- if (color == "white") runif(n_c, 0.15, 0.50) else runif(n_c, 0.12, 0.45)
        col <- if (color == "white") grDevices::rgb(1,1,1, a) else grDevices::rgb(0,0,0, a)
        graphics::symbols(x, y, circles = r, inches = FALSE, bg = col, fg = NA, add = TRUE)
      }

      # hair-like flecks
      n_h <- max(0L, n - n_c)
      if (n_h > 0) {
        x0 <- runif(n_h, 0, w); y0 <- runif(n_h, 0, h)
        len <- runif(n_h, 4, 14); ang <- runif(n_h, 0, 2*pi)
        x1 <- x0 + len * cos(ang); y1 <- y0 + len * sin(ang)
        a  <- if (color == "white") runif(n_h, 0.08, 0.25) else runif(n_h, 0.10, 0.30)
        col<- if (color == "white") grDevices::rgb(1,1,1, a) else grDevices::rgb(0,0,0, a)
        lwd<- runif(n_h, 0.4, 1.1)
        graphics::segments(x0, y0, x1, y1, col = col, lwd = lwd)
      }
      grDevices::dev.off()
      magick::image_blur(dev, 0, 0.5)
    }

    if (dust_polarity == "white") {
      lay_w <- draw_specks("white", dust_n)
      if (!is.null(lay_w)) out <- magick::image_composite(out, lay_w, operator = "screen")
    } else if (dust_polarity == "black") {
      lay_b <- draw_specks("black", dust_n)
      if (!is.null(lay_b)) out <- magick::image_composite(out, lay_b, operator = "multiply")
    } else { # mixed
      nb <- round(dust_n * dust_mix); nw <- dust_n - nb
      lay_w <- draw_specks("white", nw)
      lay_b <- draw_specks("black", nb)
      if (!is.null(lay_w)) out <- magick::image_composite(out, lay_w, operator = "screen")
      if (!is.null(lay_b)) out <- magick::image_composite(out, lay_b, operator = "multiply")
    }
  }

  # scratches (respects seed)
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

  # gate weave
  if (!isTRUE(all.equal(jitter, 0))) {
    out <- magick::image_rotate(out, jitter)
    out <- magick::image_background(out, "black", flatten = TRUE)
    out <- magick::image_crop(out, sprintf("%dx%d+0+0", w, h))
  }

  magick::image_blur(out, 0, 0.4)
}


#' Animate an educational-film segment
#'
#' Builds an animated GIF of a short 16mm instructional clip with gate weave,
#' flicker, and per-frame dust.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_blank(100, 100, "white")
#'   animate_edu_film(img, n_frames = 5, fps = 2)
#' }
#' }
#'
#' @param img magick image (your rasterized ggplot)
#' @param n_frames number of frames (e.g., 48)
#' @param fps frames per second (e.g., 10)
#' @param weave_deg max rotation (deg) for gate weave (per-frame)
#' @param jitter_xy max XY translation in pixels (per-frame)
#' @param flicker amplitude of brightness flicker (0..0.2 ~ gentle)
#' @param loop 0 = loop forever (GIF), otherwise number of loops
#' @param bg background color used when translating/rotating (usually "black")
#' @param seed optional integer; if set, dust/scratches vary per-frame deterministically
#' @param write_gif optional file path; if provided, writes the GIF and also returns it
#' @param max_dust if not using a fixed `dust` in `...`, generate random `dust` form 0...`max_dust`
#' @param ... passed to `patina_edu_film()` (e.g., warmth, grain, dust_polarity, dust_n, scratches)
#' @return an animated magick-image (GIF). If `write_gif` is provided, the file is also written.
#' @export
animate_edu_film <- function(img,
                             n_frames = 48,
                             fps = 10,
                             weave_deg = 0.1,
                             jitter_xy = 0.1,
                             flicker = 0.06,
                             loop = 0,
                             bg = "black",
                             seed = NULL,
                             write_gif = NULL,
                             max_dust = 0.3,
                             ...) {
  stopifnot(inherits(img, "magick-image"))
  info <- magick::image_info(img)[1, ]
  w <- info$width; h <- info$height

  # small helpers scoped here (avoid importing rlang)
  clamp01 <- function(x) pmax(0, pmin(1, x))

  frames <- vector("list", n_frames)
  # tiny phase offsets so motion isn't perfectly periodic
  phi_w <- runif(1, 0, 2*pi)
  phi_f <- runif(1, 0, 2*pi)

  for (i in seq_len(n_frames)) {
    # per-frame gate weave: sine + a little randomness
    rot <- weave_deg * sin(2*pi * i / 18 + phi_w) + rnorm(1, 0, weave_deg/6)
    dx  <- round(rnorm(1, 0, jitter_xy))
    dy  <- round(rnorm(1, 0, jitter_xy))

    # brightness flicker (kept subtle)
    flick <- flicker * (0.7 * sin(2*pi * i / 12 + phi_f) + 0.3 * rnorm(1, 0, 0.5))
    br    <- round(100 * (1 + clamp01(flick)))  # 100 +/- a few percent

    # per-frame seed so dust/scratches move; respects global set.seed if seed is NULL
    frame_seed <- if (is.null(seed)) NULL else as.integer(seed) + i - 1

    # Random dust
    this_dust <- runif(1, 0, max_dust)

    # 1) apply your film patina with per-frame jitter+seed
    fr <- rlang::exec(
      patina_edu_film,
      img,
      jitter = rot,
      seed   = frame_seed,
      !!!modifyList(
        list(
          dust = this_dust
        ),
        rlang::list2(...)
      )
    )

    # 2) apply flicker
    if (flicker > 0) {
      fr <- magick::image_modulate(fr, brightness = br)
    }

    # 3) tiny XY weave (translate but keep size)
    if (jitter_xy > 0) {
      pad <- max(2L, abs(dx), abs(dy)) + 2L
      ext <- magick::image_extent(
        fr, sprintf("%dx%d", w + 2*pad, h + 2*pad),
        gravity = "center", color = bg  # <— use color=bg here
      )
      fr  <- magick::image_crop(ext, sprintf("%dx%d+%d+%d", w, h, pad + dx, pad + dy))
    }


    frames[[i]] <- fr
  }

  anim <- magick::image_animate(magick::image_join(frames), fps = fps, loop = loop)

  if (!is.null(write_gif)) {
    magick::image_write(anim, path = write_gif)
  }
  anim
}
