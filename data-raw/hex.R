# --- Minimal hex sticker: text-only plot -> wiggle -> patina -> hex crop ----

# Packages this script uses directly
stopifnot(requireNamespace("ggplot2", quietly = TRUE),
          requireNamespace("ggpath",  quietly = TRUE),
          requireNamespace("magick",  quietly = TRUE))

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1) Build a blank plot with two text layers
# ---------------------------------------------------------------------------
make_text_plot <- function(title = "ggpatina",
                           tagline = "make it look found",
                           title_size = 22, tag_size = 9) {
  # Coordinates are 0..1; we place title a bit above center, tagline under it
  df <- data.frame(x = c(0.5, 0.5), y = c(0.60, 0.44),
                   label = c(title, tagline),
                   size = c(title_size, tag_size),
                   face = c("bold", "plain"))

  ggplot2::ggplot(df, ggplot2::aes(x, y, label = label)) +
    ggplot2::geom_text(ggplot2::aes(size = size, fontface = face), lineheight = 0.95) +
    ggplot2::coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = FALSE, clip = "off") +
    ggplot2::scale_size_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0,0,0,0))
}

hex_crop <- function(infile, outfile = "man/figures/logo.png") {
  img <- cropcircles::hex_crop(
    images = infile,
    border_colour = "grey22",
    border_size = 10
  )
  p <- ggplot2::ggplot() +
    ggpath::geom_from_path(ggplot2::aes(0.5, 0.5, path = img)) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()

  print(p)
  hexSticker::save_sticker(
    filename = outfile,
    sticker = p
  )
  return(invisible(outfile))
}

# ---------------------------------------------------------------------------
# 2) Parameters (tweak to taste)
# ---------------------------------------------------------------------------
width_in  <- 2.0     # art size before hex crop
height_in <- 2.0     # tall rectangle—save_sticker will crop to pointy-top hex
dpi       <- 300

title <- "ggpatina"
tag   <- "make it look found"

# ---------------------------------------------------------------------------
# 3) Build plot, apply period font (fonts only), then wobble to image
# ---------------------------------------------------------------------------
p0 <- make_text_plot(title, tag, title_size = 28, tag_size = 14)

# Fonts only; no size changes or global state
p1 <- apply_period_fonts(
  p0, era = "journal-1930s", scope = "targeted",
  install_if_missing = TRUE, use_showtext = TRUE
)

# Subtle hand-drawn wiggle
img <- hand_drawn_wiggle(
  p1,
  width  = width_in,
  height = height_in,
  dpi    = dpi,
  scale  = 2.0,      # bump to 2.5–3.0 if you want more wobble
  freq   = 0.55,
  seed   = 7,
  affect_text = TRUE,
  fill_bg = "cyan"
)

# ---------------------------------------------------------------------------
# 4) Optional patinas (pick one; comment out others)
# ---------------------------------------------------------------------------
# A. Old journal scan
# img_pat <- scanify_journal(img, sepia = TRUE, dither = TRUE, tilt_deg = 0.4,
#                            paper = here::here("inst/extdata/paper_texture.png"))

# # B. Transparency slide
# img_pat <- slideify_transparency(img, leak_strength = 0.14, vignette = 0.20, skew = 0.010, grain = 0.45)

# # C. Photocopy
# img_pat <- patina_photocopy(img, ghost_opacity = 0.10, banding = 0.10, tilt_deg = 0.3, protect_lines = 0.85)

# # D. Newscast / CRT (keeps color if your text is colored)
img_pat <- patina_newscast(img, sat=5, scan_strength = 0.8, scan_period = 15L, ca_px = 8L, noise = 0.25, warp = "full", warp_amount = 0.01)

# ---------------------------------------------------------------------------
# 5) Save a rectangular PNG, then hex-crop it
# ---------------------------------------------------------------------------
tmp_png <- tempfile(fileext = ".png")
magick::image_write(img_pat, path = tmp_png, format = "png")

hex_crop(tmp_png, outfile = "man/figures/logo.png")

message("Wrote: man/figures/logo.png")
