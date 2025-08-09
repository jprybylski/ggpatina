
#' Period-inspired font theme for ggplot
#'
#' @param era One of "journal-1930s","journal-1960s","slide-1970s","slide-1980s",
#'   "typewriter-1950s","handdrawn-pen".
#' @param base_size Base text size.
#' @param install_if_missing If TRUE, attempt to install Google fonts via sysfonts.
#' @param use_showtext If TRUE, enable showtext for reliable rendering.
#' @export
period_font_theme <- function(
  era = c("journal-1930s","journal-1960s","slide-1970s","slide-1980s","typewriter-1950s","handdrawn-pen"),
  base_size = 11, install_if_missing = TRUE, use_showtext = TRUE
) {
  era <- match.arg(era)

  # Google name -> local alias (use these in element_text(family=...))
  alias <- c(
    "IM FELL English" = "im_fell_english",
    "Crimson Pro"     = "crimson_pro",
    "Courier Prime"   = "courier_prime",
    "Archivo"         = "archivo",
    "Jost"            = "jost",
    "Special Elite"   = "special_elite",
    "Patrick Hand"    = "patrick_hand"
  )

  presets <- list(
    `journal-1930s`    = list(title="IM FELL English", body="IM FELL English", mono="Courier Prime"),
    `journal-1960s`    = list(title="Crimson Pro",     body="Crimson Pro",     mono="Courier Prime"),
    `slide-1970s`      = list(title="Archivo",         body="Archivo",         mono="Courier Prime"),
    `slide-1980s`      = list(title="Jost",            body="Jost",            mono="Courier Prime"),
    `typewriter-1950s` = list(title="Special Elite",   body="Special Elite",   mono="Special Elite"),
    `handdrawn-pen`    = list(title="Patrick Hand",    body="Patrick Hand",    mono="Courier Prime")
  )
  fam_google <- presets[[era]]
  fam <- lapply(fam_google, function(g) alias[[g]])  # use aliases in the theme

  # --- Correct registration block ---
  if (install_if_missing) {
    if (!requireNamespace("sysfonts", quietly = TRUE))
      stop("Please install 'sysfonts' to auto-install Google fonts.")
    # add each font with correct (name, family) ordering
    for (gname in unique(unlist(presets))) {
      sysfonts::font_add_google(name = gname, family = alias[[gname]])
    }
  }
  # ----------------------------------

  if (use_showtext) {
    if (!requireNamespace("showtext", quietly = TRUE))
      stop("Please install 'showtext' to draw Google fonts reliably.")
    showtext::showtext_auto(enable = TRUE)
  }

  bump <- switch(era, "slide-1970s"=1.10, "slide-1980s"=1.12, "handdrawn-pen"=1.05, 1.00)

  ggplot2::theme_minimal(base_size = base_size, base_family = fam$body) +
    ggplot2::theme(
      text = ggplot2::element_text(family = fam$body),
      plot.title = ggplot2::element_text(family = fam$title, face = "bold",
                                         size = base_size * 1.35 * bump,
                                         margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = base_size * 1.05 * bump,
                                            margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(family = fam$mono, size = base_size * 0.8, hjust = 1),
      axis.title = ggplot2::element_text(face = if (grepl("^journal", era)) "plain" else "bold",
                                         size = base_size * 1.0 * bump),
      axis.text  = ggplot2::element_text(size = base_size * 0.95 * bump),
      strip.text = ggplot2::element_text(family = fam$title, face = "bold",
                                         size = base_size * 1.0 * bump)
    )
}


#' Apply period fonts to a plot
#' @export
apply_period_fonts <- function(p, era = "journal-1960s", ...) {
  p + period_font_theme(era = era, ...)
}
