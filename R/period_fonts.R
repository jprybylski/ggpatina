
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
  presets <- list(
    `journal-1930s` = list(title="IM FELL English", body="IM FELL English", mono="Courier Prime"),
    `journal-1960s` = list(title="Crimson Pro",     body="Crimson Pro",     mono="Courier Prime"),
    `slide-1970s`   = list(title="Archivo",         body="Archivo",         mono="Courier Prime"),
    `slide-1980s`   = list(title="Jost",            body="Jost",            mono="Courier Prime"),
    `typewriter-1950s` = list(title="Special Elite", body="Special Elite", mono="Special Elite"),
    `handdrawn-pen` = list(title="Patrick Hand",    body="Patrick Hand",    mono="Courier Prime")
  )
  fam <- presets[[era]]

  ensure_font <- function(name) {
    ok <- try(!is.na(systemfonts::match_font(name)$path), silent = TRUE)
    if (!isTRUE(ok) && install_if_missing) {
      if (!requireNamespace("sysfonts", quietly = TRUE))
        stop("Install 'sysfonts' to auto-install Google fonts.")
      sysfonts::font_add_google(name, name, regular.wt = 400, bold.wt = 700)
    }
    invisible(TRUE)
  }
  for (n in unique(unlist(fam))) ensure_font(n)

  if (use_showtext) {
    if (!requireNamespace("showtext", quietly = TRUE))
      stop("Install 'showtext' to draw Google fonts reliably.")
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
