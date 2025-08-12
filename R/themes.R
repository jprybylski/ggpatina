#' Newspaper theme
#'
#' Off-white page with crisp axes and dashed major grid lines.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_newspaper()
#' }
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background color
#' @param paper overall page color
#' @param grid_col major grid line color
#' @export
theme_newspaper <- function(base_size = 11, base_family = NULL,
                            bg = "#f7f3e8", paper = "#f5f0e6",
                            grid_col = "#6b6b6b") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = paper, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.3, linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#111111", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#111111", linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = "#e8e0cf", color = "#111111", linewidth = 0.3),
      strip.text       = ggplot2::element_text(margin = ggplot2::margin(4,4,4,4)),
      plot.margin      = ggplot2::margin(10, 12, 12, 12)
    )
}

#' Newscast theme
#'
#' Dark background with light ink, faint grid, and CRT-inspired styling.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_newscast()
#' }
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background
#' @param page overall background
#' @param grid_col grid color (low alpha)
#' @export
theme_newscast <- function(base_size = 11, base_family = NULL,
                           bg = "#151515", page = "#111111",
                           grid_col = "#ffffff22") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = bg,   color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(color = "#f2f2f2"),
      axis.title       = ggplot2::element_text(color = "#ffffff"),
      axis.line        = ggplot2::element_line(color = "#f0f0f0", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#e8e8e8", linewidth = 0.3),
      legend.background= ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key       = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.background = ggplot2::element_rect(fill = "#1c1c1c", color = "#303030"),
      strip.text       = ggplot2::element_text(color = "#ffffff"),
      plot.margin      = ggplot2::margin(8, 10, 10, 10)
    )
}

#' Educational-film theme
#'
#' Warm page tones with dotted majors and a subtle border.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_edu_film()
#' }
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background
#' @param page overall background
#' @param grid_col major grid color
#' @export
theme_edu_film <- function(base_size = 11, base_family = NULL,
                           bg = "#e9e1c9", page = "#e5dcc5",
                           grid_col = "#5b533c66") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = bg,   color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#403a2a88", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#403a2a88", linewidth = 0.3),
      axis.text        = ggplot2::element_text(color = "#2e2a1f"),
      axis.title       = ggplot2::element_text(color = "#2e2a1f"),
      panel.border     = ggplot2::element_rect(fill = NA, color = "#403a2a44", linewidth = 0.4),
      strip.background = ggplot2::element_rect(fill = "#ddd1ad", color = "#403a2a66"),
      strip.text       = ggplot2::element_text(margin = ggplot2::margin(3,5,3,5)),
      plot.margin      = ggplot2::margin(10, 12, 14, 12)
    )
}

#' Transparency-slide theme
#'
#' Clean white slide with a thin grid and generous margins.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_transparency()
#' }
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param page page background
#' @param grid_col grid color
#' @export
theme_transparency <- function(base_size = 11, base_family = NULL,
                               page = "white", grid_col = "#bdbdbd66") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = page, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#111111", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#111111", linewidth = 0.3),
      plot.margin      = ggplot2::margin(16, 18, 16, 18)  # room for vignette/leaks
    )
}

#' Blueprint theme
#'
#' Navy background with cyan lines and high-contrast text.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     theme_blueprint()
#' }
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param page page/navy background
#' @param ink line/text color
#' @param grid_col grid color
#' @export
theme_blueprint <- function(base_size = 11, base_family = NULL,
                            page = "#0b1d3a", ink = "#e6f2ff",
                            grid_col = "#78b2ff33") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = page, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.4, linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(color = ink),
      axis.title       = ggplot2::element_text(color = ink),
      axis.line        = ggplot2::element_line(color = "#9ad1ff", linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(color = "#9ad1ff", linewidth = 0.4),
      strip.background = ggplot2::element_rect(fill = NA, color = "#68aef7"),
      strip.text       = ggplot2::element_text(color = ink),
      legend.text      = ggplot2::element_text(color = ink),
      legend.title     = ggplot2::element_text(color = ink),
      plot.margin      = ggplot2::margin(12, 14, 12, 14),
      plot.title       = ggplot2::element_text(color = ink)
    )
}


#' Journal-style theme core
#'
#' Conservative scaffold for journals: clear axes, restrained grids, white
#' backgrounds, and no font-size changes so it plays nicely with font helpers.
#'
#' @param base_size Numeric base text size passed to \code{ggplot2::theme_minimal()}.
#'   Defaults to 11. This does not change sizes you have already set explicitly.
#' @param base_family Optional base font family. Leave \code{NULL} to inherit.
#' @param grid_major_y,grid_major_x Logical; draw major grids on Y / X?
#' @param grid_minor Logical; draw minor grids?
#' @param panel_border Logical; draw a thin black panel border?
#' @param strip_border Logical; draw a thin border around facet strips?
#' @param axis_ticks_inside Logical; if \code{TRUE} ticks point inward.
#'
#' @return A \code{ggplot2::theme} object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'   p + theme_journal_core()
#' }
theme_journal_core <- function(
  base_size = 11, base_family = NULL,
  grid_major_y = TRUE, grid_major_x = FALSE,
  grid_minor   = FALSE, panel_border = TRUE,
  strip_border = TRUE, axis_ticks_inside = TRUE
) {
  gmaj_x <- if (isTRUE(grid_major_x)) ggplot2::element_line(colour = "#c7c7c7", linewidth = 0.3, lineend = "round") else ggplot2::element_blank()
  gmaj_y <- if (isTRUE(grid_major_y)) ggplot2::element_line(colour = "#c7c7c7", linewidth = 0.3, lineend = "round") else ggplot2::element_blank()
  gmin   <- if (isTRUE(grid_minor))   ggplot2::element_line(colour = "#e0e0e0", linewidth = 0.25, lineend = "round") else ggplot2::element_blank()
  pborder<- if (isTRUE(panel_border)) ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.5) else ggplot2::element_blank()
  sbord  <- if (isTRUE(strip_border)) ggplot2::element_rect(fill = NA, colour = "#606060", linewidth = 0.5) else ggplot2::element_blank()
  tdir   <- if (isTRUE(axis_ticks_inside)) ggplot2::unit(-3, "pt") else ggplot2::unit(3, "pt")

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # panel
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major.x = gmaj_x,
      panel.grid.major.y = gmaj_y,
      panel.grid.minor   = gmin,
      panel.border       = pborder,

      # axes
      axis.title.x = ggplot2::element_text(face = "plain"),
      axis.title.y = ggplot2::element_text(face = "plain"),
      axis.text    = ggplot2::element_text(colour = "#111111"),
      axis.ticks   = ggplot2::element_line(colour = "#121212", linewidth = 0.3),
      axis.ticks.length = tdir,
      axis.line    = ggplot2::element_line(colour = "black", linewidth = 0.4),

      # strips
      strip.background  = ggplot2::element_rect(fill = "white", colour = NA),
      strip.background.x = sbord,
      strip.background.y = sbord,
      strip.text        = ggplot2::element_text(face = "bold"),
      strip.placement   = "outside",
      strip.clip        = "off",

      # legend
      legend.key    = ggplot2::element_rect(fill = "white", colour = NA),
      legend.title  = ggplot2::element_text(face = "plain"),
      legend.text   = ggplot2::element_text(),

      # plot
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(),
      plot.caption  = ggplot2::element_text(size = base_size * 0.85, colour = "#2b2b2b"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin   = ggplot2::margin(10, 12, 10, 12)
    )
}

#' Journal theme: c. 1900–1920
#'
#' Stark, monochrome look with only Y major grid and a thin panel border.
#'
#' @inheritParams theme_journal_core
#' @return A \code{ggplot2::theme}.
#' @export
#' @examples
#' # ggplot2::theme_set(theme_journal_1900s())
theme_journal_1900s <- function(base_size = 11, base_family = NULL) {
  theme_journal_core(
    base_size = base_size, base_family = base_family,
    grid_major_y = TRUE, grid_major_x = FALSE, grid_minor = FALSE,
    panel_border = TRUE, strip_border = TRUE, axis_ticks_inside = TRUE
  ) + ggplot2::theme(
    axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.6),
    panel.grid.major.y = ggplot2::element_line(colour = "#b0b0b0", linewidth = 0.35)
  )
}

#' Journal theme: c. 1930s–1940s
#'
#' Finer lines, Y and light X major grids, bordered facet strips.
#'
#' @inheritParams theme_journal_core
#' @return A \code{ggplot2::theme}.
#' @export
theme_journal_1930s <- function(base_size = 11, base_family = NULL) {
  theme_journal_core(
    base_size = base_size, base_family = base_family,
    grid_major_y = TRUE, grid_major_x = TRUE, grid_minor = FALSE,
    panel_border = TRUE, strip_border = TRUE, axis_ticks_inside = TRUE
  ) + ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(colour = "#d4d4d4", linewidth = 0.3),
    panel.grid.major.y = ggplot2::element_line(colour = "#c2c2c2", linewidth = 0.3),
    axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.45)
  )
}

#' Journal theme: c. 1960s
#'
#' Clean, light grids on both axes, no panel border, outward ticks.
#'
#' @inheritParams theme_journal_core
#' @return A \code{ggplot2::theme}.
#' @export
theme_journal_1960s <- function(base_size = 11, base_family = NULL) {
  theme_journal_core(
    base_size = base_size, base_family = base_family,
    grid_major_y = TRUE, grid_major_x = TRUE, grid_minor = FALSE,
    panel_border = FALSE, strip_border = TRUE, axis_ticks_inside = FALSE
  ) + ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(colour = "#dddddd", linewidth = 0.3),
    panel.grid.major.y = ggplot2::element_line(colour = "#c9c9c9", linewidth = 0.3),
    strip.text         = ggplot2::element_text(face = "bold"),
    axis.line          = ggplot2::element_line(colour = "black", linewidth = 0.35)
  )
}

#' Discrete color palette for journal themes
#'
#' Muted grayscale or limited spot-color sets that reproduce well in print-like
#' patinas. The palettes are intentionally short; repeated values will recycle.
#'
#' @param era One of \code{"1900s"}, \code{"1930s"}, \code{"1960s"}.
#' @param variant \code{"gray"} (default) or \code{"spot"} (only for 1960s).
#' @param ... Passed to \code{ggplot2::scale_color_manual()}.
#' @return A discrete color scale.
#' @export
#' @examples
#' # p + scale_color_journal("1930s")
scale_color_journal <- function(era = c("1900s","1930s","1960s"),
                                variant = c("gray","spot"),
                                ...) {
  era <- match.arg(era); variant <- match.arg(variant)
  gray <- list(
    `1900s` = c("#111111","#2d2d2d","#555555","#7a7a7a","#a1a1a1"),
    `1930s` = c("#101010","#303030","#585858","#808080","#a8a8a8","#c2c2c2"),
    `1960s` = c("#0f0f0f","#2e2e2e","#4b4b4b","#6a6a6a","#8a8a8a","#adadad")
  )
  spot <- c("#9b2c2c", "#1e6e74", "#7d6b1f", "#3b3b3b") # burgundy, teal, ochre, charcoal

  if (variant == "gray") {
    ggplot2::scale_color_manual(values = gray[[era]], ...)
  } else {
    if (era != "1960s") warning("Spot colors only defined for 1960s; using 1960s palette.")
    ggplot2::scale_color_manual(values = spot, ...)
  }
}

#' Discrete fill palette for journal themes
#'
#' See \code{\link{scale_color_journal}} for details.
#'
#' @inheritParams scale_color_journal
#' @return A discrete fill scale.
#' @export
#' @examples
#' # p + scale_fill_journal("1900s")
scale_fill_journal <- function(era = c("1900s","1930s","1960s"),
                               variant = c("gray","spot"),
                               ...) {
  era <- match.arg(era); variant <- match.arg(variant)
  gray <- list(
    `1900s` = c("#111111","#2d2d2d","#555555","#7a7a7a","#a1a1a1"),
    `1930s` = c("#101010","#303030","#585858","#808080","#a8a8a8","#c2c2c2"),
    `1960s` = c("#0f0f0f","#2e2e2e","#4b4b4b","#6a6a6a","#8a8a8a","#adadad")
  )
  spot <- c("#9b2c2c", "#1e6e74", "#7d6b1f", "#3b3b3b")

  if (variant == "gray") {
    ggplot2::scale_fill_manual(values = gray[[era]], ...)
  } else {
    if (era != "1960s") warning("Spot colors only defined for 1960s; using 1960s palette.")
    ggplot2::scale_fill_manual(values = spot, ...)
  }
}

#' Continuous grayscale scale (color)
#'
#' Two-stop gradient that prints well with journal patinas.
#'
#' @param low,high Hex colors for the gradient endpoints.
#' @param ... Passed to \code{ggplot2::scale_color_gradient()}.
#' @return A continuous color scale.
#' @export
#' @examples
#' # p + scale_color_journal_continuous()
scale_color_journal_continuous <- function(low = "#0f0f0f", high = "#adadad", ...) {
  ggplot2::scale_color_gradient(low = low, high = high, ...)
}

#' Continuous grayscale scale (fill)
#'
#' @inheritParams scale_color_journal_continuous
#' @return A continuous fill scale.
#' @export
#' @examples
#' # p + scale_fill_journal_continuous()
scale_fill_journal_continuous <- function(low = "#0f0f0f", high = "#adadad", ...) {
  ggplot2::scale_fill_gradient(low = low, high = high, ...)
}
