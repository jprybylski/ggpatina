
# one place for element -> role mapping
.ggpatina_mapping <- c(
  "text"         = "body",
  "plot.title"   = "title",
  "plot.subtitle"= "body",
  "plot.caption" = "mono",
  "axis.title"   = "body",
  "axis.text"    = "body",
  "strip.text"   = "title",
  "legend.title" = "title",
  "legend.text"  = "body"
)

# choose a template element: prefer plot’s own, else global theme, else global base text
.template_for <- function(th, name) {
  th[[name]] %||% ggplot2::theme_get()[[name]] %||% ggplot2::theme_get()$text
}

# mutate only the $family on a text-like element; preserve size/face/margins/class
.set_family <- function(el, family) {
  if (is.null(el) || inherits(el, "element_blank")) return(el)
  if ("family" %in% names(el)) el$family <- family
  el
}

# helper: map era -> font families and (optionally) register them
.period_families <- function(era = c("journal-1930s","journal-1960s","slide-1970s",
                                     "slide-1980s","typewriter-1950s","handdrawn-pen"),
                             install_if_missing = TRUE,
                             use_showtext = FALSE) { # default FALSE (no global side effects)
  era <- match.arg(era)
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
  fam <- lapply(fam_google, function(g) alias[[g]])

  if (isTRUE(install_if_missing)) {
    if (!requireNamespace("sysfonts", quietly = TRUE))
      stop("Please install 'sysfonts'.")
    # register only the families needed for this era
    for (gname in unique(unlist(fam_google))) {
      try(sysfonts::font_add_google(name = gname,
                                    family = alias[[gname]],
                                    regular.wt = 400, bold.wt = 700),
          silent = TRUE)
    }
  }
  # keep showtext opt-in but off by default; prefer the scoped helpers below
  if (isTRUE(use_showtext)) {
    if (!requireNamespace("showtext", quietly = TRUE))
      stop("Please install 'showtext'.")
    showtext::showtext_auto(TRUE) # caller asked explicitly; warn in docs it's global
  }
  fam
}

#' Apply period fonts in place (fonts only)
#'
#' Changes only the `family` of text elements on a ggplot, preserving size, face,
#' colour, margins, and element classes. When an element is missing and needs to
#' be created (scope = "all_listed"), it is cloned from the current/global theme
#' first so sizes never reset.
#'
#' @param p A ggplot.
#' @param era One of: "journal-1930s","journal-1960s","slide-1970s",
#'   "slide-1980s","typewriter-1950s","handdrawn-pen".
#' @param scope "base_only" (only `text`), "targeted" (also title/subtitle/caption,
#'   axis/strip/legend if they already exist), or "all_listed" (force-create any
#'   missing listed elements by cloning a template).
#' @param install_if_missing,use_showtext See `.period_families()`.
#' @return The modified ggplot.
#' @export
apply_period_fonts <- function(p,
                               era = "journal-1960s",
                               scope = c("base_only","targeted","all_listed"),
                               install_if_missing = TRUE,
                               use_showtext = FALSE) {
  stopifnot(inherits(p, "ggplot"))
  scope <- match.arg(scope)
  fam <- .period_families(era, install_if_missing, use_showtext)

  th <- p$theme %||% ggplot2::theme_get()

  # base text: clone a template so size/lineheight/etc. are preserved
  th$text <- .set_family(.template_for(th, "text"), fam$body)

  if (scope != "base_only") {
    for (nm in setdiff(names(.ggpatina_mapping), "text")) {
      role <- .ggpatina_mapping[[nm]]
      if (scope == "targeted") {
        # only modify if element exists already (no size resets)
        if (!is.null(th[[nm]]) && !inherits(th[[nm]], "element_blank")) {
          th[[nm]] <- .set_family(th[[nm]], fam[[role]])
        }
      } else { # all_listed: ensure it exists by cloning a template, then set family
        th[[nm]] <- .set_family(th[[nm]] %||% .template_for(th, nm), fam[[role]])
      }
    }
  }

  p$theme <- th
  p
}

#' Period font theme (fonts only; clones sizes from current global theme)
#' @inheritParams apply_period_fonts
#' @return A `ggplot2::theme` that changes only font families on listed elements.
#' @export
period_font_theme <- function(era = c("journal-1930s","journal-1960s","slide-1970s",
                                      "slide-1980s","typewriter-1950s","handdrawn-pen"),
                              scope = c("base_only","targeted","all_listed"),
                              install_if_missing = TRUE,
                              use_showtext = FALSE) {
  fam <- .period_families(match.arg(era), install_if_missing, use_showtext)
  scope <- match.arg(scope)

  thg <- ggplot2::theme_get()
  mods <- list(text = .set_family(thg$text, fam$body))
  if (scope != "base_only") {
    for (nm in setdiff(names(.ggpatina_mapping), "text")) {
      tpl <- thg[[nm]] %||% thg$text
      mods[[nm]] <- .set_family(tpl, fam[[ .ggpatina_mapping[[nm]] ]])
    }
  }
  do.call(ggplot2::theme, mods)
}

# ---- Scoped showtext wrappers (no global state left behind) ------------------

# Enables showtext_auto just for this call, then turns it off.
# If the user already had auto=TRUE globally, this will disable it afterwards.
with_showtext_auto <- function(expr) {
  if (!requireNamespace("showtext", quietly = TRUE))
    stop("Please install 'showtext'.")
  withr::defer(showtext::showtext_auto(FALSE), parent.frame())
  showtext::showtext_auto(TRUE)
  force(expr)
}

#' Print / save helpers that scope showtext to this call only
#'
#' @param plot `ggplot` Plot object
#' @param filename Name passed to ggsave
#' @param ... Passed to ggsave
#' @rdname print_save
#' @export
print_st <- function(plot) with_showtext_auto(print(plot))


#' @rdname print_save
#' @export
ggsave_st <- function(filename, plot, ...) with_showtext_auto(
  ggplot2::ggsave(filename = filename, plot = plot, ...)
)
