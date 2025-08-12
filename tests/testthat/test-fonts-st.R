test_that("apply_period_fonts sets families", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p2 <- apply_period_fonts(p, era = "journal-1960s", install_if_missing = FALSE, use_showtext = FALSE)
  expect_equal(p2$theme$text$family, "crimson_pro")
})

test_that("period_font_theme returns theme with families", {
  skip_if_not_installed("ggplot2")
  th <- period_font_theme("slide-1980s", install_if_missing = FALSE, use_showtext = FALSE)
  expect_s3_class(th, "theme")
  expect_equal(th$text$family, "jost")
})

test_that("print_st prints without error when showtext available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("showtext")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_error(print_st(p), NA)
})

test_that("print_st errors without showtext", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  withr::with_libpaths(
    tempdir(), action = "replace", include_site = FALSE, include_base = FALSE, {
    if ("showtext" %in% loadedNamespaces()) unloadNamespace("showtext")
    expect_error(print_st(p), "Please install 'showtext'", fixed = TRUE)
  })
})

test_that("ggsave_st saves file when showtext available", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("showtext")
  f <- tempfile(fileext = ".png")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_error(ggsave_st(f, plot = p, width = 1, height = 1, units = "in", dpi = 72), NA)
  expect_true(file.exists(f))
})

test_that("ggsave_st errors without showtext", {
  skip_if_not_installed("ggplot2")
  f <- tempfile(fileext = ".png")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  withr::with_libpaths(
    tempdir(), action = "replace", include_site = FALSE, include_base = FALSE, {
    if ("showtext" %in% loadedNamespaces()) unloadNamespace("showtext")
    expect_error(
      ggsave_st(f, plot = p, width = 1, height = 1, units = "in", dpi = 72),
      "Please install 'showtext'",
      fixed = TRUE
    )
  })
})
