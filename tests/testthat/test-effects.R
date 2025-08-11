test_that("as_magick renders ggplot to magick image", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("magick")
  skip_if_not_installed("showtext")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  img <- as_magick(p, width = 1, height = 1, dpi = 72)
  expect_s3_class(img, "magick-image")
})

test_that("patina_blueprint outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_blueprint(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_edu_film outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_edu_film(img, dust = 0)
  expect_s3_class(out, "magick-image")
})

test_that("animate_edu_film creates multiple frames", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(50, 50, "white")
  anim <- animate_edu_film(img, n_frames = 3, dust = 0)
  expect_s3_class(anim, "magick-image")
  info <- magick::image_info(anim)
  expect_gt(nrow(info), 1)
})

test_that("patina_ink_bleed outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_ink_bleed(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_kodachrome outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_kodachrome(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_newscast outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_newscast(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_newspaper outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_newspaper(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_photocopy outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_photocopy(img)
  expect_s3_class(out, "magick-image")
})

test_that("hand_drawn_wiggle outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(60, 60, "white")
  out <- hand_drawn_wiggle(img, scale = 1)
  expect_s3_class(out, "magick-image")
})

test_that("slideify_transparency outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  out <- slideify_transparency(img)
  expect_s3_class(out, "magick-image")
})

test_that("scanify_journal outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  out <- scanify_journal(img, dither = FALSE, sepia = FALSE)
  expect_s3_class(out, "magick-image")
})
