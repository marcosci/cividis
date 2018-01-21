context("scales")
test_that("scales work correctly", {

  fill_scale <- scale_fill_cividis()
  expect_equal(fill_scale$scale_name, "gradientn")

  fill_scale <- scale_fill_cividis(discrete = TRUE)
  expect_equal(fill_scale$is_discrete(), TRUE)

  color_scale <- scale_color_cividis()
  expect_equal(color_scale$scale_name, "gradientn")

  color_scale <- scale_color_cividis(discrete = TRUE)
  expect_equal(color_scale$is_discrete(), TRUE)

  expect_equal(cividis_pal(1)(5),
               c("#00204DFF", "#414D6BFF", "#7C7B78FF", "#BCAF6FFF", "#FFEA46FF"))

})