context("palette generation")
test_that("palette generation is accurate", {

  # data is intact
  expect_equal(dim(cividis.map), c(256, 4))

  # bad inputs
  expect_warning(cividis(n = 1, alpha = 1, begin = 0, end = 1, direction = 1, option = "E"))
  expect_error(cividis(1, direction = 100))
  expect_error(cividis(1, begin = -1))
  expect_error(cividis(1, begin = 100))
  expect_error(cividis(1, end = -1))
  expect_error(cividis(1, end = 100))
  expect_equal(cividis(1, direction = -1), "#FFEA46FF")

})
