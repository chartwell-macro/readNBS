test_that("dimension keys are canonical, encoded and fail closed", {
  expect_equal(
    nbs_dimension_key(list(size = "90–144 m²", city = "北京")),
    paste0(
      "city=%E5%8C%97%E4%BA%AC|",
      "size=90%E2%80%93144%20m%C2%B2"
    )
  )
  expect_equal(nbs_dimension_key(list()), "")
  expect_error(
    nbs_dimension_key(list(city = c("beijing", "shanghai"))),
    class = "nbs_dimension_error"
  )
  expect_error(
    nbs_dimension_key(stats::setNames(list("x", "y"), c("a", "a"))),
    class = "nbs_dimension_error"
  )
})
