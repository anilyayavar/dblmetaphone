test_that("us_surnames has the documented shape", {
  expect_s3_class(us_surnames, "data.frame")
  expect_named(us_surnames, c("surname", "rank", "count"))
  expect_equal(nrow(us_surnames), 5000L)
  expect_equal(us_surnames$surname[1], "Smith")
  expect_false(anyNA(us_surnames))
  expect_false(is.unsorted(us_surnames$rank))
})
