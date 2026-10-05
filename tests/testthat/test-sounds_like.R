test_that("sounds_like compares element by element with recycling", {
  expect_equal(sounds_like("Agarwal", c("Aggarwal", "Agrawal", "Agnihotri")),
               c(TRUE, TRUE, FALSE))
  expect_equal(sounds_like(c("Smith", "Jose"), c("Schmidt", "Hosay")),
               c(TRUE, TRUE))
})

test_that("Double Metaphone uses secondary codes, Metaphone does not", {
  expect_true(sounds_like("Smith", "Schmidt"))
  expect_false(sounds_like("Smith", "Schmidt", method = "metaphone"))
})

test_that("full codes tell long names apart", {
  expect_false(sounds_like("Venkatesh", "Venkataraman"))
  expect_true(sounds_like("Venkatesh", "Venkataraman", max_length = 4))
})

test_that("missing and empty names are handled", {
  expect_equal(sounds_like(c("Anil", NA, ""), c(NA, "Anil", "")),
               c(NA, NA, FALSE))
  expect_equal(sounds_like("123", "456"), FALSE)
  expect_length(sounds_like(character(0), "Anil"), 0L)
})

test_that("by_word passes through", {
  expect_false(sounds_like("Ramesh Kumar", "Kumar Ramesh", by_word = TRUE))
  expect_true(sounds_like("Ramesh Kumar", "Rameshh Kumaar", by_word = TRUE))
})
