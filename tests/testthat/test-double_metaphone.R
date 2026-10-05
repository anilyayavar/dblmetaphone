test_that("four-character codes match the Apache Commons Codec reference", {
  # 1221 names with expected codes, from DoubleMetaphone2Test.java in
  # Apache Commons Codec. See inst/COPYRIGHTS.
  ref <- utils::read.csv(test_path("testdata", "commons_double_metaphone.csv"),
                         stringsAsFactors = FALSE, na.strings = character(0))
  res <- double_metaphone(ref$word, max_length = 4)
  expect_equal(res$primary, ref$primary)
  expect_equal(res$secondary, ref$secondary)
})

test_that("well-known examples give the published codes", {
  res <- double_metaphone(c("Smith", "Schmidt", "Xavier", "Michael",
                            "Jose", "Zhao", "Filipowicz", "Womo"))
  expect_equal(res$primary,
               c("SM0", "XMT", "SF", "MKL", "HS", "J", "FLPTS", "AM"))
  expect_equal(res$secondary,
               c("XMT", "SMT", "SFR", "MXL", "HS", "J", "FLPFX", "FM"))
})

test_that("codes are not cut to four characters", {
  res <- double_metaphone(c("Venkatesh", "Venkataraman", "Venkatraman"))
  expect_equal(res$primary, c("FNKTX", "FNKTRMN", "FNKTRMN"))
  expect_true(all(nchar(res$primary) > 4))
})

test_that("max_length cuts the codes", {
  res <- double_metaphone("Ramakrishnan", max_length = 4)
  expect_equal(res$primary, "RMKR")
  expect_equal(double_metaphone("Ramakrishnan", max_length = 6)$primary,
               "RMKRXN")
})

test_that("double L in Spanish names is coded once", {
  # Regression test for the bug in the first version of the package.
  res <- double_metaphone(c("Cabrillo", "Gallegos"))
  expect_equal(res$primary, c("KPRL", "KLKS"))
  expect_equal(res$secondary, c("KPR", "KKS"))
})

test_that("GN is coded as in the original algorithm", {
  res <- double_metaphone(c("Cagney", "Wagner", "Agnes"))
  expect_equal(res$primary, c("KKN", "AKNR", "AKNS"))
  expect_equal(res$secondary, c("KKN", "FKNR", "ANS"))
})

test_that("the result has one row per input and handles NA", {
  x <- c("Anil", NA, "", "123", "Atharv")
  res <- double_metaphone(x)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("primary", "secondary"))
  expect_equal(nrow(res), 5L)
  expect_equal(res$primary[c(2, 3, 4)], c(NA, "", ""))
  expect_equal(res$secondary[2], NA_character_)
  expect_equal(nrow(double_metaphone(character(0))), 0L)
  expect_equal(nrow(double_metaphone(NULL)), 0L)
})

test_that("case, factors, repeats and punctuation are handled", {
  expect_equal(double_metaphone("smith"), double_metaphone("SMITH"))
  expect_equal(double_metaphone(factor(c("Smith", "Smith"))),
               double_metaphone(c("Smith", "Smith")))
  expect_equal(double_metaphone("O'Brien"), double_metaphone("OBrien"))
  expect_equal(double_metaphone("Ram-Kumar"), double_metaphone("Ram Kumar"))
  expect_equal(double_metaphone("  Ram   Kumar. "),
               double_metaphone("Ram Kumar"))
})

test_that("accented letters are read as plain letters", {
  expect_equal(double_metaphone("M\u00FCller"), double_metaphone("Muller"))
  expect_equal(double_metaphone("Jos\u00E9"), double_metaphone("Jose"))
  # The cedilla is sounded as S
  expect_equal(double_metaphone("Fran\u00E7ois")$primary, "FRNS")
})

test_that("by_word codes each word separately", {
  res <- double_metaphone("Ramesh Kumar Sharma", by_word = TRUE)
  expect_equal(res$primary, "RMX KMR XRM")
  whole <- double_metaphone("Ramesh Kumar Sharma")
  expect_false(grepl(" ", whole$primary))
  expect_equal(double_metaphone("Ramakrishnan Iyer", by_word = TRUE,
                                max_length = 4)$primary,
               "RMKR AR")
  expect_equal(double_metaphone("", by_word = TRUE)$primary, "")
})

test_that("bad arguments give clear errors", {
  expect_error(double_metaphone("a", max_length = 0), "max_length")
  expect_error(double_metaphone("a", max_length = c(4, 5)), "max_length")
  expect_error(double_metaphone("a", by_word = NA), "by_word")
  expect_error(double_metaphone(list("a")), "character vector")
})
