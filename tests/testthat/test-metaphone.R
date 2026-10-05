# Expected codes are from MetaphoneTest.java in Apache Commons Codec,
# which uses a maximum length of four unless stated otherwise.
# See inst/COPYRIGHTS.

test_that("codes match the Apache Commons Codec reference", {
  cases <- c(
    SCIENCE = "SNS", SCENE = "SN", SCY = "S", GNU = "N", SIGNED = "SNT",
    GHENT = "KNT", BAUGH = "B", AXEAXE = "AKSK", howl = "HL",
    testing = "TSTN", The = "0", quick = "KK", brown = "BRN", fox = "FKS",
    jumped = "JMPT", over = "OFR", lazy = "LS", dogs = "TKS",
    PHISH = "FX", SHOT = "XT", ODSIAN = "OTXN", PULSION = "PLXN",
    RETCH = "RX", WATCH = "WX", OTIA = "OX", PORTION = "PRXN",
    SCHEDULE = "SKTL", SCHEMATIC = "SKMT", DISCHARGE = "TSKR",
    ECHO = "EX", TEACH = "TX", CHERI = "XR", CHIP = "XP", CHRIST = "XRST",
    CIAO = "X", CITY = "ST", CAT = "KT", DODGY = "TJ", DODGE = "TJ",
    ADGIEMTI = "AJMT", WHY = "", COMB = "KM", TOMB = "TM", WOMB = "WM",
    CIAPO = "XP"
  )
  expect_equal(metaphone(names(cases), max_length = 4),
               unname(cases))
  expect_equal(metaphone("AXEAXEAXE", max_length = 6), "AKSKSK")
  expect_equal(metaphone("CHARACTER", max_length = 5), "XRKTR")
})

test_that("Commons Codec sound-alike groups get equal codes", {
  groups <- list(
    c("Lawrence", "Lorenza"),
    c("Gary", "Cahra", "Cara", "Carey", "Kara", "Kerry", "Cory", "Gray"),
    c("Mary", "Mair", "Maria", "Moira", "Myra"),
    c("Peter", "Peadar", "Pedro", "Pieter", "Piotr"),
    c("Ray", "Rey", "Roi", "Roy", "Ruy"),
    c("Wright", "Rota", "Rudd", "Ryde"),
    c("Xalan", "Celene", "Selina", "Suellen", "Xylina")
  )
  for (g in groups) {
    expect_length(unique(metaphone(g, max_length = 4)), 1L)
  }
})

test_that("codes are not cut to four characters", {
  expect_equal(metaphone("Venkataraman"), "FNKTRMN")
  expect_equal(metaphone("Venkataraman", max_length = 4), "FNKT")
})

test_that("leading vowels are kept and NA is returned for NA", {
  expect_equal(metaphone(c("Anil", "Ekta", NA, "")),
               c("ANL", "EKT", NA, ""))
  expect_length(metaphone(character(0)), 0L)
})

test_that("by_word codes each word separately", {
  expect_equal(metaphone("Ramesh Kumar", by_word = TRUE), "RMX KMR")
  expect_equal(metaphone("Ramesh Kumar"), "RMXKMR")
})

test_that("accented letters are read as plain letters", {
  expect_equal(metaphone("M\u00FCller"), metaphone("Muller"))
  expect_equal(metaphone("Fran\u00E7ois"), metaphone("Fransois"))
})
