#' Metaphone codes, in full
#'
#' Encodes names or words with the original Metaphone algorithm of
#' Lawrence Philips (1990). Each input gets one code. Words that sound
#' alike in English, such as "Knight" and "Night", get the same code.
#'
#' Many implementations cut the code to four characters. Here the code is
#' kept in full by default. Set `max_length = 4` to get the traditional
#' short codes.
#'
#' The rules follow the Metaphone implementation in Apache Commons Codec,
#' a widely used reference version. Unlike Double Metaphone, Metaphone
#' keeps a leading vowel as itself, so "Anna" gives `"AN"` and "Emma"
#' gives `"EM"`.
#'
#' @inheritSection double_metaphone Several words
#' @inheritParams double_metaphone
#'
#' @section Preparing the text:
#' Letters are changed to capitals, and common accented Latin letters are
#' replaced by plain ones. The C with a cedilla becomes S. Apostrophes are
#' removed, and any other character that is not a letter is treated as a
#' space. Names written in other scripts should be transliterated to Latin
#' letters first.
#'
#' @return A character vector of codes, the same length as `x`. Missing
#'   values in `x` give `NA`. A value with no codable letters gives an
#'   empty string.
#'
#' @references
#' Philips, L. (1990). Hanging on the metaphone. *Computer Language*,
#' 7(12), 38-43.
#'
#' Apache Commons Codec, the Metaphone class.
#' \url{https://commons.apache.org/proper/commons-codec/}
#'
#' @seealso [double_metaphone()] for the improved two-code algorithm,
#'   [sounds_like()] to compare two sets of names, and
#'   `vignette("fullmetaphone")` for worked examples.
#'
#' @examples
#' metaphone(c("Knight", "Night", "Philip", "Filip", "Catherine", "Katherine"))
#'
#' # Full codes against the traditional four characters
#' metaphone(c("Rosenberg", "Rosenbaum"))
#' metaphone(c("Rosenberg", "Rosenbaum"), max_length = 4)
#'
#' metaphone("Jean Pierre Dubois", by_word = TRUE)
#' @export
metaphone <- function(x, max_length = Inf, by_word = FALSE) {
  check_args(max_length, by_word)
  codes <- encode_vector(x, mp_engine, n_codes = 1L, keep_special = FALSE,
                         by_word = by_word, max_length = max_length)
  codes[, 1L]
}

# Metaphone for one cleaned, upper-case string.
#
# A translation of the Metaphone class in Apache Commons Codec (Apache
# License 2.0). Positions are kept 0-based, as in the Java code, so the
# two can be compared rule by rule. The Java code stops at a maximum
# length; this version runs to the end of the string.
mp_engine <- function(word) {
  n_in <- nchar(word)
  if (n_in == 0L) {
    return("")
  }
  # A single character is its own code.
  if (n_in == 1L) {
    return(word)
  }

  inwd <- strsplit(word, "", fixed = TRUE)[[1L]]

  # Initial letter exceptions
  first <- inwd[1L]
  second <- inwd[2L]
  if (first %in% c("K", "G", "P") && second == "N") {
    local <- inwd[-1L]                   # KN, GN, PN
  } else if (first == "A" && second == "E") {
    local <- inwd[-1L]                   # AE
  } else if (first == "W" && second == "R") {
    local <- inwd[-1L]                   # WR becomes R
  } else if (first == "W" && second == "H") {
    local <- c("W", inwd[-(1:2)])        # WH becomes W
  } else if (first == "X") {
    local <- c("S", inwd[-1L])           # initial X becomes S
  } else {
    local <- inwd
  }

  wdsz <- length(local)
  text <- paste(local, collapse = "")
  vowels <- c("A", "E", "I", "O", "U")
  frontv <- c("E", "I", "Y")
  varson <- c("C", "S", "P", "T", "G")

  char_at <- function(i) local[i + 1L]
  is_last <- function(i) i + 1L == wdsz
  is_vowel <- function(i) i < wdsz && char_at(i) %in% vowels
  is_next <- function(i, ch) i < wdsz - 1L && char_at(i + 1L) == ch
  is_prev <- function(i, ch) i > 0L && i < wdsz && char_at(i - 1L) == ch
  region <- function(i, test) {
    end <- i + nchar(test)
    end <= wdsz && substr(text, i + 1L, end) == test
  }

  code <- character(0)
  n <- 0L
  while (n < wdsz) {
    symb <- char_at(n)
    # Skip a repeated letter, except C
    if (symb != "C" && is_prev(n, symb)) {
      n <- n + 1L
      next
    }
    add <- switch(EXPR = symb,
      "A" = , "E" = , "I" = , "O" = , "U" = {
        # A vowel is kept only at the start
        if (n == 0L) symb else ""
      },
      "B" = {
        # Silent at the end, after M, as in "dumb"
        if (is_prev(n, "M") && is_last(n)) "" else "B"
      },
      "C" = {
        if (is_prev(n, "S") && !is_last(n) &&
            char_at(n + 1L) %in% frontv) {
          ""                                # SCI, SCE, SCY
        } else if (is_prev(n, "S") && is_next(n, "H")) {
          "K"                               # SCH
        } else if (region(n, "CIA") || is_next(n, "H")) {
          "X"                               # CIA, CH
        } else if (!is_last(n) && char_at(n + 1L) %in% frontv) {
          "S"                               # CI, CE, CY
        } else {
          "K"
        }
      },
      "D" = {
        if (!is_last(n + 1L) && is_next(n, "G") &&
            char_at(n + 2L) %in% frontv) {
          n <- n + 2L                       # DGE, DGI, DGY
          "J"
        } else {
          "T"
        }
      },
      "G" = {
        if (is_last(n + 1L) && is_next(n, "H")) {
          ""                                # GH at the end
        } else if (!is_last(n + 1L) && is_next(n, "H") &&
                   !is_vowel(n + 2L)) {
          ""                                # GH before a consonant
        } else if (n > 0L && (region(n, "GN") || region(n, "GNED"))) {
          ""                                # silent G
        } else if (!is_last(n) && char_at(n + 1L) %in% frontv &&
                   !is_prev(n, "G")) {
          "J"
        } else {
          "K"
        }
      },
      "H" = {
        if (is_last(n)) {
          ""                                # H at the end
        } else if (n > 0L && char_at(n - 1L) %in% varson) {
          ""                                # CH, SH, PH, TH, GH
        } else if (is_vowel(n + 1L)) {
          "H"
        } else {
          ""
        }
      },
      "K" = {
        if (n > 0L && is_prev(n, "C")) "" else "K"
      },
      "P" = {
        if (is_next(n, "H")) "F" else "P"
      },
      "Q" = "K",
      "S" = {
        if (region(n, "SH") || region(n, "SIO") || region(n, "SIA")) {
          "X"
        } else {
          "S"
        }
      },
      "T" = {
        if (region(n, "TIA") || region(n, "TIO")) {
          "X"
        } else if (region(n, "TCH")) {
          ""                                # silent in TCH
        } else if (region(n, "TH")) {
          "0"                               # zero, for the "th" sound
        } else {
          "T"
        }
      },
      "V" = "F",
      "W" = , "Y" = {
        # Kept only before a vowel
        if (!is_last(n) && is_vowel(n + 1L)) symb else ""
      },
      "X" = "KS",
      "Z" = "S",
      "F" = , "J" = , "L" = , "M" = , "N" = , "R" = symb,
      ""
    )
    code <- c(code, add)
    n <- n + 1L
  }
  paste(code, collapse = "")
}
