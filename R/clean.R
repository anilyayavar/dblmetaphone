# Text preparation shared by metaphone() and double_metaphone().

# Accented Latin letters and their plain equivalents. Written with \u
# escapes so that the source file stays ASCII, as CRAN requires.
accent_from <- paste0(
  "\u00C0\u00C1\u00C2\u00C3\u00C4\u00C5\u0100\u0102\u0104",
  "\u00C8\u00C9\u00CA\u00CB\u0112\u0116\u0118\u011A",
  "\u00CC\u00CD\u00CE\u00CF\u012A\u012E\u0130",
  "\u00D2\u00D3\u00D4\u00D5\u00D6\u00D8\u014C\u0150",
  "\u00D9\u00DA\u00DB\u00DC\u016A\u016E\u0170\u0172",
  "\u00DD\u0178",
  "\u0106\u010C\u010A",
  "\u010E\u0110\u00D0",
  "\u011E\u0122\u0120",
  "\u0136",
  "\u0139\u013B\u013D\u0141",
  "\u0143\u0145\u0147",
  "\u0154\u0158",
  "\u015A\u0160\u015E\u0218",
  "\u0164\u0162\u021A",
  "\u0179\u017D\u017B"
)
accent_to <- paste0(
  "AAAAAAAAA",
  "EEEEEEEE",
  "IIIIIII",
  "OOOOOOOO",
  "UUUUUUUU",
  "YY",
  "CCC",
  "DDD",
  "GGG",
  "K",
  "LLLL",
  "NNN",
  "RR",
  "SSSS",
  "TTT",
  "ZZZ"
)

# Add the lower-case forms, so that the result does not depend on how
# toupper() treats accented letters in the current locale. In Latin-1
# the lower-case letter is 0x20 above the capital. In Latin Extended-A
# it is the next code point. Y with diaeresis is the one exception.
lower_case_of <- function(cp) {
  ifelse(cp >= 0xC0 & cp <= 0xDE, cp + 0x20,
         ifelse(cp == 0x178, 0xFF, cp + 1))
}
accent_from <- paste0(accent_from,
                      intToUtf8(lower_case_of(utf8ToInt(accent_from))))
accent_to <- paste0(accent_to, accent_to)

cedilla <- "\u00C7" # C with cedilla, sounded as S
n_tilde <- "\u00D1" # N with tilde, sounded as N

# Upper-case the text, replace accented letters, and reduce everything
# that is not a letter to single spaces.
#
# Double Metaphone has its own rules for the cedilla and the n with
# tilde, so `keep_special = TRUE` leaves those two letters in place.
# Otherwise they become S and N.
clean_text <- function(x, keep_special = FALSE) {
  x <- enc2utf8(x)
  x <- gsub("\u00DF", "SS", x)                          # sharp s
  x <- gsub("[\u00C6\u00E6\u01FC\u01FD]", "AE", x)      # ash
  x <- gsub("[\u0152\u0153]", "OE", x)                  # oe ligature
  x <- gsub("[\u00DE\u00FE]", "TH", x)                  # thorn
  x <- chartr(accent_from, accent_to, x)
  x <- chartr("\u00E7\u00F1", paste0(cedilla, n_tilde), x)
  x <- toupper(x)
  if (!keep_special) {
    x <- chartr(paste0(cedilla, n_tilde), "SN", x)
  }
  # Apostrophes join a name together, as in O'Brien or D'Souza.
  x <- gsub("['\u2019\u2018`]", "", x)
  x <- gsub(paste0("[^A-Z", cedilla, n_tilde, "]+"), " ", x)
  trimws(x)
}

# Check the shared arguments of the exported functions.
check_args <- function(max_length, by_word) {
  if (!is.numeric(max_length) || length(max_length) != 1L ||
      is.na(max_length) || max_length < 1) {
    stop("`max_length` must be a single number of at least 1, or Inf.",
         call. = FALSE)
  }
  if (!is.logical(by_word) || length(by_word) != 1L || is.na(by_word)) {
    stop("`by_word` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

# Cut a code to `max_length` characters. Inf means no limit.
cut_code <- function(code, max_length) {
  if (is.finite(max_length)) substr(code, 1L, max_length) else code
}

# Encode one cleaned string, either as a whole or word by word.
# `engine` takes one cleaned string and returns a character vector of
# codes (one for Metaphone, two for Double Metaphone).
encode_one <- function(text, engine, by_word, max_length) {
  if (!by_word) {
    return(cut_code(engine(text), max_length))
  }
  words <- strsplit(text, " ", fixed = TRUE)[[1L]]
  words <- words[nzchar(words)]
  if (length(words) == 0L) {
    return(engine(""))
  }
  codes <- vapply(words, function(w) cut_code(engine(w), max_length),
                  engine(""), USE.NAMES = FALSE)
  if (is.matrix(codes)) {
    apply(codes, 1L, function(r) paste(r[nzchar(r)], collapse = " "))
  } else {
    paste(codes[nzchar(codes)], collapse = " ")
  }
}

# Run an engine over a whole vector. Each distinct value is encoded once,
# which saves a lot of time on large name columns with repeats.
# Returns a matrix with one row per element of `x`.
encode_vector <- function(x, engine, n_codes, keep_special, by_word,
                          max_length) {
  if (is.null(x)) x <- character(0)
  if (!is.atomic(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  x <- as.character(x)
  out <- matrix(NA_character_, nrow = length(x), ncol = n_codes)
  ok <- !is.na(x)
  if (!any(ok)) {
    return(out)
  }
  cleaned <- clean_text(x[ok], keep_special = keep_special)
  distinct <- unique(cleaned)
  codes <- vapply(distinct, encode_one, character(n_codes),
                  engine = engine, by_word = by_word,
                  max_length = max_length, USE.NAMES = FALSE)
  codes <- matrix(codes, ncol = n_codes, byrow = TRUE)
  out[ok, ] <- codes[match(cleaned, distinct), , drop = FALSE]
  out
}
