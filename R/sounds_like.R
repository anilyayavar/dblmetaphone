#' Do two names sound alike?
#'
#' Compares two vectors of names element by element and reports whether
#' each pair gets the same phonetic code.
#'
#' With `method = "double"`, a pair matches when any Double Metaphone code
#' of one name (primary or secondary) equals any code of the other. This
#' is the comparison Philips recommended. With `method = "metaphone"`, the
#' single Metaphone codes must be equal.
#'
#' A name with no codable letters, such as `""` or `"123"`, never matches.
#'
#' @param x,y Character vectors of names. They are recycled to a common
#'   length in the usual way, so one of them may be a single name.
#' @param method Either `"double"` (the default) for Double Metaphone or
#'   `"metaphone"` for the original Metaphone.
#' @inheritParams double_metaphone
#'
#' @return A logical vector. `NA` where either name is missing.
#'
#' @seealso [double_metaphone()], [metaphone()], and
#'   `vignette("fullmetaphone")` for worked examples of de-duplication and
#'   record linkage.
#'
#' @examples
#' sounds_like("Meyer", c("Meier", "Mayer", "Maier", "Miller"))
#'
#' sounds_like("Smith", "Schmidt")
#' sounds_like("Smith", "Schmidt", method = "metaphone")
#'
#' # Full codes tell long names apart. Four-character codes do not.
#' sounds_like("Christopher Anderson", "Christina Andrews")
#' sounds_like("Christopher Anderson", "Christina Andrews", max_length = 4)
#'
#' # Search a register of names
#' hits <- us_surnames[sounds_like("Schneider", us_surnames$surname), ]
#' head(hits)
#' @export
sounds_like <- function(x, y, method = c("double", "metaphone"),
                        max_length = 32, by_word = FALSE) {
  method <- match.arg(method)
  check_args(max_length, by_word)
  if (is.null(x)) x <- character(0)
  if (is.null(y)) y <- character(0)
  n <- if (length(x) == 0L || length(y) == 0L) {
    0L
  } else {
    max(length(x), length(y))
  }
  x <- rep_len(as.character(x), n)
  y <- rep_len(as.character(y), n)

  if (method == "metaphone") {
    cx <- metaphone(x, max_length = max_length, by_word = by_word)
    cy <- metaphone(y, max_length = max_length, by_word = by_word)
    out <- cx == cy & nzchar(cx)
  } else {
    cx <- double_metaphone(x, max_length = max_length, by_word = by_word)
    cy <- double_metaphone(y, max_length = max_length, by_word = by_word)
    same <- function(a, b) a == b & nzchar(a)
    out <- same(cx$primary, cy$primary) | same(cx$primary, cy$secondary) |
      same(cx$secondary, cy$primary) | same(cx$secondary, cy$secondary)
  }
  out[is.na(x) | is.na(y)] <- NA
  out
}
