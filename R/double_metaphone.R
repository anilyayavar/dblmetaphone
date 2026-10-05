#' Double Metaphone codes, in full
#'
#' Encodes names or words with the Double Metaphone algorithm of Lawrence
#' Philips (2000). Each input gets two codes. The primary code is the most
#' likely pronunciation. The secondary (alternate) code allows for another
#' common pronunciation, for example of a name from another language.
#'
#' Most implementations, including `PGRdup::DoubleMetaphone()`, keep only
#' the first four characters of each code. Here the codes are kept in full
#' by default, so "Venkatesh" and "Venkataraman" no longer get the same
#' code. Set `max_length = 4` to get the traditional short codes.
#'
#' The rules follow the C implementation by Maurice Aubrey, which is based
#' on Philips' own C++ code. That is the version used by most other
#' software.
#'
#' @section Preparing the text:
#' Letters are changed to capitals, and common accented Latin letters are
#' replaced by plain ones, so a u with an umlaut is read as U. The C with
#' a cedilla and the N with a tilde keep their own Double Metaphone rules
#' and are read as S and N. Apostrophes are
#' removed, so "O'Brien" is read as "OBRIEN". Any other character that is
#' not a letter, such as a digit, hyphen or full stop, is treated as a
#' space. Names written in other scripts, such as Devanagari, should be
#' transliterated to Latin letters first.
#'
#' @section Several words:
#' By default the whole text is encoded as one string, exactly as the
#' original algorithm does. Spaces are kept and some rules use them, for
#' example "San Jacinto" or "Van Damme". The code then runs the words
#' together. Set `by_word = TRUE` to encode each word on its own instead.
#' The word codes are then joined with single spaces, so "Ramesh Kumar"
#' gives `"RMX KMR"`. This makes it easy to compare names whose parts are
#' written in a different order.
#'
#' @param x A character vector of names or words. Factors are accepted.
#' @param max_length The largest number of characters to keep in each
#'   code. The default `Inf` keeps the full code. With `by_word = TRUE`
#'   the limit applies to each word separately.
#' @param by_word If `TRUE`, encode each word separately and join the codes
#'   with spaces. See the section on several words.
#'
#' @return A data frame with one row for each element of `x` and two
#'   character columns, `primary` and `secondary`. Missing values in `x`
#'   give `NA` in both columns. A value with no codable letters gives
#'   empty strings.
#'
#' @references
#' Philips, L. (2000). The double metaphone search algorithm. *C/C++
#' Users Journal*, 18(6), 38-43.
#'
#' @seealso [metaphone()] for the original single-code algorithm, and
#'   [sounds_like()] to compare two sets of names.
#'
#' @examples
#' double_metaphone(c("Smith", "Schmidt", "Thompson", "Agarwal", "Aggarwal"))
#'
#' # Full codes keep long names apart
#' double_metaphone(c("Venkatesh", "Venkataraman"))
#' double_metaphone(c("Venkatesh", "Venkataraman"), max_length = 4)
#'
#' # Encode each part of a name separately
#' double_metaphone("Ramesh Kumar Sharma", by_word = TRUE)
#'
#' # Add the codes to a data frame
#' vendors <- data.frame(name = c("Shree Ganesh Traders", "Sri Ganesh Trader"))
#' cbind(vendors, double_metaphone(vendors$name))
#' @export
double_metaphone <- function(x, max_length = Inf, by_word = FALSE) {
  check_args(max_length, by_word)
  codes <- encode_vector(x, dm_engine, n_codes = 2L, keep_special = TRUE,
                         by_word = by_word, max_length = max_length)
  data.frame(primary = codes[, 1L], secondary = codes[, 2L],
             stringsAsFactors = FALSE)
}

# Double Metaphone for one cleaned, upper-case string.
#
# This is a line-by-line translation of the C implementation by Maurice
# Aubrey (with fixes by Kevin Atkinson), which follows Lawrence Philips'
# C++ original. Positions are kept 0-based, as in the C code, so the two
# can be compared rule by rule. The C code stops after four characters;
# this version runs to the end of the string.
#
# Returns c(primary, secondary).
dm_engine <- function(word) {
  length <- nchar(word)
  if (length == 0L) {
    return(c("", ""))
  }
  last <- length - 1L

  # The C code pads the string with five spaces so that it can look past
  # the end safely. We do the same.
  padded <- paste0(word, "     ")
  padded_length <- length + 5L
  chars <- strsplit(padded, "", fixed = TRUE)[[1L]]

  get_at <- function(pos) {
    if (pos < 0L || pos >= padded_length) "" else chars[pos + 1L]
  }
  string_at <- function(start, n, ...) {
    if (start < 0L || start >= padded_length) {
      return(FALSE)
    }
    substr(padded, start + 1L, start + n) %in% c(...)
  }
  is_vowel <- function(pos) {
    if (pos < 0L || pos >= padded_length) {
      return(FALSE)
    }
    chars[pos + 1L] %in% c("A", "E", "I", "O", "U", "Y")
  }
  slavo_germanic <- grepl("W|K|CZ", word)

  primary <- ""
  secondary <- ""
  add <- function(main, alt = main) {
    primary <<- paste0(primary, main)
    secondary <<- paste0(secondary, alt)
  }

  current <- 0L

  # Skip these letters when at the start of a word.
  if (string_at(0L, 2L, "GN", "KN", "PN", "WR", "PS")) {
    current <- current + 1L
  }
  # Initial X is pronounced Z, as in Xavier. Z maps to S.
  if (get_at(0L) == "X") {
    add("S")
    current <- current + 1L
  }

  # In the C code each case ends with `break`. Here `next` does the same
  # job, since nothing follows the switch inside the loop.
  while (current < length) {
    ch <- get_at(current)
    if (ch == cedilla) ch <- "CEDILLA"
    if (ch == n_tilde) ch <- "NTILDE"

    switch(EXPR = ch,
      "A" = , "E" = , "I" = , "O" = , "U" = , "Y" = {
        # All initial vowels map to A.
        if (current == 0L) add("A")
        current <- current + 1L
      },

      "B" = {
        # "-mb", as in "dumb", is already skipped over.
        add("P")
        current <- current + if (get_at(current + 1L) == "B") 2L else 1L
      },

      "CEDILLA" = {
        add("S")
        current <- current + 1L
      },

      "C" = {
        # Various Germanic
        if (current > 1L &&
            !is_vowel(current - 2L) &&
            string_at(current - 1L, 3L, "ACH") &&
            get_at(current + 2L) != "I" &&
            (get_at(current + 2L) != "E" ||
             string_at(current - 2L, 6L, "BACHER", "MACHER"))) {
          add("K")
          current <- current + 2L
          next
        }
        # Special case "caesar"
        if (current == 0L && string_at(current, 6L, "CAESAR")) {
          add("S")
          current <- current + 2L
          next
        }
        # Italian "chianti"
        if (string_at(current, 4L, "CHIA")) {
          add("K")
          current <- current + 2L
          next
        }
        if (string_at(current, 2L, "CH")) {
          # Find "michael"
          if (current > 0L && string_at(current, 4L, "CHAE")) {
            add("K", "X")
            current <- current + 2L
            next
          }
          # Greek roots, as in "chemistry", "chorus"
          if (current == 0L &&
              (string_at(current + 1L, 5L, "HARAC", "HARIS") ||
               string_at(current + 1L, 3L, "HOR", "HYM", "HIA", "HEM")) &&
              !string_at(0L, 5L, "CHORE")) {
            add("K")
            current <- current + 2L
            next
          }
          # Germanic, Greek, or otherwise "ch" for the "kh" sound
          if (string_at(0L, 4L, "VAN ", "VON ") ||
              string_at(0L, 3L, "SCH") ||
              # "architect" but not "arch", "orchestra", "orchid"
              string_at(current - 2L, 6L, "ORCHES", "ARCHIT", "ORCHID") ||
              string_at(current + 2L, 1L, "T", "S") ||
              ((string_at(current - 1L, 1L, "A", "O", "U", "E") ||
                current == 0L) &&
               # "wachtler", "wechsler", but not "tichner"
               string_at(current + 2L, 1L, "L", "R", "N", "M", "B", "H",
                         "F", "V", "W", " "))) {
            add("K")
          } else if (current > 0L) {
            if (string_at(0L, 2L, "MC")) {
              add("K") # "McHugh"
            } else {
              add("X", "K")
            }
          } else {
            add("X")
          }
          current <- current + 2L
          next
        }
        # "czerny"
        if (string_at(current, 2L, "CZ") &&
            !string_at(current - 2L, 4L, "WICZ")) {
          add("S", "X")
          current <- current + 2L
          next
        }
        # "focaccia"
        if (string_at(current + 1L, 3L, "CIA")) {
          add("X")
          current <- current + 3L
          next
        }
        # Double C, but not as in "McClellan"
        if (string_at(current, 2L, "CC") &&
            !(current == 1L && get_at(0L) == "M")) {
          # "bellocchio" but not "bacchus"
          if (string_at(current + 2L, 1L, "I", "E", "H") &&
              !string_at(current + 2L, 2L, "HU")) {
            # "accident", "accede", "succeed"
            if ((current == 1L && get_at(current - 1L) == "A") ||
                string_at(current - 1L, 5L, "UCCEE", "UCCES")) {
              add("KS")
            } else {
              # "bacci", "bertucci", other Italian
              add("X")
            }
            current <- current + 3L
            next
          } else {
            # Pierce's rule
            add("K")
            current <- current + 2L
            next
          }
        }
        if (string_at(current, 2L, "CK", "CG", "CQ")) {
          add("K")
          current <- current + 2L
          next
        }
        if (string_at(current, 2L, "CI", "CE", "CY")) {
          # Italian against English
          if (string_at(current, 3L, "CIO", "CIE", "CIA")) {
            add("S", "X")
          } else {
            add("S")
          }
          current <- current + 2L
          next
        }
        add("K")
        # Name sent in "mac caffrey", "mac gregor"
        if (string_at(current + 1L, 2L, " C", " Q", " G")) {
          current <- current + 3L
        } else if (string_at(current + 1L, 1L, "C", "K", "Q") &&
                   !string_at(current + 1L, 2L, "CE", "CI")) {
          current <- current + 2L
        } else {
          current <- current + 1L
        }
      },

      "D" = {
        if (string_at(current, 2L, "DG")) {
          if (string_at(current + 2L, 1L, "I", "E", "Y")) {
            add("J") # "edge"
            current <- current + 3L
          } else {
            add("TK") # "edgar"
            current <- current + 2L
          }
          next
        }
        if (string_at(current, 2L, "DT", "DD")) {
          add("T")
          current <- current + 2L
          next
        }
        add("T")
        current <- current + 1L
      },

      "F" = {
        current <- current + if (get_at(current + 1L) == "F") 2L else 1L
        add("F")
      },

      "G" = {
        if (get_at(current + 1L) == "H") {
          if (current > 0L && !is_vowel(current - 1L)) {
            add("K")
            current <- current + 2L
            next
          }
          # "ghislane", "ghiradelli"
          if (current == 0L) {
            if (get_at(current + 2L) == "I") add("J") else add("K")
            current <- current + 2L
            next
          }
          # Parker's rule, with some further refinements
          if ((current > 1L &&
               string_at(current - 2L, 1L, "B", "H", "D")) ||   # "hugh"
              (current > 2L &&
               string_at(current - 3L, 1L, "B", "H", "D")) ||   # "bough"
              (current > 3L &&
               string_at(current - 4L, 1L, "B", "H"))) {        # "broughton"
            current <- current + 2L
            next
          }
          # "laugh", "McLaughlin", "cough", "gough", "rough", "tough"
          if (current > 2L &&
              get_at(current - 1L) == "U" &&
              string_at(current - 3L, 1L, "C", "G", "L", "R", "T")) {
            add("F")
          } else if (current > 0L && get_at(current - 1L) != "I") {
            add("K")
          }
          current <- current + 2L
          next
        }

        if (get_at(current + 1L) == "N") {
          if (current == 1L && is_vowel(0L) && !slavo_germanic) {
            add("KN", "N")
          } else if (!string_at(current + 2L, 2L, "EY") &&
                     get_at(current + 1L) != "Y" &&
                     !slavo_germanic) {
            # Not as in "cagney"
            add("N", "KN")
          } else {
            add("KN")
          }
          current <- current + 2L
          next
        }
        # "tagliaro"
        if (string_at(current + 1L, 2L, "LI") && !slavo_germanic) {
          add("KL", "L")
          current <- current + 2L
          next
        }
        # -ges-, -gep-, -gel-, -gie- at the beginning
        if (current == 0L &&
            (get_at(current + 1L) == "Y" ||
             string_at(current + 1L, 2L, "ES", "EP", "EB", "EL", "EY",
                       "IB", "IL", "IN", "IE", "EI", "ER"))) {
          add("K", "J")
          current <- current + 2L
          next
        }
        # -ger-, -gy-
        if ((string_at(current + 1L, 2L, "ER") ||
             get_at(current + 1L) == "Y") &&
            !string_at(0L, 6L, "DANGER", "RANGER", "MANGER") &&
            !string_at(current - 1L, 1L, "E", "I") &&
            !string_at(current - 1L, 3L, "RGY", "OGY")) {
          add("K", "J")
          current <- current + 2L
          next
        }
        # Italian, as in "biaggi"
        if (string_at(current + 1L, 1L, "E", "I", "Y") ||
            string_at(current - 1L, 4L, "AGGI", "OGGI")) {
          if (string_at(0L, 4L, "VAN ", "VON ") ||
              string_at(0L, 3L, "SCH") ||
              string_at(current + 1L, 2L, "ET")) {
            add("K") # obviously Germanic
          } else if (string_at(current + 1L, 4L, "IER ")) {
            add("J") # always soft with a French ending
          } else {
            add("J", "K")
          }
          current <- current + 2L
          next
        }
        current <- current + if (get_at(current + 1L) == "G") 2L else 1L
        add("K")
      },

      "H" = {
        # Keep only if first and before a vowel, or between two vowels.
        if ((current == 0L || is_vowel(current - 1L)) &&
            is_vowel(current + 1L)) {
          add("H")
          current <- current + 2L
        } else {
          current <- current + 1L # also takes care of "HH"
        }
      },

      "J" = {
        # Obviously Spanish, "jose", "san jacinto"
        if (string_at(current, 4L, "JOSE") || string_at(0L, 4L, "SAN ")) {
          if ((current == 0L && get_at(current + 4L) == " ") ||
              string_at(0L, 4L, "SAN ")) {
            add("H")
          } else {
            add("J", "H")
          }
          current <- current + 1L
          next
        }
        if (current == 0L && !string_at(current, 4L, "JOSE")) {
          add("J", "A") # "Yankelovich" against "Jankelowicz"
        } else if (is_vowel(current - 1L) &&
                   !slavo_germanic &&
                   get_at(current + 1L) %in% c("A", "O")) {
          add("J", "H") # Spanish pronunciation of "bajador"
        } else if (current == last) {
          add("J", "")
        } else if (!string_at(current + 1L, 1L, "L", "T", "K", "S", "N",
                              "M", "B", "Z") &&
                   !string_at(current - 1L, 1L, "S", "K", "L")) {
          add("J")
        }
        # "JJ" could happen
        current <- current + if (get_at(current + 1L) == "J") 2L else 1L
      },

      "K" = {
        current <- current + if (get_at(current + 1L) == "K") 2L else 1L
        add("K")
      },

      "L" = {
        if (get_at(current + 1L) == "L") {
          # Spanish, as in "cabrillo", "gallegos"
          if ((current == length - 3L &&
               string_at(current - 1L, 4L, "ILLO", "ILLA", "ALLE")) ||
              ((string_at(last - 1L, 2L, "AS", "OS") ||
                string_at(last, 1L, "A", "O")) &&
               string_at(current - 1L, 4L, "ALLE"))) {
            add("L", "")
            current <- current + 2L
            next
          }
          current <- current + 2L
        } else {
          current <- current + 1L
        }
        add("L")
      },

      "M" = {
        if ((string_at(current - 1L, 3L, "UMB") &&
             (current + 1L == last ||
              string_at(current + 2L, 2L, "ER"))) ||  # "dumb", "thumb"
            get_at(current + 1L) == "M") {
          current <- current + 2L
        } else {
          current <- current + 1L
        }
        add("M")
      },

      "N" = {
        current <- current + if (get_at(current + 1L) == "N") 2L else 1L
        add("N")
      },

      "NTILDE" = {
        current <- current + 1L
        add("N")
      },

      "P" = {
        if (get_at(current + 1L) == "H") {
          add("F")
          current <- current + 2L
          next
        }
        # Also "campbell", "raspberry"
        if (string_at(current + 1L, 1L, "P", "B")) {
          current <- current + 2L
        } else {
          current <- current + 1L
        }
        add("P")
      },

      "Q" = {
        current <- current + if (get_at(current + 1L) == "Q") 2L else 1L
        add("K")
      },

      "R" = {
        # French, as in "rogier", but not "hochmeier"
        if (current == last &&
            !slavo_germanic &&
            string_at(current - 2L, 2L, "IE") &&
            !string_at(current - 4L, 2L, "ME", "MA")) {
          add("", "R")
        } else {
          add("R")
        }
        current <- current + if (get_at(current + 1L) == "R") 2L else 1L
      },

      "S" = {
        # "island", "isle", "carlisle", "carlysle"
        if (string_at(current - 1L, 3L, "ISL", "YSL")) {
          current <- current + 1L
          next
        }
        # "sugar-"
        if (current == 0L && string_at(current, 5L, "SUGAR")) {
          add("X", "S")
          current <- current + 1L
          next
        }
        if (string_at(current, 2L, "SH")) {
          # Germanic
          if (string_at(current + 1L, 4L, "HEIM", "HOEK", "HOLM", "HOLZ")) {
            add("S")
          } else {
            add("X")
          }
          current <- current + 2L
          next
        }
        # Italian and Armenian
        if (string_at(current, 3L, "SIO", "SIA") ||
            string_at(current, 4L, "SIAN")) {
          if (!slavo_germanic) add("S", "X") else add("S")
          current <- current + 3L
          next
        }
        # German and anglicised forms, so "smith" matches "schmidt" and
        # "snider" matches "schneider". Also -sz- in Slavic languages,
        # though in Hungarian it is pronounced S.
        if ((current == 0L &&
             string_at(current + 1L, 1L, "M", "N", "L", "W")) ||
            string_at(current + 1L, 1L, "Z")) {
          add("S", "X")
          current <- current + if (string_at(current + 1L, 1L, "Z")) 2L else 1L
          next
        }
        if (string_at(current, 2L, "SC")) {
          # Schlesinger's rule
          if (get_at(current + 2L) == "H") {
            # Dutch origin, as in "school", "schooner"
            if (string_at(current + 3L, 2L, "OO", "ER", "EN", "UY", "ED",
                          "EM")) {
              # "schermerhorn", "schenker"
              if (string_at(current + 3L, 2L, "ER", "EN")) {
                add("X", "SK")
              } else {
                add("SK")
              }
            } else if (current == 0L && !is_vowel(3L) &&
                       get_at(3L) != "W") {
              add("X", "S")
            } else {
              add("X")
            }
            current <- current + 3L
            next
          }
          if (string_at(current + 2L, 1L, "I", "E", "Y")) {
            add("S")
          } else {
            add("SK")
          }
          current <- current + 3L
          next
        }
        # French, as in "resnais", "artois"
        if (current == last && string_at(current - 2L, 2L, "AI", "OI")) {
          add("", "S")
        } else {
          add("S")
        }
        current <- current +
          if (string_at(current + 1L, 1L, "S", "Z")) 2L else 1L
      },

      "T" = {
        if (string_at(current, 4L, "TION")) {
          add("X")
          current <- current + 3L
          next
        }
        if (string_at(current, 3L, "TIA", "TCH")) {
          add("X")
          current <- current + 3L
          next
        }
        if (string_at(current, 2L, "TH") || string_at(current, 3L, "TTH")) {
          # "thomas", "thames", or Germanic
          if (string_at(current + 2L, 2L, "OM", "AM") ||
              string_at(0L, 4L, "VAN ", "VON ") ||
              string_at(0L, 3L, "SCH")) {
            add("T")
          } else {
            add("0", "T") # yes, a zero, for the "th" sound
          }
          current <- current + 2L
          next
        }
        current <- current +
          if (string_at(current + 1L, 1L, "T", "D")) 2L else 1L
        add("T")
      },

      "V" = {
        current <- current + if (get_at(current + 1L) == "V") 2L else 1L
        add("F")
      },

      "W" = {
        # Can also be in the middle of a word
        if (string_at(current, 2L, "WR")) {
          add("R")
          current <- current + 2L
          next
        }
        if (current == 0L &&
            (is_vowel(current + 1L) || string_at(current, 2L, "WH"))) {
          if (is_vowel(current + 1L)) {
            add("A", "F") # "Wasserman" should match "Vasserman"
          } else {
            add("A")      # "Uomo" should match "Womo"
          }
        }
        # "Arnow" should match "Arnoff"
        if ((current == last && is_vowel(current - 1L)) ||
            string_at(current - 1L, 5L, "EWSKI", "EWSKY", "OWSKI",
                      "OWSKY") ||
            string_at(0L, 3L, "SCH")) {
          add("", "F")
          current <- current + 1L
          next
        }
        # Polish, as in "filipowicz"
        if (string_at(current, 4L, "WICZ", "WITZ")) {
          add("TS", "FX")
          current <- current + 4L
          next
        }
        current <- current + 1L # otherwise skip it
      },

      "X" = {
        # French, as in "breaux"
        if (!(current == last &&
              (string_at(current - 3L, 3L, "IAU", "EAU") ||
               string_at(current - 2L, 2L, "AU", "OU")))) {
          add("KS")
        }
        current <- current +
          if (string_at(current + 1L, 1L, "C", "X")) 2L else 1L
      },

      "Z" = {
        # Chinese pinyin, as in "zhao"
        if (get_at(current + 1L) == "H") {
          add("J")
          current <- current + 2L
          next
        }
        if (string_at(current + 1L, 2L, "ZO", "ZI", "ZA") ||
            (slavo_germanic && current > 0L &&
             get_at(current - 1L) != "T")) {
          add("S", "TS")
        } else {
          add("S")
        }
        current <- current + if (get_at(current + 1L) == "Z") 2L else 1L
      },

      # Anything else, including spaces
      current <- current + 1L
    )
  }

  c(primary, secondary)
}
