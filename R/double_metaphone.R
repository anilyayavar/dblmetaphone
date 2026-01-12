#' Double Metaphone
#'
#' Double Metaphone phonetic encoding algorithm
#'
#' @param word Character string
#' @return Character vector of length 2 (primary, secondary)
#' @export
double_metaphone <- function(word) {
  # Constants
  max_length <- 32

  # Helper functions
  char_at <- function(s, pos) {
    if (pos < 1 || pos > nchar(s)) return("")
    substr(s, pos, pos)
  }

  string_at <- function(s, start, length, ...) {
    if (start < 1 || start > nchar(s)) return(FALSE)

    patterns <- list(...)
    substr_str <- substr(s, start, start + length - 1)

    for (pattern in patterns) {
      if (pattern == "") break
      if (substr(substr_str, 1, nchar(pattern)) == pattern) {
        return(TRUE)
      }
    }
    FALSE
  }

  is_vowel <- function(s, pos) {
    if (pos < 1 || pos > nchar(s)) return(FALSE)
    char <- char_at(s, pos)
    char %in% c("A", "E", "I", "O", "U", "Y")
  }

  is_slavo_germanic <- function(s) {
    grepl("W", s) || grepl("K", s) || grepl("CZ", s) || grepl("WITZ", s)
  }

  # Main function
  if (is.na(word) || !nzchar(word)) {
    return(c("", ""))
  }

  # Convert to uppercase
  original <- toupper(word)

  # Remove non-alphabetic characters
  original <- gsub("[^A-Z]", "", original)

  # Add padding
  original_padded <- paste0(original, "     ")

  length <- nchar(original)
  last <- length

  primary <- ""
  secondary <- ""
  current <- 1

  # Skip certain prefixes
  if (string_at(original_padded, current, 2, "GN", "KN", "PN", "WR", "PS", "")) {
    current <- current + 1
  }

  # Initial 'X' is pronounced 'Z' (which maps to 'S')
  if (char_at(original_padded, current) == "X") {
    primary <- paste0(primary, "S")
    secondary <- paste0(secondary, "S")
    current <- current + 1
  }

  # Main loop
  while (nchar(primary) < max_length || nchar(secondary) < max_length) {

    if (current > length) break

    prev_current <- current
    ch <- char_at(original_padded, current)

    switch(ch,
           "A" = ,
           "E" = ,
           "I" = ,
           "O" = ,
           "U" = ,
           "Y" = {
             if (current == 1) {
               primary <- paste0(primary, "A")
               secondary <- paste0(secondary, "A")
             }
             current <- current + 1
           },

           "B" = {
             primary <- paste0(primary, "P")
             secondary <- paste0(secondary, "P")
             if (char_at(original_padded, current + 1) == "B") {
               current <- current + 2
             } else {
               current <- current + 1
             }
           },

           "C" = {
             # Various Germanic
             if (current > 2 &&
                 !is_vowel(original_padded, current - 2) &&
                 string_at(original_padded, current - 1, 3, "ACH", "") &&
                 (char_at(original_padded, current + 2) != "I" &&
                  (char_at(original_padded, current + 2) != "E" ||
                   string_at(original_padded, current - 2, 6, "BACHER", "MACHER", "")))) {
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "K")
               current <- current + 2
             }
             # Special case 'caesar'
             else if (current == 1 && string_at(original_padded, current, 6, "CAESAR", "")) {
               primary <- paste0(primary, "S")
               secondary <- paste0(secondary, "S")
               current <- current + 2
             }
             # Italian 'chianti'
             else if (string_at(original_padded, current, 4, "CHIA", "")) {
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "K")
               current <- current + 2
             }
             else if (string_at(original_padded, current, 2, "CH", "")) {
               # Find 'michael'
               if (current > 1 && string_at(original_padded, current, 4, "CHAE", "")) {
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "X")
                 current <- current + 2
               }
               # Greek roots e.g. 'chemistry', 'chorus'
               else if (current == 1 &&
                        (string_at(original_padded, current + 1, 5, "HARAC", "HARIS", "") ||
                         string_at(original_padded, current + 1, 3, "HOR", "HYM", "HIA", "HEM", "")) &&
                        !string_at(original_padded, 1, 5, "CHORE", "")) {
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "K")
                 current <- current + 2
               }
               # Germanic, Greek, or otherwise 'ch' for 'kh' sound
               else if ((string_at(original_padded, 1, 4, "VAN ", "VON ", "") ||
                         string_at(original_padded, 1, 3, "SCH", "")) ||
                        # 'architect' but not 'arch', 'orchestra', 'orchid'
                        string_at(original_padded, current - 2, 6, "ORCHES", "ARCHIT", "ORCHID", "") ||
                        string_at(original_padded, current + 2, 1, "T", "S", "") ||
                        ((string_at(original_padded, current - 1, 1, "A", "O", "U", "E", "") ||
                          (current == 1)) &&
                         # e.g., 'wachtler', 'wechsler', but not 'tichner'
                         string_at(original_padded, current + 2, 1, "L", "R", "N", "M", "B", "H", "F", "V", "W", " ", ""))) {
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "K")
               } else {
                 if (current > 1) {
                   if (string_at(original_padded, 1, 2, "MC", "")) {
                     # e.g., "McHugh"
                     primary <- paste0(primary, "K")
                     secondary <- paste0(secondary, "K")
                   } else {
                     primary <- paste0(primary, "X")
                     secondary <- paste0(secondary, "K")
                   }
                 } else {
                   primary <- paste0(primary, "X")
                   secondary <- paste0(secondary, "X")
                 }
               }
               current <- current + 2
             }
             # e.g., 'czerny'
             else if (string_at(original_padded, current, 2, "CZ", "") &&
                      !string_at(original_padded, current - 2, 4, "WICZ", "")) {
               primary <- paste0(primary, "S")
               secondary <- paste0(secondary, "X")
               current <- current + 2
             }
             # e.g., 'focaccia'
             else if (string_at(original_padded, current + 1, 3, "CIA", "")) {
               primary <- paste0(primary, "X")
               secondary <- paste0(secondary, "X")
               current <- current + 3
             }
             # Double 'C', but not if e.g. 'McClellan'
             else if (string_at(original_padded, current, 2, "CC", "") &&
                      !((current == 2) && (char_at(original_padded, 1) == "M"))) {
               # 'bellocchio' but not 'bacchus'
               if (string_at(original_padded, current + 2, 1, "I", "E", "H", "") &&
                   !string_at(original_padded, current + 2, 2, "HU", "")) {
                 # 'accident', 'accede', 'succeed'
                 if (((current == 2) && (char_at(original_padded, current - 1) == "A")) ||
                     string_at(original_padded, current - 1, 5, "UCCEE", "UCCES", "")) {
                   primary <- paste0(primary, "KS")
                   secondary <- paste0(secondary, "KS")
                 } else {
                   # 'bacci', 'bertucci', other Italian
                   primary <- paste0(primary, "X")
                   secondary <- paste0(secondary, "X")
                 }
                 current <- current + 3
               } else {
                 # Pierce's rule
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "K")
                 current <- current + 2
               }
             }
             else if (string_at(original_padded, current, 2, "CK", "CG", "CQ", "")) {
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "K")
               current <- current + 2
             }
             else if (string_at(original_padded, current, 2, "CI", "CE", "CY", "")) {
               # Italian vs. English
               if (string_at(original_padded, current, 3, "CIO", "CIE", "CIA", "")) {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "X")
               } else {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "S")
               }
               current <- current + 2
             }
             else {
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "K")

               # Name sent in 'mac caffrey', 'mac gregor'
               if (string_at(original_padded, current + 1, 2, " C", " Q", " G", "")) {
                 current <- current + 3
               } else if (string_at(original_padded, current + 1, 1, "C", "K", "Q", "") &&
                          !string_at(original_padded, current + 1, 2, "CE", "CI", "")) {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
             }
           },

           "D" = {
             if (string_at(original_padded, current, 2, "DG", "")) {
               if (string_at(original_padded, current + 2, 1, "I", "E", "Y", "")) {
                 # e.g., 'edge'
                 primary <- paste0(primary, "J")
                 secondary <- paste0(secondary, "J")
                 current <- current + 3
               } else {
                 # e.g., 'edgar'
                 primary <- paste0(primary, "TK")
                 secondary <- paste0(secondary, "TK")
                 current <- current + 2
               }
             } else if (string_at(original_padded, current, 2, "DT", "DD", "")) {
               primary <- paste0(primary, "T")
               secondary <- paste0(secondary, "T")
               current <- current + 2
             } else {
               primary <- paste0(primary, "T")
               secondary <- paste0(secondary, "T")
               current <- current + 1
             }
           },

           "F" = {
             if (char_at(original_padded, current + 1) == "F") {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "F")
             secondary <- paste0(secondary, "F")
           },

           "G" = {
             if (char_at(original_padded, current + 1) == "H") {
               if (current > 1 && !is_vowel(original_padded, current - 1)) {
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "K")
                 current <- current + 2
               } else if (current < 4) {
                 # 'ghislane', 'ghiradelli'
                 if (current == 1) {
                   if (char_at(original_padded, current + 2) == "I") {
                     primary <- paste0(primary, "J")
                     secondary <- paste0(secondary, "J")
                   } else {
                     primary <- paste0(primary, "K")
                     secondary <- paste0(secondary, "K")
                   }
                   current <- current + 2
                 }
               } else {
                 # Parker's rule (with some further refinements)
                 if (((current > 2) &&
                      string_at(original_padded, current - 2, 1, "B", "H", "D", "")) ||
                     ((current > 3) &&
                      string_at(original_padded, current - 3, 1, "B", "H", "D", "")) ||
                     ((current > 4) &&
                      string_at(original_padded, current - 4, 1, "B", "H", ""))) {
                   current <- current + 2
                 } else {
                   # e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
                   if ((current > 3) &&
                       (char_at(original_padded, current - 1) == "U") &&
                       string_at(original_padded, current - 3, 1, "C", "G", "L", "R", "T", "")) {
                     primary <- paste0(primary, "F")
                     secondary <- paste0(secondary, "F")
                   } else if (current > 1 && char_at(original_padded, current - 1) != "I") {
                     primary <- paste0(primary, "K")
                     secondary <- paste0(secondary, "K")
                   }
                   current <- current + 2
                 }
               }
             } else if (char_at(original_padded, current + 1) == "N") {
               if (current == 2 && is_vowel(original_padded, 1) && !is_slavo_germanic(original)) {
                 primary <- paste0(primary, "KN")
                 secondary <- paste0(secondary, "N")
               } else if (!string_at(original_padded, current + 2, 2, "EY", "") &&
                          (char_at(original_padded, current + 1) != "Y") &&
                          !is_slavo_germanic(original)) {
                 primary <- paste0(primary, "N")
                 secondary <- paste0(secondary, "KN")
               } else {
                 primary <- paste0(primary, "KN")
                 secondary <- paste0(secondary, "KN")
               }
               current <- current + 2
             } else if (string_at(original_padded, current + 1, 2, "LI", "") &&
                        !is_slavo_germanic(original)) {
               # 'tagliaro'
               primary <- paste0(primary, "KL")
               secondary <- paste0(secondary, "L")
               current <- current + 2
             } else if ((current == 1) &&
                        ((char_at(original_padded, current + 1) == "Y") ||
                         string_at(original_padded, current + 1, 2, "ES", "EP", "EB", "EL", "EY", "IB", "IL", "IN", "IE", "EI", "ER", ""))) {
               # -ges-, -gep-, -gel-, -gie- at beginning
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "J")
               current <- current + 2
             } else if ((string_at(original_padded, current + 1, 2, "ER", "") ||
                         (char_at(original_padded, current + 1) == "Y")) &&
                        !string_at(original_padded, 1, 6, "DANGER", "RANGER", "MANGER", "") &&
                        !string_at(original_padded, current - 1, 1, "E", "I", "") &&
                        !string_at(original_padded, current - 1, 3, "RGY", "OGY", "")) {
               # -ger-, -gy-
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "J")
               current <- current + 2
             } else if (string_at(original_padded, current + 1, 1, "E", "I", "Y", "") ||
                        string_at(original_padded, current - 1, 4, "AGGI", "OGGI", "")) {
               # Italian e.g., 'biaggi'
               if ((string_at(original_padded, 1, 4, "VAN ", "VON ", "") ||
                    string_at(original_padded, 1, 3, "SCH", "")) ||
                   string_at(original_padded, current + 1, 2, "ET", "")) {
                 primary <- paste0(primary, "K")
                 secondary <- paste0(secondary, "K")
               } else {
                 # Always soft if French ending
                 if (string_at(original_padded, current + 1, 4, "IER ", "")) {
                   primary <- paste0(primary, "J")
                   secondary <- paste0(secondary, "J")
                 } else {
                   primary <- paste0(primary, "J")
                   secondary <- paste0(secondary, "K")
                 }
               }
               current <- current + 2
             } else {
               if (char_at(original_padded, current + 1) == "G") {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
               primary <- paste0(primary, "K")
               secondary <- paste0(secondary, "K")
             }
           },

           "H" = {
             # Only keep if first & before vowel or between 2 vowels
             if ((current == 1 || is_vowel(original_padded, current - 1)) &&
                 is_vowel(original_padded, current + 1)) {
               primary <- paste0(primary, "H")
               secondary <- paste0(secondary, "H")
               current <- current + 2
             } else {
               # Also takes care of 'HH'
               current <- current + 1
             }
           },

           "J" = {
             # Obvious Spanish, 'jose', 'san jacinto'
             if (string_at(original_padded, current, 4, "JOSE", "") ||
                 string_at(original_padded, 1, 4, "SAN ", "")) {
               if (((current == 1) && (char_at(original_padded, current + 4) == " ")) ||
                   string_at(original_padded, 1, 4, "SAN ", "")) {
                 primary <- paste0(primary, "H")
                 secondary <- paste0(secondary, "H")
               } else {
                 primary <- paste0(primary, "J")
                 secondary <- paste0(secondary, "H")
               }
               current <- current + 1
             } else if (current == 1 && !string_at(original_padded, current, 4, "JOSE", "")) {
               # Yankelovich/Jankelowicz
               primary <- paste0(primary, "J")
               secondary <- paste0(secondary, "A")
               current <- current + 1
             } else {
               # Spanish pronunciation of e.g. 'bajador'
               if (is_vowel(original_padded, current - 1) &&
                   !is_slavo_germanic(original) &&
                   (char_at(original_padded, current + 1) == "A" ||
                    char_at(original_padded, current + 1) == "O")) {
                 primary <- paste0(primary, "J")
                 secondary <- paste0(secondary, "H")
               } else if (current == last) {
                 primary <- paste0(primary, "J")
                 secondary <- paste0(secondary, "")
                 current <- current + 1
               } else if (!string_at(original_padded, current + 1, 1,
                                     "L", "T", "K", "S", "N", "M", "B", "Z", "") &&
                          !string_at(original_padded, current - 1, 1, "S", "K", "L", "")) {
                 primary <- paste0(primary, "J")
                 secondary <- paste0(secondary, "J")
                 current <- current + 1
               } else {
                 current <- current + 1
               }
             }

             if (char_at(original_padded, current) == "J") {
               # It could happen!
               current <- current + 1
             }
           },

           "K" = {
             if (char_at(original_padded, current + 1) == "K") {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "K")
             secondary <- paste0(secondary, "K")
           },

           "L" = {
             if (char_at(original_padded, current + 1) == "L") {
               # Spanish e.g. 'cabrillo', 'gallegos'
               if (((current == (length - 2)) &&
                    string_at(original_padded, current - 1, 4, "ILLO", "ILLA", "ALLE", "")) ||
                   ((string_at(original_padded, last - 1, 2, "AS", "OS", "") ||
                     string_at(original_padded, last, 1, "A", "O", "")) &&
                    string_at(original_padded, current - 1, 4, "ALLE", ""))) {
                 primary <- paste0(primary, "L")
                 secondary <- paste0(secondary, "")
                 current <- current + 2
               } else {
                 current <- current + 2
               }
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "L")
             secondary <- paste0(secondary, "L")
           },

           "M" = {
             if ((string_at(original_padded, current - 1, 3, "UMB", "") &&
                  ((current + 1) == last ||
                   string_at(original_padded, current + 2, 2, "ER", ""))) ||
                 # 'dumb', 'thumb'
                 (char_at(original_padded, current + 1) == "M")) {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "M")
             secondary <- paste0(secondary, "M")
           },

           "N" = {
             if (char_at(original_padded, current + 1) == "N") {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "N")
             secondary <- paste0(secondary, "N")
           },

           "P" = {
             if (char_at(original_padded, current + 1) == "H") {
               primary <- paste0(primary, "F")
               secondary <- paste0(secondary, "F")
               current <- current + 2
             } else {
               # Also account for "campbell", "raspberry"
               if (string_at(original_padded, current + 1, 1, "P", "B", "")) {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
               primary <- paste0(primary, "P")
               secondary <- paste0(secondary, "P")
             }
           },

           "Q" = {
             if (char_at(original_padded, current + 1) == "Q") {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "K")
             secondary <- paste0(secondary, "K")
           },

           "R" = {
             # French e.g. 'rogier', but exclude 'hochmeier'
             if (current == last &&
                 !is_slavo_germanic(original) &&
                 string_at(original_padded, current - 2, 2, "IE", "") &&
                 !string_at(original_padded, current - 4, 2, "ME", "MA", "")) {
               primary <- paste0(primary, "")
               secondary <- paste0(secondary, "R")
             } else {
               primary <- paste0(primary, "R")
               secondary <- paste0(secondary, "R")
             }

             if (char_at(original_padded, current + 1) == "R") {
               current <- current + 2
             } else {
               current <- current + 1
             }
           },

           "S" = {
             # Special cases 'island', 'isle', 'carlisle', 'carlysle'
             if (string_at(original_padded, current - 1, 3, "ISL", "YSL", "")) {
               current <- current + 1
             }
             # Special case 'sugar-'
             else if (current == 1 && string_at(original_padded, current, 5, "SUGAR", "")) {
               primary <- paste0(primary, "X")
               secondary <- paste0(secondary, "S")
               current <- current + 1
             }
             else if (string_at(original_padded, current, 2, "SH", "")) {
               # Germanic
               if (string_at(original_padded, current + 1, 4, "HEIM", "HOEK", "HOLM", "HOLZ", "")) {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "S")
               } else {
                 primary <- paste0(primary, "X")
                 secondary <- paste0(secondary, "X")
               }
               current <- current + 2
             }
             # Italian & Armenian
             else if (string_at(original_padded, current, 3, "SIO", "SIA", "") ||
                      string_at(original_padded, current, 4, "SIAN", "")) {
               if (!is_slavo_germanic(original)) {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "X")
               } else {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "S")
               }
               current <- current + 3
             }
             # German & anglicisations
             else if ((current == 1 &&
                       string_at(original_padded, current + 1, 1, "M", "N", "L", "W", "")) ||
                      string_at(original_padded, current + 1, 1, "Z", "")) {
               primary <- paste0(primary, "S")
               secondary <- paste0(secondary, "X")
               if (string_at(original_padded, current + 1, 1, "Z", "")) {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
             }
             else if (string_at(original_padded, current, 2, "SC", "")) {
               # Schlesinger's rule
               if (char_at(original_padded, current + 2) == "H") {
                 # Dutch origin, e.g. 'school', 'schooner'
                 if (string_at(original_padded, current + 3, 2,
                               "OO", "ER", "EN", "UY", "ED", "EM", "")) {
                   # 'schermerhorn', 'schenker'
                   if (string_at(original_padded, current + 3, 2, "ER", "EN", "")) {
                     primary <- paste0(primary, "X")
                     secondary <- paste0(secondary, "SK")
                   } else {
                     primary <- paste0(primary, "SK")
                     secondary <- paste0(secondary, "SK")
                   }
                   current <- current + 3
                 } else {
                   if (current == 1 && !is_vowel(original_padded, 4) &&
                       char_at(original_padded, 4) != "W") {
                     primary <- paste0(primary, "X")
                     secondary <- paste0(secondary, "S")
                   } else {
                     primary <- paste0(primary, "X")
                     secondary <- paste0(secondary, "X")
                   }
                   current <- current + 3
                 }
               } else if (string_at(original_padded, current + 2, 1, "I", "E", "Y", "")) {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "S")
                 current <- current + 3
               } else {
                 primary <- paste0(primary, "SK")
                 secondary <- paste0(secondary, "SK")
                 current <- current + 3
               }
             }
             else {
               # French e.g. 'resnais', 'artois'
               if (current == last &&
                   string_at(original_padded, current - 2, 2, "AI", "OI", "")) {
                 primary <- paste0(primary, "")
                 secondary <- paste0(secondary, "S")
               } else {
                 primary <- paste0(primary, "S")
                 secondary <- paste0(secondary, "S")
               }

               if (string_at(original_padded, current + 1, 1, "S", "Z", "")) {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
             }
           },

           "T" = {
             if (string_at(original_padded, current, 4, "TION", "")) {
               primary <- paste0(primary, "X")
               secondary <- paste0(secondary, "X")
               current <- current + 3
             }
             else if (string_at(original_padded, current, 3, "TIA", "TCH", "")) {
               primary <- paste0(primary, "X")
               secondary <- paste0(secondary, "X")
               current <- current + 3
             }
             else if (string_at(original_padded, current, 2, "TH", "") ||
                      string_at(original_padded, current, 3, "TTH", "")) {
               # Special case 'thomas', 'thames' or Germanic
               if (string_at(original_padded, current + 2, 2, "OM", "AM", "") ||
                   string_at(original_padded, 1, 4, "VAN ", "VON ", "") ||
                   string_at(original_padded, 1, 3, "SCH", "")) {
                 primary <- paste0(primary, "T")
                 secondary <- paste0(secondary, "T")
               } else {
                 primary <- paste0(primary, "0")  # yes, zero
                 secondary <- paste0(secondary, "T")
               }
               current <- current + 2
             } else {
               if (string_at(original_padded, current + 1, 1, "T", "D", "")) {
                 current <- current + 2
               } else {
                 current <- current + 1
               }
               primary <- paste0(primary, "T")
               secondary <- paste0(secondary, "T")
             }
           },

           "V" = {
             if (char_at(original_padded, current + 1) == "V") {
               current <- current + 2
             } else {
               current <- current + 1
             }
             primary <- paste0(primary, "F")
             secondary <- paste0(secondary, "F")
           },

           "W" = {
             # Can also be in middle of word
             if (string_at(original_padded, current, 2, "WR", "")) {
               primary <- paste0(primary, "R")
               secondary <- paste0(secondary, "R")
               current <- current + 2
             } else if (current == 1 &&
                        (is_vowel(original_padded, current + 1) ||
                         string_at(original_padded, current, 2, "WH", ""))) {
               # Wasserman should match Vasserman
               if (is_vowel(original_padded, current + 1)) {
                 primary <- paste0(primary, "A")
                 secondary <- paste0(secondary, "F")
               } else {
                 # Need Uomo to match Womo
                 primary <- paste0(primary, "A")
                 secondary <- paste0(secondary, "A")
               }
               current <- current + 1
             } else if ((current == last && is_vowel(original_padded, current - 1)) ||
                        string_at(original_padded, current - 1, 5,
                                  "EWSKI", "EWSKY", "OWSKI", "OWSKY", "") ||
                        string_at(original_padded, 1, 3, "SCH", "")) {
               # Arnow should match Arnoff
               primary <- paste0(primary, "")
               secondary <- paste0(secondary, "F")
               current <- current + 1
             } else if (string_at(original_padded, current, 4, "WICZ", "WITZ", "")) {
               # Polish e.g. 'filipowicz'
               primary <- paste0(primary, "TS")
               secondary <- paste0(secondary, "FX")
               current <- current + 4
             } else {
               # Else skip it
               current <- current + 1
             }
           },

           "X" = {
             # French e.g. breaux
             if (!((current == last) &&
                   (string_at(original_padded, current - 3, 3, "IAU", "EAU", "") ||
                    string_at(original_padded, current - 2, 2, "AU", "OU", "")))) {
               primary <- paste0(primary, "KS")
               secondary <- paste0(secondary, "KS")
             }

             if (string_at(original_padded, current + 1, 1, "C", "X", "")) {
               current <- current + 2
             } else {
               current <- current + 1
             }
           },

           "Z" = {
             # Chinese pinyin e.g. 'zhao'
             if (char_at(original_padded, current + 1) == "H") {
               primary <- paste0(primary, "J")
               secondary <- paste0(secondary, "J")
               current <- current + 2
             } else if (string_at(original_padded, current + 1, 2, "ZO", "ZI", "ZA", "") ||
                        (is_slavo_germanic(original) &&
                         (current > 1 && char_at(original_padded, current - 1) != "T"))) {
               primary <- paste0(primary, "S")
               secondary <- paste0(secondary, "TS")
             } else {
               primary <- paste0(primary, "S")
               secondary <- paste0(secondary, "S")
             }

             if (char_at(original_padded, current + 1) == "Z") {
               current <- current + 2
             } else {
               current <- current + 1
             }
           },

           {
             # Default case
             current <- current + 1
           }
    )
    if (current == prev_current) {
      current <- current + 1
    }
  }

  # Trim to max_length
  if (nchar(primary) > max_length) {
    primary <- substr(primary, 1, max_length)
  }
  if (nchar(secondary) > max_length) {
    secondary <- substr(secondary, 1, max_length)
  }

  c(primary, secondary)
}

# Vectorized version for multiple inputs
#' @export
double_metaphone_vec <- function(words) {
  result <- lapply(words, double_metaphone)
  primary <- sapply(result, `[`, 1)
  secondary <- sapply(result, `[`, 2)
  data.frame(primary = primary, secondary = secondary, stringsAsFactors = FALSE)
}
