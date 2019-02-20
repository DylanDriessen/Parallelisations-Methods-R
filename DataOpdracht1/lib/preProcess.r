preProcess <- function() {
  import("stringi")   # String manipulation
  # TRANSLITERATE TO LATIN
  docs$text <- stringi::stri_trans_general(docs$text, 'Latin-ASCII')
}