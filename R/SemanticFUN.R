
##Package requirements: usethis::use_package(c("dplyr", "tidyverse", "stringr", "readxl", "ggrepel"))


##Text preprocessing

#### split_sequential

#' Title: Sequential division of words into single elements
#'
#' The function can process a text file or string. It performs sequential division
#' of words into single elements, splitting is based on typical character separators
#' including; tab, space, comma, semicolon and full stop.
#'
#' @param x A txt.file or string
#'
#' @return character vector with separated words
#' @examples
#' txt <- c("I am so happy")
#'
#' txt <- split_sequential(txt)
#'
#' @export
split_sequential <- function(x) {
  stops <- c("\t", "\\.", ",", ";", " ")  # Character separators
  tmp <- as.character(x)                   # make sure input is character

  for (sep in stops) {
    # split each element of tmp by sep
    tmp <- unlist(lapply(tmp, function(s) strsplit(s, sep)[[1]]), use.names = FALSE)
    # remove empty strings
    tmp <- tmp[tmp != ""]
  }

  return(tmp)
}

#### txt_low_split

#' Title: Sequential word division and conversion to lower case letters
#'
#' The function can process a text file or string. It performs sequential division
#' of words into single elements, splitting is based on typical character separators
#' including; tab, space, comma, semicolon and full stop. The output vector has all lower case letters.
#'
#'
#' @param x A txt.file or string
#'
#' @return character vector with separated words and lowercase letters
#' @examples
#' txt <- c("I am so happy")
#'
#' txt_low_split(txt)
#'
#' @export
txt_low_split <- function(x) {as.character(tolower(split_sequential(x)))}

##Semantic labelling and Definition of query words

#Loop through input file to find pos or neg words depending on input



#### query_prefix

#' Title: Query for word prefix
#'
#' The function queries a given text for words begining with a specified prefix.
#'
#' @param x Character vector to query
#' @param y List of words with prefix annotation "word*"
#' @return A character vector of class character
#' @examples
#' txt <- c("I", "am", "so", "happy")
#' prefix_words <- "happ*"
#'
#' query_prefix(txt, pos_words)
#'
#' @export
query_prefix <- function(x, y) {
  prefix <- character(0)

  for (word in y) {
    if (endsWith(word, "*")) {
      prefix <- c(prefix, str_extract(word, "^[a-z]+(?=\\*)"))
    }
    matched <- unique(c(
      x[sapply(x, function(z) any(startsWith(z, prefix)))]))
  }
  return(matched)
}


#### query_suffix

#' Title: Query for word suffix
#'
#' The function queries a given text for words ending with a specified suffix.
#'
#' @param x Character vector to query
#' @param y List of words with suffix annotation "*word"
#' @return A character vector of class character
#' @examples
#' txt <- c("She", "wouldn't", "be", "happy")
#' suffix_words <- c("*py", "*'t")
#'
#' query_suffix(txt, suffix_words)
#'
#' @export
query_suffix <- function(x,y) {
  suffix <- character(0)

  for (word in y) {
    if (startsWith(word, "*")) {
      suffix <- c(suffix, str_extract(sub("^\\*", "", word), "([a-z]+$)|('[a-z]+$)"))
    }
    matched <- unique(c(x[sapply(x, function(z) any(endsWith(z, suffix)))]))
  }
  return(matched)
}


#### query_equal

#' Title: Query for word matches
#'
#' The function queries a given text for characters with an
#' exact match to those specified in a list of query words
#'
#' @param x Character vector to query
#' @param y List of query words
#' @return A character vector of class character
#' @examples
#' txt <- c("I", "am", "so", "happy")
#' query_words <- "am"
#'
#' query_equal(txt, query_words)
#'
#' @export
query_equal <- function(x,y) {
  equal  <- character(0)

  for (word in y) {
    equal <- c(equal, str_extract(word, "\\b[a-z]+\\b"))}
  matched <- unique(c(x[x %in% equal]))
  return(matched)
}


#### query_all

#' Title: Query for word matches to query words
#'
#' The function queries a given text for words with either an
#' exact match, a prefix or suffix of characters specified in a list of query words.
#'
#' @param x Character vector to query
#' @param y List of query words
#' @return A character vector of class character
#' @examples
#' txt <- c("I", "am", "so", "happy", "that", "you" "are", "using", "this", "function")
#' query_words <- c("am", "us*", "that", "*tion")
#'
#' query_all(txt, query_words)
#'
#' @export
query_all <- function(x, y) {

  # storage for extracted patterns
  prefix <- character(0)
  suffix <- character(0)
  equal  <- character(0)

  # ---- Extract patterns from y ----
  for (word in y) {

    if (endsWith(word, "*")) {
      # prefix match (abc*)
      prefix <- c(prefix, str_extract(word, "^[a-z]+(?=\\*)"))

    } else if (startsWith(word, "*")) {
      # suffix match (*abc)
      suffix <- c(suffix, str_extract(sub("^\\*", "", word),
                                      "([a-z]+$)|('[a-z]+$)"))

    } else {
      # exact match (abc)
      equal <- c(equal, str_extract(word, "\\b[a-z]+\\b"))
    }
  }

  # ---- matching in x ----

  matched_prefix <- x[sapply(x, function(z) any(startsWith(z, prefix)))]
  matched_suffix <- x[sapply(x, function(z) any(endsWith(z, suffix)))]
  matched_equal  <- x[x %in% equal]

  # combine and make unique
  matched <- unique(c(matched_prefix, matched_suffix, matched_equal))

  return(matched)
}



#Calculation of word count, sum and ratio of negative/positive words

#### wordcount

#' Title: Word counts of query words
#'
#' The function provides word count (the number of times the word appears)
#' in a given text for a specified list of query words.
#'
#' @param x Character vector (to query)
#' @param y List of query words
#' @return counts for each query word
#' @examples
#' txt <- c("I", "am", "so", "happy")
#' pos_words <- "happy"
#'
#' wordcount(txt, pos_words)
#'
#' @export
wordcount <- function(x, y){
  matched_words <- unique(query_all(x, y))
  counts <- sapply(matched_words, function(w) sum(str_count(x, fixed(w))))
  total_matches <- sum(counts)
  return(counts)
}


#### wordsum

#' Title: Sum of query words
#'
#' The function provides the sum of wordcounts.
#'
#' @param x Character vector (to query)
#' @param y List of query words
#' @return numerical sum
#' @examples
#' txt <- c("I", "am", "so", "happy")
#' pos_words <- "happy"
#'
#' wordsum(txt, pos_words)
#'
#' @export
wordsum <- function(x, y){
  matched_words <- unique(query_all(x, y))
  counts <- sapply(matched_words, function(w) sum(str_count(x, fixed(w))))
  total_matches <- sum(counts)
  return(total_matches)
}


#### wordratio

#' Title: Ratio of the sum of query words
#'
#' The function provides a numeric ratio of the sum of total
#' query words of two separate query vectors.
#'
#' @param x Character vector (to query)
#' @param y List of query words
#' @param z List of query words
#' @return numerical ratio of wordsum(y)/worsum(z)
#' @examples
#' txt <- c("I", "am", "so", "proud", "and", "scared")
#' pos_words <- "proud"
#' neg_words <- "scared"
#'
#' wordratio(txt, pos_words, neg_words)
#'
#' @export
wordratio <- function(x, y, z) {
  denom <- wordsum(x, z)
  if (denom == 0) return(NA)
  wordsum(x, y) / denom
}


