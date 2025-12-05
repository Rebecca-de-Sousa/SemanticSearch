

library(dplyr)
library(tidyr)
library(stringr)


### Functions for text pre-processing ###

## split sequential takes an input file x and splits into single words
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

## txt_lower converts the text_file into all lower characters
# and applies the split sequential function
txt_low_split <- function(x) {as.character(tolower(split_sequential(x)))}


### Functions for word query from an example list, looking for prefix, suffix and direct matches

#query prefix
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

#query suffix
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

#query equal
query_equal <- function(x,y) {
  equal  <- character(0)

  for (word in y) {
    equal <- c(equal, str_extract(word, "\\b[a-z]+\\b"))}
  matched <- unique(c(x[x %in% equal]))
  return(matched)
}


## Combining individual functions, here there is a problem with duplicates from equal and prefix or suffix
words_match <- function(x,y) {
  list_1 <- list(c(query_prefix(x,y), query_suffix(x,y), query_equal(x,y)))
  return(unlist(list_1))
}

#### Combined query
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




### Functions for calculation of wordcounts and ratios ###

wordcount <- function(x, y){
  matched_words <- unique(query_all(x, y))
  counts <- sapply(matched_words, function(w) sum(str_count(x, fixed(w))))
  total_matches <- sum(counts)
  return(counts)
}

wordsum <- function(x, y){
  matched_words <- unique(query_all(x, y))
  counts <- sapply(matched_words, function(w) sum(str_count(x, fixed(w))))
  total_matches <- sum(counts)
  return(total_matches)
}


wordratio <- function(x, y, z) {
  denom <- wordsum(x, z)
  if (denom == 0) return(NA)
  wordsum(x, y) / denom
}

