require(data.table)
require(stringi, quietly=TRUE)
require(quanteda)


# Yes..., must be only latin. Also catch empty strings (wordstem reduces "s" to "")
filterChar <- function(x) { grepl("^[A-Za-zéèê0-9'-]+$",x)}
stripChar <- function(x) { gsub("'|-","",x)}

stripLowAndStem <- function(x) {tolower(wordstem(stripChar(x)))}

#ngct4[as.list(rev(wordstem(unlist(stri_extract_all_words("adam sandlers")))))][,.(N=sum(N)),by=.(V2,V3,V4)][order(N)]

# Return an input string processed for searching ngrams
inputToTokens <- function(x) {
  as.list(sapply(stri_extract_all_words(x), stripLowAndStem))
}

# Take list of tokens for match, return function that will match
# the rows in a data
getRowMatches <- function(ngramdt, tokens) {
  colstomatch <- ncol(ngramdt) - 2
  if (colstomatch > 0) {
    ngramdt[rev(tokens)[1:colstomatch]]
  }
  else {
    ngramdt
  }
}

# Take an input of inputTokens, list of ngram data tables (with reverse-order
# keys already set for N-1 tokens, e.g. setkeyv(ngctL[[3]],paste0("V",2:1))
# list index should indicate n-gram length, empty entries are okay. Return list
# of up to nres results for final token, best matches first. (Unlikely to return
# less than nres unless an empty or short N=1 ngram list is given.)
backoffPredict <- function(inputTokens, ngList, nres=3) {
  matches <- list()
  maxng <- length(ngList)
  searchMax <- min(maxng,length(inputTokens)+1)
  ngind <- searchMax
  # Don't bother searching over ngrams that are longer than 1+our token length
  # They're suboptimal anyway as n>2 are filtered for sparsity
  searchNg <- ngList[1:searchMax]
  allMatches <- lapply(ngList, getRowMatches, inputTokens)
  needMatches <- nres
  while (needMatches > 0 && ngind > 0) {
    # get matches on ngList[ngind] and append up to length(matches) - nres
    matchesN <- allMatches[[ngind]][order(-N)]
    matchesNrow <- nrow(matchesN)
    if (matchesNrow>0) {
      ccount <- ncol(matchesN)
      cnames <- colnames(matchesN)
      matchesN <- matchesN[1:min(needMatches,matchesNrow)]
      newmatches <- as.character(unlist(matchesN[, cnames[ccount-1], with=FALSE]))
      if ( !is.na(newmatches[1])) {
        matches <- append(matches, as.list(newmatches))
        needMatches <- nres - length(matches)
      }
    }
    ngind <- ngind - 1
  }
  matches
}
