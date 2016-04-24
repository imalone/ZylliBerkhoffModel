require(data.table)
require(stringi, quietly=TRUE)
#require(quanteda) # Needed if stemming

countWords <- function(lines){
  sapply(lines,
         function(x){
           if(nchar(x) > 0) {
             length(unlist(stri_split_boundaries(x)))
           } else {
             0
           }
         }
  )
}

# Yes..., must be only latin. Also catch empty strings (wordstem reduces "s" to "")
filterChar <- function(x) { grepl("^[A-Za-zéèê0-9'-]+$",x)}
stripChar <- function(x) { gsub("'|-","",x)}

stripAndStem <- function(x, language="none") {
  if(language=="none") {
    stripChar(x)
  }
  else {
    wordstem(stripChar(x), language)
  }
}
stripLowAndStem <- function(x, language="none") {
  stripAndStem(tolower(x), language)
}


# Return an input string processed for searching ngrams
inputToTokens <- function(x, language="none") {
  tokens <- sapply(stri_extract_all_words(x), stripLowAndStem, language)
  Encoding(tokens) <- "unknown"
  as.list(tokens)
}

# Take list of tokens for match, return function that will match
# the rows in a data
getRowMatches <- function(tokens, nglist, thisn) {
  colstomatch <- thisn - 1
  if (colstomatch > 0) {
    ngmatch <- copy(nglist[[thisn]][rev(tokens)[1:colstomatch], nomatch=0])
  }
  else {
    ngmatch <- copy(nglist[[thisn]])
  }
  lastcol <- paste0("V",thisn)
  ngmatch[, "w" := get(lastcol)]
  ngmatch[]
}



# Take an input of inputTokens, list of ngram data tables (with reverse-order
# keys already set for N-1 tokens, e.g. setkeyv(ngctL[[3]],paste0("V",2:1))
# list index should indicate n-gram length, empty entries are okay. Return list
# of up to nres results for final token, best matches first. (Unlikely to return
# less than nres unless an empty or short N=1 ngram list is given.)
backoffPredict <- function(inputTokens, ngList, nres=3, indmode = TRUE) {
  matches <- data.table(w=character(), score=numeric())
  maxng <- length(ngList)
  searchMax <- min(maxng,length(inputTokens)+1)
  ngind <- searchMax
  # Don't bother searching over ngrams that are longer than 1+our token length
  # They're suboptimal anyway as n>2 are filtered for sparsity
  searchNg <- ngList[1:searchMax]


  needMatches <- nres
  while (needMatches > 0 && ngind > 0) {
    # get matches on ngList[ngind] and append up to length(matches) - nres
    if (indmode) {
      matchesN <- matchInNgL(inputTokens, ngList, ngind)[order(-N)]
    }
    else {
      matchesN <- getRowMatches(inputTokens, ngList, ngind)[order(-N)]
    }
    matchesNrow <- nrow(matchesN)
    if (matchesNrow>0) {
      totN <- sum(matchesN[,N])
      # Scoring is arbitrary, but puts longer matches above shorter ones
      newmatches <- matchesN[, .(w, score=ngind+N/totN)]
      setkey(newmatches,w)
      newmatches <- newmatches[!matches[,w]][order(-score)]
      newmatchesN <- nrow(newmatches)
      if (newmatchesN > 0) {
        matches <- rbind(matches, newmatches[1:min(needMatches,newmatchesN)])
      }
      needMatches <- nres - length(matches)
    }
    ngind <- ngind - 1
  }
  matches
}


# Return list of fixed K-N discount values calculated on the ngram counts table
# ngtable
KNDisc <- function(ngtable) {
  ncounts <- sapply(1:4, function(n){nrow(ngtable[N==n])})
  y = ncounts[1] / (ncounts[1]+2*ncounts[2])
  if ( y==0 ) {
    # Safety measure if we have y=0, usually comes from
    # trying to calculate discounts after N=1 counts have been trimmed
    list(y=0, d1=0, d2=0, d3p=0)
  }
  else {
    list(y=y,
         d1 = 1 - 2*y*ncounts[2]/ncounts[1],
         d2 = 2 - 3*y*ncounts[3]/ncounts[2],
         d3p = 3 - 4*y*ncounts[4]/ncounts[3]
    )
  }
}


# Add Ndisc column to data table by applying discounts (d1, d2, d3p) to column N.
# If discounts not supplied, then fixed discounts are calculated. The discounts
# list is returned.
# addDiscCol <- function(ngtable, discounts = list()) {
#   if (length(discounts)==0) {
#     discounts <- KNDisc(ngtable)
#   }
#   applydisc <- function (N, d) {
#     if (N==1) {N-d$d1}
#     else if (N==2) { N-d$d2}
#     else {N-d$d3p}
#   }
#   ngtable[, Ndisc := sapply(N,applydisc, discounts)]
#   discounts
# }
subtractDiscount <- function (N, d) {
  if (N==1) {N-d$d1}
  else if (N==2) { N-d$d2}
  else {N-d$d3p}
}

# Do Kneser-Ney interpolation, using fixed discounting and recursion. ngList
# is a list 1:n of n-gram tables of 1:n depth, which will be descended.
# Take an input of inputTokens, list of ngram data tables (with reverse-order
# keys already set for N-1 tokens, e.g. setkeyv(ngctL[[3]],paste0("V",2:1))
# pthresh is used internally to threshold probability entries when combining
# (for faster operation)

KNpredict <- function(inputTokens, ngList, disclist = list(), indmode=TRUE,
                      pthresh = 0.01) {
  thisn <- length(ngList)
  ngtable <- ngList[[thisn]]
  ngsub <- if (thisn > 1) {
    ngList[1:(thisn-1)]
  }
  else {
    list()
  }

  if (length(disclist)==0) {
    disc <- KNDisc(ngtable)
  }
  else {
    disc <- disclist[[thisn]]
    disclist <- disclist[1:(thisn-1)]
  }
  tkLength <- length(inputTokens)
  totalcount <- 0
  if ( tkLength >= thisn - 1 ) {
    inputTokens <- inputTokens[seq(to=tkLength, length.out= thisn-1)]
    if (indmode)  {
      ngtableMatch <- matchInNgL(inputTokens, ngList, thisn)
    } else {
      ngtableMatch <- if (thisn-1 > 0) {
        ngtable[rev(inputTokens)]
      }
      else {
        ngtable
      }
      ngtableMatch <- ngtableMatch[, thisn:(thisn+2), with=FALSE]
      names(ngtableMatch)[1] <- "w"
    }
    totalcount <- sum(ceiling(ngtableMatch$N), na.rm=TRUE)
  }
  if ( !is.na(totalcount) && totalcount > 0) {
    gamma <- disc$d1 * nrow(ngtableMatch[N>0 & N<=1]) +
      disc$d2 * nrow(ngtableMatch[N>1 & N<=2]) +
      disc$d3p * nrow(ngtableMatch[N>2])
    gamma <- gamma / totalcount
    ngtableMatch <- ngtableMatch[, .(w, prob = Ndisc/totalcount)]
    if (thisn > 1) {
      lowerOrderMatch <- KNpredict(inputTokens, ngsub, disclist, indmode, pthresh)
      lowerOrderMatch[, prob:= prob*gamma]
      if (pthresh > 0) {
        ngtableMatch <- rbind(ngtableMatch[prob>pthresh],
            lowerOrderMatch[prob>pthresh])[, .(prob=sum(prob)),
                                                          by=w]
      }
      else {
        ngtableMatch <- rbind(ngtableMatch,
                              lowerOrderMatch)[, .(prob=sum(prob)),
                                                          by=w]
      }
    }
    ngtableMatch[order(-prob)]
  }
  else {
    KNpredict(inputTokens, ngsub, disclist, indmode, pthresh)
  }
}


# With an array of data.tables of ngram frequencies, the first of which is
# single tokens, convert the tables so the first one acts as an index for the
# others (i.e. add an index variable to nglist[[1]], and replace V1... of the
# other tables with numbers referring to this index). Currently relies on
# all entries being present in the nglist[[1]] table, though zero frequency
# entries could be added.
ngLcodeTokens <- function(nglist) {
  if (length(nglist) == 0) {
    return()
  }
  if (length(nglist) == 1) {
    nglist[[1]][order(-N), index := .I]
    setkey(nglist[[1]], V1)
    return()
  }
  # If we are processing tokens, e.g. removing hyphens for initial tokens
  # in ngrams, then they need to be added on to the index list.
  ng2 <- nglist[[2]]
  setkey(ng2,V1)
  ngExtra <- unique(ng2[!nglist[[1]][,V1]][, .(V1, N=0)])
  nglist[[1]] <- rbind(nglist[[1]], ngExtra, fill=TRUE)
  nglist[[1]][order(-N), index := .I]
  setkey(nglist[[1]], V1)
  
  lapply (nglist[2:length(nglist)],
          function(ngtable, indtable) {
            lapply( grep("^V\\d+$", colnames(ngtable), value=TRUE),
                    function(vcol) {
                      ngtable[, vcol := indtable[ngtable[,vcol, with=FALSE],
                                                 index], 
                              with=FALSE]
                    })
          },
          nglist[[1]]
  )
  nglist
}

# Match last depth-1 tokens in table number depth, where
# that table should be keyed in reverse order from its penultimate
# V (i.e. with V1...Vn, keys Vn-1 ... V1), and table #1 is keyed on V1
# return table, last VN mapped back to table 1 (text value) as w and with
# other fields (e.g. N, Ndisc)
matchInNgL <- function(tokens, nglist, depth) {
  if (depth < 2){
    ng1 <- copy(nglist[[1]])
    whichV1 <- colnames(ng1) == "V1"
    colnames(ng1)[whichV1] <- "w"
    # Only N>0, don't return index-only rows
    return(ng1[N>0])
  }
  ngind <- nglist[[1]][, .(V1, index)]
  ngmatch <- nglist[[depth]]
  matchlen <- min (length(tokens), max(depth-1,0))
  matchtok <- tokens[seq(to=length(tokens), length.out=matchlen)]
  matchind <- ngind[unlist(matchtok), index]
  #  matchrows <- ngmatch[transpose(list(rev(matchind))), nomatch=0]
  matchcols <- as.data.table(transpose(list(matchind)))
  setkeyv(matchcols, rev(colnames(matchcols)))
  matchrows <- ngmatch[matchcols, nomatch=0]
  lastcol <- last(grep("^V\\d+$", colnames(ngmatch), value=TRUE))
  #setkey(ngind, index)
  #matchrows <- ngind[ matchrows[, .SD, eval(lastcol)] ]
  #matchrows <- ngind[ matchrows[, .SD, eval(lastcol)] , on=c(index=eval(lastcol)) ]
  setkey(ngind, index)
  setkeyv(matchrows, lastcol)
  matchrows <- ngind[ matchrows ]
  whichV1 <- colnames(matchrows) == "V1"
  colnames(matchrows)[whichV1] <- "w"
  matchrows[, i.V1 := NULL]
  matchrows[]
}
