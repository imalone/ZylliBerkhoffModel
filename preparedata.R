source("support.R")
source("debounce.R")

ngctL <- readRDS("../ngctLbig.rds")
for (n in 1:length(ngctL)) {
  for (ind in 1:n) {
    ngctL[[n]] <- ngctL[[n]][!grepl("[0-9]{2,}", get(paste0("V",ind)))]
  }
}

for (n in 1:length(ngctL)) {
  for (nc in 1:n) {
    ngctL[[n]][, nc:=as.character(get(names(ngctL[[n]])[nc])), with=FALSE]
  }
}
for (n in 1:length(ngctL)) {
  for (ind in paste0("V",1:n)) {
    Encoding(ngctL[[n]][[ind]])<-"unknown"
  }
}

ngctL <- ngLcodeTokens(ngctL)

disclist <- list()
for (n in 1:length(ngctL)) {
  disclist[[n]] <- KNDisc(ngctL[[n]])
  ngctL[[n]][, Ndisc := sapply(N,subtractDiscount, disclist[[n]])]
}

lapply(2:length(ngctL), function(x){setkeyv(ngctL[[x]],paste0("V",(x-1):1))})

# Cut version, only depth 3, to better fit on Shiny.io.
ngctLc <- ngctL[1:3]
# Testing shows a training set of about 0.45 of the total data with N=1 entries
# removed from the depth 4 ngram after discount calculation may perform better,
# and just about fit into shiny memory, but will take time to generate.

#saveRDS(ngctL, "../ngctLbig-prepped-no-double-digit.rds")
saveRDS(ngctLc, "./Data/ngctLbig-prepped-no-double-digit-cut.rds")
saveRDS(disclist,"./Data/ngctLbig-no-double-digit-disclist.rds")
