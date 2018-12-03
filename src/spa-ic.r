# SPA-IC
# An I Ching based type-casting used for SPA analysis of randomly generated populations of hexagrams.
# The I Ching is a classic example that uses 6 binary dimensions to type-cast the space of possible ways of being and changing.

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

# so that we can generate links to more information about each component of the reading.
urlTemplate <- c("https://ichingdb.pythonanywhere.com/ichingdb/", "/", "/")

# randomly generate n hexagrams
getHexagrams <- function(n = 10000, asFactor = TRUE) {
  out <- data.frame(matrix(NA, nrow=n, ncol=6))
  lines <- rep(0, 6)
  for (r in 1:n) {
    for (l in 1:6) {
      lines[l] <- as.integer(sample(c(0,1), 1, replace = TRUE))
    }
    out[r,] <- lines
  }
  colnames(out) <- c("L1", "L2", "L3", "L4", "L5", "L6")
  if (asFactor) {
    out$L1 <- factor(out$L1)
    out$L2 <- factor(out$L2)
    out$L3 <- factor(out$L3)
    out$L4 <- factor(out$L4)
    out$L5 <- factor(out$L5)
    out$L6 <- factor(out$L6)
    lineSymbols <- c("yin", "yang")
    levels(out$L1) <- lineSymbols
    levels(out$L2) <- lineSymbols
    levels(out$L3) <- lineSymbols
    levels(out$L4) <- lineSymbols
    levels(out$L5) <- lineSymbols
    levels(out$L6) <- lineSymbols
  }
  return(out)
}

# 100,000 hexagrams. For some reason it is faster to do this in small chunks
# set.seed(777) # two isolated nodes
seed <- as.numeric(Sys.time())
print(seed)
set.seed(seed)
Data <- getHexagrams(10000)
# for (i in 1:9) Data <- rbind(Data, getHexagrams(10000))

str(Data)

symbolSet <- getSymbolSet(Data)

# focus on relevant data
dims <- c(1:6) # columns of interest
colnames(Data)[dims]
symSet <- symbolSet[,dims] # strip down to relevant columns
symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs

TF <- getTypeFreqs(Data, dims, symSet)
rownames(TF) <- c(2, 24, 7, 19, 15, 36, 46, 11, 16, 51, 40, 54, 62, 55, 32, 34, 8, 3, 29, 60, 39, 63, 48, 5, 45, 17, 47, 58, 31, 49, 28, 43, 23, 27, 4, 41, 52, 22, 18, 26, 35, 21, 64, 38, 56, 30, 50, 14, 20, 42, 59, 61, 53, 37, 57, 9, 12, 25, 6, 10, 33, 13, 44, 1)
str(TF)
summary(TF$PP)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# Examine paths of maximum likelihood between nodes
g.max <- mkGraph(TF, symSet, onlyMax = TRUE)
pr <- getPageRanked(g.max)

decomp <- getDecomposition(g.max)
analyseSubgraph(decomp, rank = 1, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 2, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 3, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 4, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 5, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 6, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 7, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 8, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 9, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 10, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 11, pr = pr, urlTemplate = urlTemplate)




# Examine all paths between nodes
g.full <- mkGraph(TF, symSet)

pr <- getPageRanked(g.full)

# keep only the top percentile
g <- trimGraph(g.full, percentile = 0.5)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 2, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 3, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 4, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 5, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 6, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 7, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 8, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 9, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 10, pr = pr, urlTemplate = urlTemplate)

analyseAllSubgraphs(decomp, pr = pr, urlTemplate = urlTemplate)

# Interactive step through
analyseAllSubgraphs(decomp, interactive = TRUE, pr = pr, urlTemplate = urlTemplate)

