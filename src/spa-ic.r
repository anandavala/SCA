# SPA-IC
# An I Ching based type-casting used for SPA analysis of randomly generated populations of hexagrams.
# The I Ching is a classic example that uses 6 binary dimensions to type-cast the space of possible ways of being and changing.

# randomly generate n hexagrams
getHexagrams <- function(n = 10000) {
  lineSymbols <- c("yin", "yang")
  out <- data.frame(matrix(NA, nrow=n, ncol=6))
  lines <- rep(0, 6)
  for (r in 1:n) {
    for (l in 1:6) {
      lines[l] <- as.integer(sample(c(0,1), 1, replace = TRUE))
    }
    out[r,] <- lines
  }
  colnames(out) <- c("L1", "L2", "L3", "L4", "L5", "L6")
  out$L1 <- factor(out$L1)
  out$L2 <- factor(out$L2)
  out$L3 <- factor(out$L3)
  out$L4 <- factor(out$L4)
  out$L5 <- factor(out$L5)
  out$L6 <- factor(out$L6)
  levels(out$L1) <- lineSymbols
  levels(out$L2) <- lineSymbols
  levels(out$L3) <- lineSymbols
  levels(out$L4) <- lineSymbols
  levels(out$L5) <- lineSymbols
  levels(out$L6) <- lineSymbols
  return(out)
}

# 100,000 hexagrams. For some reason it is faster to do this in small chunks
Data <- getHexagrams(20000)
for (i in 1:4) Data <- rbind(Data, getHexagrams(20000))

str(Data)

symbolSet <- getSymbolSet(Data)

# focus on relevant data
dims <- c(1:6) # columns of interest
colnames(Data)[dims]
symSet <- symbolSet[,dims] # strip down to relevant columns
symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs

TF <- getTypeFreqs(Data, dims, symSet)
str(TF)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# Cluster Diagram
gDist <- daisy(TF, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)

scenarios <- getScenarios(TF, symSet)
colnames(scenarios)
str(scenarios)

# Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")

# What are the most discouraged and encouraged adaptations, with the highest absolute choice difference?
head(scenarios[order(-abs(scenarios$Diff1)), ])

# What are the adaptations with the least choice difference?
head(scenarios[order(abs(scenarios$Diff1)), ])

# What are the adaptations with the most targeted pressure?
head(scenarios[order(-abs(scenarios$TP1)), ])

# What are the adaptations with the least targeted pressure?
head(scenarios[order(abs(scenarios$TP1)), ])


paths <- getAllPaths(scenarios, symSet)

str(paths)

sortedPlot(paths, "TGroupAP", datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total choice difference values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDiff", datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of demographic pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDP", datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total targeted pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChTP", datatype = "Evolutionary Paths")


# The most discouraged paths (in terms of choice difference)
(tmp <- head(paths[order(paths$TChDiff), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

# The most discouraged paths (in terms of targeted pressure)
(tmp <- head(paths[order(paths$TChTP), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

# The most encouraged paths (in terms of choice difference)
(tmp <- head(paths[order(-paths$TChDiff), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

# The most encouraged paths (in terms of targeted pressure)
(tmp <- head(paths[order(-paths$TChTP), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

# The least pressure paths (in terms of choice difference)
(tmp <- head(paths[order(abs(paths$TChDiff)), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

# The least pressure paths (in terms of targeted pressure)
(tmp <- head(paths[order(abs(paths$TChTP)), ], n = 16))
getPath(scenarios, row2CharVec(tmp[1, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[2, 1:6]), symSet)
getPath(scenarios, row2CharVec(tmp[3, 1:6]), symSet)

