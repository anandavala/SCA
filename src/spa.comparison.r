# SPA-COMPARISON

# Here I explore ways of comparing two related data sets.
# I use the Myers-Briggs data for males and females, or they could be different snapshots in time.
# First I explore computing the simple differences, i.e. PPF - PPM or PPM - PPF.
# The values are no longer the percentage prevalences, they are the difference in PP between the two data sets.
# But what if I just plug those values in and do the analysis anyway?
# It works. Mathematically speaking the algorithm only looks at the difference between type-frequency values.
# So it doesn't matter if some are positive and some negative. Or that they don't sum to 100%.
# But do the results make sense? How to interpret the results?


#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

loaded <- loadMB()

TF <- loaded$TF
symSet <- loaded$symSet

# PPFM = PPF - PPM (diff = females - males)
TF <- mutate(TF, PPFM = PPF - PPM, PPMF = PPM - PPF)
rownames(TF) <- TF$Type
TF <- select(TF, D1, D2, D3, D4, PP = PPMF)
str(TF)

# # make type-frequency distribution uniform
# TF$PP <- rep(100 / nrow(TF), nrow(TF))
# summary(TF$PP)

summary(TF$PP)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

g.max <- mkGraph(TF, symSet, onlyMax = TRUE)

pr <- getPageRanked(g.max, layout = layout_with_graphopt)

g.full <- mkGraph(TF, symSet)

pr <- getPageRanked(g.full)

# adjust the percentile value until all nodes are simply connected
# PPT use 0.83
# PPF use 0.843
# PPM use 0.901
g <- trimGraph(g.full, percentile = 0.901)

pr <- getPageRanked(g)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.359)
# PPT use 0.7
# PPF use 0.7
# PPM use 0.7
g <- trimGraph(g.full, percentile = 0.8)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)
analyseSubgraph(decomp, rank = 3, pr = pr)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.358)
# PPT use 0.6
# PPF use 0.6
# PPM use 0.6
g <- trimGraph(g.full, percentile = 0.6)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)
analyseSubgraph(decomp, rank = 3, pr = pr)
analyseSubgraph(decomp, rank = 4, pr = pr)
analyseSubgraph(decomp, rank = 5, pr = pr)


analyseAllSubgraphs(decomp, pr = pr)

# Interactive step through
analyseAllSubgraphs(decomp, interactive = TRUE, pr = pr)




