# SPA-ICHING
# An I Ching based type-casting used for SPA analysis of randomly generated populations of hexagrams.
# The I Ching is a classic example that uses 6 binary dimensions to type-cast the space of possible ways of being and changing.

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

# so that we can generate links to more information about each component of the reading.
urlTemplate <- c("https://ichingdb.pythonanywhere.com/ichingdb/", "/", "/")

# this is where the hexagrams are randomly generated
loaded <- loadIChing()

TF <- loaded$TF
symSet <- loaded$symSet

str(TF)
summary(TF$PP)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# Examine paths of maximum likelihood between nodes
g.max <- mkGraph(TF, symSet, onlyMax = TRUE)
pr <- getPageRanked(g.max)

# g.max.sim <- mkGraph(TF, symSet, onlyMax = TRUE, useSimilarity = TRUE)
# pr <- getPageRanked(g.max.sim)

decomp <- getDecomposition(g.max)
analyseSubgraph(decomp, rank = 1, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 2, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 3, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 4, pr = pr, urlTemplate = urlTemplate, layoutFunc = layout_with_graphopt)
analyseSubgraph(decomp, rank = 5, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 6, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 7, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 8, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 9, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 10, pr = pr, urlTemplate = urlTemplate)
analyseSubgraph(decomp, rank = 11, pr = pr, urlTemplate = urlTemplate)



# Examine all paths between nodes
g.full <- mkGraph(TF, symSet)

pr <- getPageRanked(g.full, layoutFunc = layout_with_gem)

# keep only the top percentile
g <- trimGraph(g.full, percentile = 0.5)

pr <- getPageRanked(g, layoutFunc = layout_with_graphopt)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr, urlTemplate = urlTemplate)
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

