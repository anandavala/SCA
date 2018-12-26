#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SCA/") # edit to suit your environment
source("./src/utils.r")
source("./src/utils-loaders.r")

spec <- list(c("A", "B"),
             c("A", "B", "C"),
             c("A", "B", "C", "D") )
symSet <- getSymSetFromSpec(spec)

TF <- getNGramTF(symSet, 10000, TRUE)
TF
summary(TF$PP)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# using difference
g.max.diff <- mkGraph(TF, symSet, onlyMax = TRUE)
pr <- getPageRanked(g.max.diff, layoutFunc = layout_with_dh)

getScenarios(tf, symSet, "A,A,C>X")


# using similarity
g.max.sim <- mkGraph(TF, symSet, onlyMax = TRUE, useSimilarity = TRUE)
pr <- getPageRanked(g.max.sim, layoutFunc = layout_with_dh)

# using difference
g.full.diff <- mkGraph(TF, symSet)
pr <- getPageRanked(g.full.diff, layoutFunc = layout_with_gem)

# using similarity
g.full.sim <- mkGraph(TF, symSet, useSimilarity = TRUE)
pr <- getPageRanked(g.full.sim, layoutFunc = layout_with_gem)

# adjust the percentile value until all nodes are simply connected

# using difference
# PPT use 0.83
# PPF use 0.843
# PPM use 0.901
g <- trimGraph(g.full.diff, percentile = 0.5)
pr <- getPageRanked(g, layoutFunc = layout_with_dh)

# using similarity
# PPT use 0.9753
# PPF use 0.9987
# PPM use 0.99256
g <- trimGraph(g.full.sim, percentile = 0.3)
pr <- getPageRanked(g, layoutFunc = layout_with_gem)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.359)

# using difference
# PPT use 0.7
# PPF use 0.7
# PPM use 0.7
g <- trimGraph(g.full.diff, percentile = 0.4)
pr.diff <- getPageRanked(g, layoutFunc = layout_with_dh)
decomp.diff <- getDecomposition(g)
analyseSubgraph(decomp.diff, rank = 1, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 2, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 3, pr = pr.diff)

# using similarity
# PPT use 0.7
# PPF use 0.8
# PPM use 0.5
g <- trimGraph(g.full.sim, percentile = 0.2)
pr.sim <- getPageRanked(g, layoutFunc = layout_with_dh)
decomp.sim <- getDecomposition(g)
analyseSubgraph(decomp.sim, rank = 1, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 2, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 3, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 4, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 5, pr = pr.sim)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.358)

# using difference
# PPT use 0.6
# PPF use 0.6
# PPM use 0.6
g <- trimGraph(g.full.diff, percentile = 0.1)
pr.diff <- getPageRanked(g, layoutFunc = layout_with_dh)
decomp.diff <- getDecomposition(g)
analyseSubgraph(decomp.diff, rank = 1, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 2, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 3, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 4, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 5, pr = pr.diff)

# using similarity
# PPT use 0.3
# PPF use 0.3
# PPM use 0.3
g <- trimGraph(g.full.sim, percentile = 0.05)
pr.sim <- getPageRanked(g, layoutFunc = layout_with_dh)
decomp.sim <- getDecomposition(g)
analyseSubgraph(decomp.sim, rank = 1, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 2, pr = pr.sim)
analyseSubgraph(decomp.sim, rank = 3, pr = pr.sim)



# using difference
analyseAllSubgraphs(decomp.diff, pr = pr.diff)
# Interactive step through
analyseAllSubgraphs(decomp.diff, interactive = TRUE, pr = pr.diff)

# using similarity
analyseAllSubgraphs(decomp.sim, pr = pr.sim)
# Interactive step through
analyseAllSubgraphs(decomp.sim, interactive = TRUE, pr = pr.sim)




subVec <- strsplit("J", split = ">")[[1]]

symSet

complexSymSet(symSet)
