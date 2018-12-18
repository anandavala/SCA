# SPA-MBA

#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

loaded <- loadMB()

TF <- loaded$TF
symSet <- loaded$symSet

# compute differences between men and women
TF <- mutate(TF, PPFM = PPF - PPM, PPMF = PPM - PPF)
rownames(TF) <- paste(TF$Type, TF$Name, sep = " - ")

# here we select: 
#   PPT for both females and males
#   PPF for females
#   PPM for males
#   PPFM for females - males
#   PPMF for males - females
TF <- select(TF, D1, D2, D3, D4, PP = PPM)
str(TF)

summary(TF$PP)

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# using difference
g.max.diff <- mkGraph(TF, symSet, onlyMax = TRUE)
pr <- getPageRanked(g.max.diff, layout = layout_with_gem)

# using similarity
g.max.sim <- mkGraph(TF, symSet, onlyMax = TRUE, useSimilarity = TRUE)
pr <- getPageRanked(g.max.sim, layout = layout_with_gem)

# using difference
g.full.diff <- mkGraph(TF, symSet)
pr <- getPageRanked(g.full.diff, layout = layout_with_gem)

# using similarity
g.full.sim <- mkGraph(TF, symSet, useSimilarity = TRUE)
pr <- getPageRanked(g.full.sim, layout = layout_with_gem)

# adjust the percentile value until all nodes are simply connected

# using difference
# PPT use 0.83
# PPF use 0.843
# PPM use 0.901
g <- trimGraph(g.full.diff, percentile = 0.901)
pr <- getPageRanked(g, layout = layout_with_gem)

# using similarity
# PPT use 0.9753
# PPF use 0.9987
# PPM use 0.99256
g <- trimGraph(g.full.sim, percentile = 0.99256)
pr <- getPageRanked(g, layout = layout_with_gem)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.359)

# using difference
# PPT use 0.7
# PPF use 0.7
# PPM use 0.7
g <- trimGraph(g.full.diff, percentile = 0.7)
pr.diff <- getPageRanked(g, layout = layout_with_dh)
decomp.diff <- getDecomposition(g)
analyseSubgraph(decomp.diff, rank = 1, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 2, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 3, pr = pr.diff)

# using similarity
# PPT use 0.7
# PPF use 0.8
# PPM use 0.5
g <- trimGraph(g.full.sim, percentile = 0.8)
pr.sim <- getPageRanked(g, layout = layout_with_dh)
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
g <- trimGraph(g.full.diff, percentile = 0.6)
pr.diff <- getPageRanked(g, layout = layout_with_dh)
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
g <- trimGraph(g.full.sim, percentile = 0.3)
pr.sim <- getPageRanked(g, layout = layout_with_dh)
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




# scenarios <- getScenarios(TF, symSet, cName = "PP", nSkip = 0)
# head(scenarios)
# 
# getPath(scenarios, c("1,I", "2,N", "3,F", "4,J"), symSet, chosen = c("E","S","T","P"))
