
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

loaded <- loadCRX()

TF <- loaded$TF
symSet <- loaded$symSet

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all Myers-Briggs types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

g.max <- mkGraph(TF, symSet, onlyMax = TRUE)

pr <- getPageRanked(g.max, layout = layout_with_graphopt)

# adjust the percentile value until there are several clusters with minimal isolated nodes
g <- trimGraph(g.max, percentile = 0.75)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)
analyseSubgraph(decomp, rank = 3, pr = pr)




g.full <- mkGraph(TF, symSet)

pr <- getPageRanked(g.full)

# adjust the percentile value until there are several clusters with minimal isolated nodes
g <- trimGraph(g.full, percentile = 0.8)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)
analyseSubgraph(decomp, rank = 3, pr = pr)

# adjust the percentile value until there are several clusters with minimal isolated nodes
g <- trimGraph(g.full, percentile = 0.6)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)

# adjust the percentile value until there are several clusters with minimal isolated nodes
g <- trimGraph(g.full, percentile = 0.5)

pr <- getPageRanked(g)

decomp <- getDecomposition(g)

analyseSubgraph(decomp, rank = 1, pr = pr)
analyseSubgraph(decomp, rank = 2, pr = pr)

analyseAllSubgraphs(decomp, pr = pr)

# Interactive step through
analyseAllSubgraphs(decomp, interactive = TRUE, pr = pr)






# # Cluster Diagram
# gDist <- daisy(TF, metric = "gower")
# hc <- hclust(gDist)
# ggdendrogram(hc)
# 
# 
# scenarios <- getScenarios(TF, symSet, cName = "PP", nSkip = 0)
# 
# getPath(scenarios, c("1,b", "2,u", "3,g", "4,t", "5,t"), symSet)
# 
# # Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
# sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
# sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff3", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
# sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP3", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
# sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP3", datatype = "Adaptation Scenarios")
# 
# 
# paths <- getAllPaths(scenarios, symSet)
# str(paths)
# 
# sortedPlot(paths, "TGroupAP", lblsize = 4, datatype = "Evolutionary Paths")
# 
# # Plot of the sorted spectrum of total choice difference values for all 4 step evolutionary paths.
# sortedPlot(paths, "TChDiff", lblsize = 4, datatype = "Evolutionary Paths")
# 
# # Plot of the sorted spectrum of demographic pressure values for all 4 step evolutionary paths.
# sortedPlot(paths, "TChDP", lblsize = 4, datatype = "Evolutionary Paths")
# 
# # Plot of the sorted spectrum of total targeted pressure values for all 4 step evolutionary paths.
# sortedPlot(paths, "TChTP", lblsize = 4, datatype = "Evolutionary Paths")
