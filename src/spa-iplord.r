
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SCA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

Data <- read.table("./data/IPLORD/iplord_public_2018_10000.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
# Data <- read.table("./data/IPLORD/iplord_public_2018.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
colnames(Data)
Data$cleanname <- factor(Data$cleanname)
Data <- select(Data, year, cleanname, p_chemistry_filed, p_electrical_filed, p_instruments_filed, p_mechanical_filed, p_others_filed)

# summary(Data$p_chemistry_filed)
# summary(Data$p_electrical_filed)
# summary(Data$p_instruments_filed)
# summary(Data$p_mechanical_filed)
# summary(Data$p_others_filed)

# convert numberic to binned ordinal
# Data$p_chemistry_filed <- n2bf(Data$p_chemistry_filed, 3)


Data.years <- split(Data, Data$year)
for (i in 1:length(Data.years)) {
  Data.years[[i]] <- select(Data.years[[i]], -year)
  Data.years[[i]] <- gather(Data.years[[i]], type, PP, p_chemistry_filed:p_others_filed, factor_key=TRUE)
  Data.years[[i]]$PP <- Data.years[[i]]$PP / sum(Data.years[[i]]$PP, na.rm = TRUE) * 100
}
Data.years$'2012'
sum(Data.years$'2012'$PP, na.rm = TRUE)

colnames(Data.years$'2012')

str(Data.years$'2012')

TF <- Data.years$'2012'

# getSymSetFromTF
symSet <- getSymSetFromTF(TF, c(1,2))

# replace NA with 0


# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# using difference
g.max.diff <- mkGraph(TF, symSet, onlyMax = TRUE)
pr <- getPageRanked(g.max.diff, layoutFunc = layout_with_gem)

# using similarity
g.max.sim <- mkGraph(TF, symSet, onlyMax = TRUE, useSimilarity = TRUE)
pr <- getPageRanked(g.max.sim, layoutFunc = layout_with_gem)

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
g <- trimGraph(g.full.diff, percentile = 0.843)
pr <- getPageRanked(g, layoutFunc = layout_with_gem)

# using similarity
# PPT use 0.9753
# PPF use 0.9987
# PPM use 1.0, the min connected is the full graph
g <- trimGraph(g.full.sim, percentile = 0.9987)
pr <- getPageRanked(g, layoutFunc = layout_with_gem)

# adjust the percentile value until there are several clusters with minimal isolated nodes (0.359)

# using difference
# PPT use 0.7
# PPF use 0.7
# PPM use 0.7
g <- trimGraph(g.full.diff, percentile = 0.7)
pr.diff <- getPageRanked(g, layoutFunc = layout_with_dh)
decomp.diff <- getDecomposition(g)
analyseSubgraph(decomp.diff, rank = 1, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 2, pr = pr.diff)
analyseSubgraph(decomp.diff, rank = 3, pr = pr.diff)

# using similarity
# PPT use 0.7
# PPF use 0.8
# PPM use 0.5
g <- trimGraph(g.full.sim, percentile = 0.8)
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
g <- trimGraph(g.full.diff, percentile = 0.6)
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
g <- trimGraph(g.full.sim, percentile = 0.3)
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


