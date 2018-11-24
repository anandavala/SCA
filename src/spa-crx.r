
#### Initialise ####
rm(list=lss())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

# load all data
Data <- loadCRX()

origSymbolSet <- getSymbolSet(Data)

# standardise level names in preparation for SPA
for (c in 1:ncol(Data)) levels(Data[,c]) <- LETTERS[1:length(levels(Data[,c]))]

symbolSet <- getSymbolSet(Data)

# focus on relevant data
dims <- c(1, 4, 5, 9, 10) # columns of interest
colnames(Data)[dims]
symSet <- symbolSet[,dims] # strip down to relevant columns
symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs
origSymSet <- origSymbolSet[,dims] # strip down to relevant columns
origSymSet <- origSymSet[rowSums(is.na(origSymSet)) != ncol(origSymSet), ] # strip rows with all NAs

TF <- getTypeFreqs(Data, dims, symSet)
str(TF)
# TF is now equivalent to MB it is a type frequency table

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all Myers-Briggs types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# Cluster Diagram
gDist <- daisy(TF, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)


scenarios <- getScenarios(TF, symSet, cname = "PP", nSkip = 0)

getPath(scenarios, c("1,B", "2,C", "3,C", "4,B", "5,B"), symSet)

# Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff3", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP3", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP3", datatype = "Adaptation Scenarios")


paths <- getAllPaths(scenarios, symSet)
paths[1,]
str(paths)

sortedPlot(paths, "TGroupAP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total choice difference values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDiff", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of demographic pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total targeted pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChTP", lblsize = 4, datatype = "Evolutionary Paths")
