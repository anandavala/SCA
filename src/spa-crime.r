
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")

loaded <- loadCrime()

TF <- loaded$TF
symSet <- loaded$symSet

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# insert graph analysis here...

# # Cluster Diagram
# gDist <- daisy(TF, metric = "gower")
# hc <- hclust(gDist)
# ggdendrogram(hc)
# 
# scenarios <- getScenarios(TF, symSet)
# colnames(scenarios)
# 
# # Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
# sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
# sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff3", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff16", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "Diff34", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
# sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP3", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP16", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "DP34", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
# sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP2", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP3", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP16", datatype = "Adaptation Scenarios")
# sortedPlot(scenarios, "TP34", datatype = "Adaptation Scenarios")
# 
# 
# paths <- getAllPaths(scenarios, symSet)
# 
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
# 
# 
