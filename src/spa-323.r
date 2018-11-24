
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

df <- loadData323()

sum(df$PP)

# types sorted by prevalence
df[order(df$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. PP = %ofPop.
sortedPlot(df, "PP", ptsize = 3, datatype = "Generic 323 Types")

# Cluster Diagram
gDist <- daisy(df[,c("D1","D2","D3","PP")], metric = "gower")
ggdendrogram(hclust(gDist))

symbolSet <- data.frame(c1 = c("X", "_", "A", "B", "C"),
                        c2 = c("X", "_", "A", "B", NA),
                        c3 = c("X", "_", "A", "B", "C") )

scenarios <- getScenarios(df, symbolSet)

colnames(scenarios)
sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")

sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff3", datatype = "Adaptation Scenarios")

sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP3", datatype = "Adaptation Scenarios")

sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP3", datatype = "Adaptation Scenarios")


