
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

loadData323 <- function() {
  out <- read.csv(file = "./data/dataset-323.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  out$D1 <- as.factor(out$D1)
  out$D2 <- as.factor(out$D2)
  out$D3 <- as.factor(out$D3)
  out$PP <- as.double(out$PP)
  rownames(out) <- out$Type
  return(out)
}

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


