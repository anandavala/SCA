
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

loadCrime <- function() {
  Data <- read.table("./data/Crimes_-_2001_to_present-short.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
  # Data <- read.table("./data/Crimes_-_2001_to_present.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
  Data$IUCR <- factor(Data$IUCR)
  Data$Primary.Type <- factor(Data$Primary.Type)
  Data$Description <- factor(Data$Description)
  Data$Location.Description <- factor(Data$Location.Description)
  Data$Arrest <- factor(Data$Arrest)
  Data$Domestic <- factor(Data$Domestic)
  Data$Beat <- factor(Data$Beat)
  Data$District <- factor(Data$District)
  Data$Ward <- factor(Data$Ward)
  Data$Community.Area <- factor(Data$Community.Area)
  Data$FBI.Code <- factor(Data$FBI.Code)
  Data$Year <- factor(Data$Year)
  Data <- select(Data, -ID, -Case.Number, -Date, -Block, -X.Coordinate, -Y.Coordinate, -Latitude, -Longitude, -Updated.On, -Location)
  # Data$X.Coordinate <- n2bf(Data$X.Coordinate, 4)
  # Data$Y.Coordinate <- n2bf(Data$Y.Coordinate, 4)
  return(Data)
}

Data <- loadCrime()
str(Data)
origData <- Data

Data <- origData
str(Data)

origSymbolSet <- getSymbolSet(Data)

# standardise level names in preparation for SPA
# for (c in 1:ncol(Data)) {
#   # n <- length(levels(Data[,c]))
#   # levels(Data[,c]) <- strsplit(paste(rep("D", n), 1:n, sep = "", collapse = ","), split = ",")[[1]]
#   levels(Data[,c]) <- 1:length(levels(Data[,c]))
# }
# for (c in 1:ncol(Data)) levels(Data[,c]) <- DBLLETTERS[1:length(levels(Data[,c]))]

levels(Data[,"Community.Area"]) <- DBLLETTERS[1:length(levels(Data[,"Community.Area"]))]

symbolSet <- getSymbolSet(Data)

# focus on relevant data
dims <- c(2, 5, 6) # columns of interest
colnames(Data)[dims]
symSet <- symbolSet[,dims] # strip down to relevant columns
symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs
origSymSet <- origSymbolSet[,dims] # strip down to relevant columns
origSymSet <- origSymSet[rowSums(is.na(origSymSet)) != ncol(origSymSet), ] # strip rows with all NAs

# # each yearly dataset is a 'population sample'
# Data.years <- split(Data, Data$Year)
# names(Data.years)
# 
# # choose which year to analyse
# Data <- Data.years[["2015"]]

TF <- getTypeFreqs(Data, dims, symSet)
str(TF)
# TF is now equivalent to MB it is a type frequency table

# TF types sorted by prevalence
TF[order(TF$PP), ]

# Plot of the sorted spectrum of prevalence values for all types. AP = %ofPop.
sortedPlot(TF, "PP", ptsize = 3, datatype = "Types")

# Cluster Diagram
gDist <- daisy(TF, metric = "gower")
hc <- hclust(gDist)
ggdendrogram(hc)

scenarios <- getScenarios(TF, symSet)
colnames(scenarios)

# Testing the new on-demand approach by comparing it to the results of the old pre-computed approach
# compute path statistics without needing to first pre-compute all adaptation scenarios
getPathOnDemand(TF, c("1,THEFT", "2,false", "3,false", "1,NARCOTICS", "2,true", "3,false"), symSet)
# compare with
getPath(scenarios, c("1,THEFT", "2,false", "3,false", "1,NARCOTICS", "2,true", "3,false"), symSet)

# compute particular adaptation statistics without pre-computing them all
scenarios[1,]
getScenarios(TF, symSet, getMasks(symSet, constraints = c("", "_", "_")))

scenarios[10,]
getScenarios(TF, symSet, getMasks(symSet, constraints = c("_", "", "_")))

# so now we don't need to pre-compute all scenarios anymore!
# unless we wish to analyse the entire dataset, such as sorting to find the adaptations with the most and least pressure.

# End test

# Plot of the sorted spectrum of group prevalence values for all adaptation scenarios. GroupAP = %ofPop
sortedPlot(scenarios, "GroupAP", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
sortedPlot(scenarios, "Diff1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff3", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff16", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "Diff34", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
sortedPlot(scenarios, "DP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP3", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP16", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "DP34", datatype = "Adaptation Scenarios")

# Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
sortedPlot(scenarios, "TP1", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP2", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP3", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP16", datatype = "Adaptation Scenarios")
sortedPlot(scenarios, "TP34", datatype = "Adaptation Scenarios")


paths <- getAllPaths(scenarios, symSet)

str(paths)

sortedPlot(paths, "TGroupAP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total choice difference values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDiff", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of demographic pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChDP", lblsize = 4, datatype = "Evolutionary Paths")

# Plot of the sorted spectrum of total targeted pressure values for all 4 step evolutionary paths.
sortedPlot(paths, "TChTP", lblsize = 4, datatype = "Evolutionary Paths")


