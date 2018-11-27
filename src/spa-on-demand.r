
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

# Here I avoid pre-computing scenarios and paths.
# Using only the on-demand functions, what analysis can I do?

# The basic tools are:
# get particular adaptation scenarios
getScenarios(TF, symSet, c("X,_,_", "_,X,_", "_,false,X"))
# get adaptation scenarios that meet certain constraints
getScenarios(TF, symSet, getMasks(symSet, constraints = c("BATTERY", "", "")))
# get particular evolutionary paths
getPathOnDemand(TF, c("1,THEFT", "2,false", "3,false", "1,NARCOTICS", "2,true", "3,false"), symSet)
getPathOnDemand(TF, c("1,THEFT", "2,false", "3,false"), symSet)
getPathOnDemand(TF, c("1,NARCOTICS", "2,true", "3,false"), symSet)

# But what do these mean in a general context?
# How do we use them to infer useful information?

getScenarios(TF, symSet, c("THEFT,X,_", "THEFT,_,X"))
