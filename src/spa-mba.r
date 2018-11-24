
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

MB <- loadMB()

MBsymbolSet <- data.frame(c1 = c("X", "_", "I", "E"),
                          c2 = c("X", "_", "N", "S"),
                          c3 = c("X", "_", "F", "T"),
                          c4 = c("X", "_", "P", "J") )

MBS <- getScenarios(MB, MBsymbolSet, cname = "AP", nSkip = 3)

# sortedPlot(MBS, "GroupAP", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of choice difference values for all adaptation scenarios. Diff = yin% - yang%
# sortedPlot(MBS, "Diff1", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of demographic pressure values for all adaptation scenarios. DP = GroupAP * Diff 
# sortedPlot(MBS, "DP1", datatype = "Adaptation Scenarios")
# 
# # Plot of the sorted spectrum of TP values for all adaptation scenarios. TP = Diff / GroupAP
# sortedPlot(MBS, "TP1", datatype = "Adaptation Scenarios")
# 
# # What are the most discouraged and encouraged adaptations, with the highest absolute choice difference?
# head(MBS[order(-abs(MBS$Diff1)), ])
# 
# # What are the adaptations with the least choice difference?
# head(MBS[order(abs(MBS$Diff1)), ])
# 
# # What are the adaptations with the most targeted pressure?
# head(MBS[order(-abs(MBS$TP1)), ])
# 
# # What are the adaptations with the least targeted pressure?
# head(MBS[order(abs(MBS$TP1)), ])


# getPath works differently because the parameter symbols are not unique, i.e. D1 = "A" & D2 = "A" is possible.

getPath(MBS, c("1,E", "3,F", "4,J", "2,N"), MBsymbolSet)

getPath(MBS, c("1,E", "3,F", "4,J", "2,N", "1,I"), MBsymbolSet)

paths <- getAllPaths(MBS, MBsymbolSet)

paths["NIPF",]
paths[1:20,]
