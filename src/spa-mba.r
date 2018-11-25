
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")

loadMB <- function() {
  out <- read.csv(file = "./data/myers-briggs-dataset-01.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  out$D1 <- as.factor(out$D1)
  out$D2 <- as.factor(out$D2)
  out$D3 <- factor(out$D3, labels = c("F", "T"))
  out$D4 <- as.factor(out$D4)
  out$PPTL <- as.double(out$PPTL)
  out$PPTU <- as.double(out$PPTU)
  out$PPT <- as.double(out$PPT - 0.26 / 16)
  out$Pair <- as.integer(out$Pair)
  # Assemble results
  out <- out %>%
    mutate(LUP = (PPTL + PPTU) / 2 + 1 / 16) %>%
    mutate(AP = (LUP + PPT) / 2) %>% # don't use PMT, its only an estimate
    mutate(APN = (AP - min(AP)) / (max(AP) - min(AP))) %>%
    mutate(Num = rownames(out)) %>%
    select(Num, 1:7, AP, APN)
  rownames(out) <- out$Type
  # Add group id column
  out$Grp <- rep(NA, nrow(out))
  out[out$D2 == "N" & out$D4 == "J", ]$Grp <- 1
  out[out$D3 == "T" & out$D4 == "P", ]$Grp <- 2
  out[out$D3 == "F" & out$D4 == "P", ]$Grp <- 3
  out[out$D2 == "S" & out$D4 == "J", ]$Grp <- 4
  out$Grp <- as.factor(out$Grp)
  return(out)
}

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

paths[1:20,]
