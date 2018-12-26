# SPA-COMP-ORD

# Here I explore ways of comparing a series or ordinal data sets.
# I use a temporal series with 5 snapshots of results over time for an opinion poll about the Yellow Vests movement in France.
# The values in:
#    TFx contains percentage prevalences,
#    TFv contains the difference in PP between successive data sets
#    TFa contains the difference in differences.

# What if I just plug the values in and do the analysis?
# It works. Mathematically speaking the algorithm only looks at the difference between type-frequency values.
# So it doesn't matter if some are positive and some negative. Or that they don't sum to 100%.
# But do the results make sense? How to interpret the results?


#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SCA/") # edit to suit your environment
source("./src/utils.r")
source("./src/utils-loaders.r")

loaded <- loadYV()

symSet <- loaded$symSet
TFx <- loaded$TF

TFv <- TFx
TFv[, 3:6] <- TFv[, 4:7] - TFv[, 3:6]
TFv$PP5 <- NULL

TFa <- TFv
TFa[, 3:5] <- TFa[, 4:6] - TFa[, 3:5]
TFa$PP4 <- NULL

LO <- layout_in_circle(mkGraph(TFx, symSet, ppCol = "PP1", nSkip = 1))

#### Maximum Pressure Network ####
# using difference

# (x, v, a) for PP1
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# All positions
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, ppCol = "PP5", nSkip = 1), layout = LO)

# All velocities
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)

# All accelerations
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)


# using similarity

# (x, v, a) for PP1
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# All positions
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP5", nSkip = 1), layout = LO)

# All velocities
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)

# All accelerations
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)



#### Full Network ####
# using difference

# (x, v, a) for PP1
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP1", nSkip = 1), layout = LO)

# All positions
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP5", nSkip = 1), layout = LO)

# All velocities
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP4", nSkip = 1), layout = LO)

# All accelerations
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP3", nSkip = 1), layout = LO)

# using similarity

# (x, v, a) for PP1
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# All positions
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP5", nSkip = 1), layout = LO)

# All velocities
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)

# All accelerations
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)




#### Examine the total pressures along the path from hostile to support. ####
# Do this for every snapshot of position, velocity and acceleration.

# Position 1 & 2
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP2", nSkip = 1)

# Velocity 1
getPathOnDemand(TFv, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)

# Position 2 & 3
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP2", nSkip = 1)
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP3", nSkip = 1)

# Velocity 2
getPathOnDemand(TFv, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP2", nSkip = 1)

# Acceleration 1
getPathOnDemand(TFa, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)

# Position 3 & 4
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP3", nSkip = 1)
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP4", nSkip = 1)

# Velocity 3
getPathOnDemand(TFv, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP3", nSkip = 1)

# Acceleration 2
getPathOnDemand(TFa, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP2", nSkip = 1)

# Position 4 & 5
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP4", nSkip = 1)
getPathOnDemand(TFx, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP5", nSkip = 1)

# Velocity 4
getPathOnDemand(TFv, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP4", nSkip = 1)

# Acceleration 3
getPathOnDemand(TFa, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP3", nSkip = 1)


# could also use
scenarios <- getScenarios(TFa, symSet, ppCol = "PP3", nSkip = 1)
getPath(scenarios, c("1,s2"), symSet, chosen = c("s-2"), withTotals = FALSE)




#### In dependence order ####

#### Full Network ####
# using difference

# Position 1 & 2
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP2", nSkip = 1), layout = LO)

# Velocity 1
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP1", nSkip = 1), layout = LO)

# Position 2 & 3
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP3", nSkip = 1), layout = LO)

# Velocity 2
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP2", nSkip = 1), layout = LO)

# Acceleration 1
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP1", nSkip = 1), layout = LO)

# Position 3 & 4
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP4", nSkip = 1), layout = LO)

# Velocity 3
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP3", nSkip = 1), layout = LO)

# Acceleration 2
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP2", nSkip = 1), layout = LO)

# Position 4 & 5
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, ppCol = "PP5", nSkip = 1), layout = LO)

# Velocity 4
getPageRanked(mkGraph(TFv, symSet, ppCol = "PP4", nSkip = 1), layout = LO)

# Acceleration 3
getPageRanked(mkGraph(TFa, symSet, ppCol = "PP3", nSkip = 1), layout = LO)


# using similarity

# Position 1 & 2
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)

# Velocity 1
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# Position 2 & 3
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)

# Velocity 2
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)

# Acceleration 1
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# Position 3 & 4
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)

# Velocity 3
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)

# Acceleration 2
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP2", nSkip = 1), layout = LO)

# Position 4 & 5
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)
getPageRanked(mkGraph(TFx, symSet, useSimilarity = TRUE, ppCol = "PP5", nSkip = 1), layout = LO)

# Velocity 4
getPageRanked(mkGraph(TFv, symSet, useSimilarity = TRUE, ppCol = "PP4", nSkip = 1), layout = LO)

# Acceleration 3
getPageRanked(mkGraph(TFa, symSet, useSimilarity = TRUE, ppCol = "PP3", nSkip = 1), layout = LO)





#### explore the mean of all snapshots ####

meanSnapshot <- function(tf, initCol, lastCol) {
  out <- tf
  for (r in seq(nrow(out))) {
    out[r, initCol] <- mean(as.double(as.vector(out[r, initCol:lastCol])))
  }
  return(out[, 1:3])
}

TFxm <- meanSnapshot(TFx, 3, 7)
TFvm <- meanSnapshot(TFv, 3, 6)
TFam <- meanSnapshot(TFa, 3, 5)

# Position
# max diff
getPageRanked(mkGraph(TFxm, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# max sim
getPageRanked(mkGraph(TFxm, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# full diff
getPageRanked(mkGraph(TFxm, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
# full sim
getPageRanked(mkGraph(TFxm, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# Velocity
# max diff
getPageRanked(mkGraph(TFvm, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# max sim
getPageRanked(mkGraph(TFvm, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# full diff
getPageRanked(mkGraph(TFvm, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
# full sim
getPageRanked(mkGraph(TFvm, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)

# Acceleration
# max diff
getPageRanked(mkGraph(TFam, symSet, onlyMax = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# max sim
getPageRanked(mkGraph(TFam, symSet, onlyMax = TRUE, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)
# full diff
getPageRanked(mkGraph(TFam, symSet, ppCol = "PP1", nSkip = 1), layout = LO)
# full sim
getPageRanked(mkGraph(TFam, symSet, useSimilarity = TRUE, ppCol = "PP1", nSkip = 1), layout = LO)



#### Examine the total pressures along the path from hostile to support. ####
# Do this for every snapshot of position, velocity and acceleration.

# Position
getPathOnDemand(TFxm, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)
# Velocity
getPathOnDemand(TFvm, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)
# Acceleration
getPathOnDemand(TFam, symSet, chosen = c("s-2"), c("1,s2"), withTotals = FALSE, ppCol = "PP1", nSkip = 1)














# Position 1 & 2

# Velocity 1

# Position 2 & 3

# Velocity 2

# Acceleration 1

# Position 3 & 4

# Velocity 3

# Acceleration 2

# Position 4 & 5

# Velocity 4

# Acceleration 3

