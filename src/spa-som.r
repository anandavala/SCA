
#### Initialise ####
rm(list=ls())
setwd("~/Documents/Projects/SPA/") # edit to suit your environment
source("./src/spa-utils.r")
source("./src/spa-utils-loaders.r")
library(kohonen)
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4","#ff7f0e","#2ca02c", "#d62728","#9467bd","#8c564b","#e377c2")

# # Using IPLORD
# origData <- read.table("./data/IPLORD/iplord_public_2018_5000000.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
# # Data <- read.table("./data/IPLORD/iplord_public_2018.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
# origData$cleanname <- factor(origData$cleanname)
# Data <- select(origData, year, cleanname, p_chemistry_filed, p_electrical_filed, p_instruments_filed, p_mechanical_filed, p_others_filed)
# Data <- na.omit(Data)
# colnames(Data)
# 
# data_train <- Data[,3:7]



# Using I Ching
Data <- getHexagrams(10000, asFactor = FALSE)
str(Data)
head(Data)

data_train <- Data

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(data_train))

# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 8, ydim=8, topo="hexagonal") #, toroidal = TRUE)

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE )

#Training progress for SOM
plot(som_model, type="changes")

#Node count plot
plot(som_model, type="count", main="Node Counts", palette.name=coolBlueHotRed)

# U-matrix visualisation
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=coolBlueHotRed)

# Weight Vector View
plot(som_model, type="codes", palette.name=coolBlueHotRed)

# Kohonen Heatmap creation
var <- 6

# plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)


# Unscaled Heatmaps
#define the variable to plot 
var_unscaled <- aggregate(as.numeric(na.omit(data_train)[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)


# # Plotting unscaled variables when you there are empty nodes in the SOM
# var_unscaled <- aggregate(as.numeric(na.omit(data_train)[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
# names(var_unscaled) <- c("Node", "Value")
# # Add in NA values for non-assigned nodes - first find missing nodes:
# missingNodes <- which(!(seq(1,nrow(som_model$codes[[1]])) %in% var_unscaled$Node))
# # Add them to the unscaled variable data frame
# if (length(missingNodes) > 0) var_unscaled <- rbind(var_unscaled, data.frame(Node=missingNodes, Value=NA))
# # order the resulting data frame
# var_unscaled <- var_unscaled[order(var_unscaled$Node),]
# # Now create the heat map only using the "Value" which is in the correct order.
# plot(som_model, type = "property", property=var_unscaled$Value, main=names(data_train)[var], palette.name=coolBlueHotRed)


# Viewing WCSS for kmeans
mydata <- som_model$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss )

# Visualising cluster results
## use hierarchical clustering to cluster the codebook vectors
k <- 8
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), k)
# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)


Data$node <- som_model$unit.classif
Data$node <- factor(Data$node)
summary(Data$node)

head(Data[Data$node == 1,])
head(Data[Data$node == 2,])
head(Data[Data$node == 3,])
head(Data[Data$node == 4,])

# For the I Ching the SOM has assigned one node to each hexagram
# Thereby performing a type-casting of the 6D binary data to reduce it to 2D 8-ary data.
# This has reduced the number of dimensions whilst increasing the number of states per dimension.
# It keeps the same number of types, hence each hexagram gets its own node.


# get vector with cluster value for each original data sample
cluster_assignment <- som_cluster[som_model$unit.classif]
# for each of analysis, add the assignment as a column in the original data:
Data$cluster <- cluster_assignment
Data$cluster <- factor(Data$cluster)
summary(Data$cluster)


head(Data[Data$cluster == 1,])
head(Data[Data$cluster == 2,])
head(Data[Data$cluster == 3,])
head(Data[Data$cluster == 4,])






# compare with multisom
library(multisom)
?multisom.batch
res<- multisom.batch(Data,xheight= 8, xwidth= 8,"hexagonal",
                     min.radius=0.00010,max.radius=0.002,
                     maxit=1000,"random","gaussian","ch")
str(res)
res$All.index.by.layer
res$Best.nc
res$Best.partition





# compare with t-SNE

traindata <- Data[1:1000,]
trn <- data.matrix(traindata)

require(tsne)

cols <- rainbow(10)

# this is the epoch callback function used by tsne. 
# x is an NxK table where N is the number of data rows passed to tsne, and K is the dimension of the map. 
# Here, K is 2, since we use tsne to map the rows to a 2D representation (map).
ecb = function(x, y){ plot(x, t='n') }

tsne_res = tsne(trn, epoch_callback = ecb, perplexity=50, epoch=50)





# compare with Rt-SNE

traindata <- Data[1:1000,]
trn <- data.matrix(traindata)

require(Rtsne)

# perform dimensionality redcution from 64D to 2D
tsne <- Rtsne(as.matrix(trn), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=3)
tsne$Y
# display the results of t-SNE
cols <- rainbow(10)
plot(tsne$Y)

str(tsne$Y)
?Rtsne

library("scatterplot3d") # load
scatterplot3d(tsne$Y, angle = 55)

names(tsne)






# Compare with MDS (doesnt seem to work well)
# MDS takes too long with lots of data
smallData <- Data[1:1000, ]

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(smallData[, 1:6]) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS")
# text(x, y, labels = row.names(Data), cex=.7) 




# compare with supersom
# the data is a list of matrices, each matrix is reduced to its own layer.

## supersom example
data(yeast)
names(yeast)
yeast.supersom <- supersom(yeast, somgrid(6, 6, "hexagonal"), rlen = 1000,
                           whatmap = c("alpha", "cdc15", "cdc28", "elu"),
                           maxNA.fraction = .5)

plot(yeast.supersom, "changes")

obj.classes <- as.integer(yeast$class)
colors <- c("yellow", "green", "blue", "red", "orange")
plot(yeast.supersom, type = "mapping", col = colors[obj.classes],
     pch = obj.classes, main = "yeast data")







# Compare with SPA

Data$L1 <- factor(Data$L1)
Data$L2 <- factor(Data$L2)
Data$L3 <- factor(Data$L3)
Data$L4 <- factor(Data$L4)
Data$L5 <- factor(Data$L5)
Data$L6 <- factor(Data$L6)


symSet <- getSymSetFromData(Data[,1:6])

TF <- getTypeFreqs(Data[,1:6], symSet)
# rownames(TF) <- c(2, 24, 7, 19, 15, 36, 46, 11, 16, 51, 40, 54, 62, 55, 32, 34, 8, 3, 29, 60, 39, 63, 48, 5, 45, 17, 47, 58, 31, 49, 28, 43, 23, 27, 4, 41, 52, 22, 18, 26, 35, 21, 64, 38, 56, 30, 50, 14, 20, 42, 59, 61, 53, 37, 57, 9, 12, 25, 6, 10, 33, 13, 44, 1)
labels = c("1 Force", "2 Field", "3 Sprouting", "4 Enveloping", "5 Attending", "6 Dispute", "7 Legions", "8 Grouping", "9 Small Accumulates", "10 Treading", "11 Pervading", "12 Obstruction", "13 Harmonising People", "14 Great Being", "15 Humbling", "16 Providing For", "17 Following", "18 Corruption", "19 Nearing", "20 Viewing", "21 Biting Through", "22 Adorning", "23 Stripping", "24 Returning", "25 Disentangling", "26 Great Accumulates", "27 Jaws", "28 Great Traverses", "29 Pit", "30 Radiance", "31 Conjoining", "32 Persevering", "33 Retiring", "34 Great Invigorating", "35 Flourishing", "36 Brightness Hiding", "37 Dwelling People", "38 Divergence", "39 Limping", "40 Deliverance", "41 Diminishing", "42 Augmenting", "43 Deciding", "44 Coupling", "45 Great Works", "46 Ascending", "47 Confining", "48 The Well", "49 Skinning", "50 Vessel", "51 Shake", "52 Bound", "53 Gradual Advance", "54 Marrying the Maiden", "55 Abounding", "56 Sojourning", "57 Lady of Fates", "58 Open Expression", "59 Dispersing", "60 Articulating", "61 Centring", "62 Small Traverses", "63 Already Crossing", "64 Not Yet Crossing")
b2IC = c(2, 24, 7, 19, 15, 36, 46, 11, 16, 51, 40, 54, 62, 55, 32, 34, 8, 3, 29, 60, 39, 63, 48, 5, 45, 17, 47, 58, 31, 49, 28, 43, 23, 27, 4, 41, 52, 22, 18, 26, 35, 21, 64, 38, 56, 30, 50, 14, 20, 42, 59, 61, 53, 37, 57, 9, 12, 25, 6, 10, 33, 13, 44, 1)
rownames(TF) <- labels[b2IC]


head(Data[Data$node == 1,])
nrow(Data[Data$node == 1,])

sdn <- summary(Data$node)
sum(sdn)


# The two approaches give the same count for each hexagram.
# This suggests that a SOM performs some kind of type-casting of arbitrary numeric data.









