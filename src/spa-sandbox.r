
# install.packages("remotes")
# remotes::install_github("rdpeng/threadpool")

library(threadpool)

data(airquality)

Data <- getHexagrams(20000, asFactor = FALSE)
for (i in 1:9) Data <- rbind(Data, getHexagrams(20000))
Data$L1 <- as.integer(Data$L1)
Data$L2 <- as.integer(Data$L2)
Data$L3 <- as.integer(Data$L3)
Data$L4 <- as.integer(Data$L4)
Data$L5 <- as.integer(Data$L5)
Data$L6 <- as.integer(Data$L6)
str(Data)
# splitData

nbins <- detectCores() - 1

template <- seq(0, 1, length.out = nbins + 1)
smin <- 0
smax <- nrow(Data)
breaks <- c(smin)
range <- smax - smin
for (b in 1:(nbins - 1)){
  brk <- round(smin + range * template[b+1])
  breaks <- c(breaks, brk)
}
breaks <- c(breaks, smax)

listData <- list()
for (b in 1:nbins){
  # listData[[b]] <- as.data.frame(t(Data[breaks[b]:breaks[b+1], ]))
  listData[[b]] <- Data[breaks[b]:breaks[b+1], ]
}
str(listData)
length(listData)
nrow(listData[[1]])
ncol(listData[[1]])

f <- function(df) {
  out <- rep(NA, nrow(df))
  for (r in 1:nrow(df)) {
    out[r] <- sum(df[r,], na.rm = TRUE)
  }
  return(out)
}

system.time({res1 <- f(Data)})
str(res1)

system.time({res3 <- lapply(listData, f)})
str(res3)

library(parallel)
system.time({res4 <- mclapply(listData, f, mc.cores = nbins)})
str(res4)
# this was 10 times faster than using a for loop!





# new exploration

colnames(scenarios)
nrow(scenarios)
s5 <- scenarios[scenarios$N_ == 5, ]
nrow(s5)


v <- strsplit(s5[order(-abs(s5$Diff1)), ][1,1], split = ",")[[1]]

indX <- function(vec) {
  for (v in 1:length(vec)) {
    if (vec[v] == "X") return(v)
  }
}
indX(v)
ifelse(s5[order(-abs(s5$Diff1)), ][1, "Diff1"] < 0, "yang", "yin")
v[indX(v)] <- ifelse(s5[order(-abs(s5$Diff1)), ][1, "Diff1"] < 0, "yang", "yin")






s0 <- scenarios[scenarios$N_ == 0, ]
nrow(s0)
s0[1:4,]
