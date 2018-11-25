#### Utils ####
library(dplyr)
library(ggplot2)
library(ggdendro)
library(cluster)
library(gtools)

row2CharVec <- function(df, r = 1) {
  out <- rep("", ncol(df))
  for (c in 1:ncol(df)) {
    out[c] <- as.character(df[r, c])
  }
  return(out)
}

# create a large set of double lettered labels
df <- expand.grid(LETTERS, LETTERS)
DBLLETTERS <- sprintf("%s%s", df$Var2, df$Var1)

# converts a numeric vector to a binned factor with a chosen number of bins
n2bf <- function(vec, nbins, doubling = FALSE, asint = FALSE) {
  if (doubling) template <- c(0, 1/2^seq(nbins - 1, 0, -1))
  else template <- seq(0, 1, length.out = nbins + 1)
  s <- summary(vec)
  if (asint) {
    smin <- as.integer(floor(s["Min."]))
    smax <- as.integer(floor(s["Max."]))
  }
  else {
    smin <- s["Min."]
    smax <- s["Max."]
  }
  breaks <- c(smin - 1)
  labels <- c(as.character(smin))
  range <- smax - smin
  for (b in 1:(nbins - 1)){
    if (asint) {
      brk <- round(smin + range * template[b+1])
      prevBrk <- round(smin + range * template[b])
      labels <- c(labels, sprintf("%d-%d", prevBrk, brk))
    }
    else {
      brk <- smin + range * template[b+1]
      prevBrk <- smin + range * template[b]
      labels <- c(labels, sprintf("%0.2f-%0.2f", prevBrk, brk))
    }
    breaks <- c(breaks, brk)
  }
  breaks <- c(breaks, smax)
  if (asint) {
    prevBrk <- round(smin + range * template[nbins])
    labels <- c(labels, sprintf("%d-%d", prevBrk, smax))
  }
  else {
    prevBrk <- smin + range * template[nbins]
    labels <- c(labels, sprintf("%0.2f-%0.2f", prevBrk, smax))
  }
  return(cut(vec, breaks = breaks, labels = labels[2:length(labels)]))
}



# plot a sorted data frame
# first sort the values, then plot them to see how they vary over the spectrum
sortedPlot <- function(df, sortby, lblsize = NULL, datatype = "", ylabel = "", ptsize = 1, suffix = "") {
  df <- df[order(eval(parse(text = paste("df$", sortby, sep = "")))), ]
  rank <- 1:nrow(df)
  plt <- ggplot(df, aes(x = rank, y = eval(parse(text = sortby)))) +
    geom_point(size = ptsize) +
    scale_x_continuous(breaks = rank, labels = rownames(df)) +
    theme(axis.text.x = element_text(face="bold", angle=90, size = lblsize)) +
    ylab(ylabel) +
    xlab(sprintf("%s sorted by ascending %s values", datatype, sortby)) +
    ggtitle(sprintf("Sorted Plot: %s", suffix), subtitle = sprintf("%s for %s sorted by ascending %s values", sortby, datatype, sortby))
  plt
}

numX <- function(str) { # str is vector of characters
  count <- 0
  for (i in 1:length(str)) {
    if (str[i] == "X") count <- count + 1
  }
  return(count)
}

# return the row number for a given symbol within a given dimension
getRowOfParam <- function(s, colNum, symSet) {
  for (r in 3:sum(!is.na(symSet[,colNum]))) {
    if (symSet[r,colNum] == sprintf("%s", s)) return(r)
  }
  return(0)
}


getMasks <- function(symSet, masks = c(), str = rep("", ncol(symSet)), iter = 1, constraints = rep("", ncol(symSet))) {
  if (constraints[iter] != "") {
    r <- c(1)
    syms <- c(constraints[iter])
  }
  else {
    r <- 1:sum(!is.na(symSet[,iter]))
    syms <- symSet[, iter]
  }
  for (s in r) {
    str[iter] <- sprintf("%s", syms[s])
    if (iter < ncol(symSet)) masks <- getMasks(symSet, masks, str, iter + 1, constraints = constraints)
    else if (numX(str) == 1) masks <- append(masks, paste(str, sep = "", collapse = ","))
  }
  return(masks)
}

colFromList <- function(lst, cnum) {
  out <- c()
  for (i in 1:length(lst)) {
    out <- c(out, lst[[i]][cnum])
  }
  return(out)
}

num_ <- function(strs) { # strs is a vector of strings
  res <- c()
  for (str in strs) {
    str <- strsplit(str, "")[[1]]
    count <- 0
    for (i in 1:length(str)) {
      if (str[i] == "_") count <- count + 1
    }
    res <- append(res, count)
  }
  return(res)
}

# function, given a mask compute some statistics for the associated group of types
getScenarios <- function(df, symSet = NULL, masks = NULL, cname = "PP", nSkip = 1) {
  if (is.null(masks)) masks <- getMasks(symSet)
  groupAPs <- c()
  ratios <- list()
  pps <- list()
  diffs <- list()
  params <- list()
  origdf <- df
  maxSyms <- nrow(symSet) - 2
  maxp <- ncol(symSet)
  for (mask in masks) {
    df <- origdf
    pp <- 1:maxp # possible positions
    xp <- 0 # X position
    vec <- strsplit(mask, split = ",")[[1]]
    for (p in 1:maxp) {
      if (p > length(params)) params[[p]] <- c(vec[p])
      else params[[p]] <- append(params[[p]], vec[p])
      if (vec[p] == "X") {
        pp <- pp[pp!=p]
        xp <- p
      }
      else if (vec[p] == "_") {
        pp <- pp[pp!=p]
      }
    }
    if (length(pp) > 0) {
      for (i in 1:length(pp)) {
        tmp <- split(df, df[, nSkip + pp[i]])
        df <- eval(parse(text = paste("tmp$", vec[pp[i]], sep = "")))
      }
    }
    tmp <- split(df, df[, nSkip + xp])
    subdfs <- list()
    nsyms <- sum(!is.na(symSet[,xp])) - 2
    ps <- rep(NA, nsyms)
    for (i in 1:nsyms) {
      subdfs[[i]] <- eval(parse(text = paste("tmp$", as.character(symSet[2 + i, xp]), sep = "")))
      ps[i] <- sum(eval(parse(text = paste("subdfs[[i]]$", cname, sep = ""))))
    }
    sumps <- sum(ps)
    pp <- rep(NA, nsyms)
    ratio <- rep(NA, nsyms)
    diff <- rep(NA, nsyms)
    for (i in 1:nsyms) {
      pp[i] <- ps[i] / sumps * 100
      ratio[i] <- ps[i] / (sumps - ps[i]) # ratio between each and the rest
      diff[i] <- ps[i] - (sumps - ps[i]) # difference between each and the rest
    }
    groupAPs <- append(groupAPs, sumps)
    ratios <- append(ratios, list(ratio))
    pps <- append(pps, list(pp))
    diffs <- append(diffs, list(diff))
  }
  out <- data.frame(row.names = masks, 
                    Mask = masks,
                    N_ = num_(masks))
  out$Mask <- as.character(out$Mask)
  for (i in 1:maxp) {
    eval(parse(text = paste("out$D", i, " <- params[[i]]", sep = "")))
  }
  out$GroupAP = groupAPs
  for (i in 1:maxSyms) {
    eval(parse(text = paste("out$R", i, " <- colFromList(ratios, i)", sep = "")))
  }
  for (i in 1:maxSyms) {
    eval(parse(text = paste("out$PP", i, " <- colFromList(pps, i)", sep = "")))
  }
  for (i in 1:maxSyms) {
    eval(parse(text = paste("out$Diff", i, " <- colFromList(diffs, i)", sep = "")))
  }
  for (i in 1:maxSyms) {
    eval(parse(text = paste("out$DP", i, " <- out$GroupAP / 100 * out$Diff", i, sep = "")))
  }
  for (i in 1:maxSyms) {
    eval(parse(text = paste("out$TP", i, " <- out$Diff", i, " / out$GroupAP * 100", sep = "")))
  }
  return(out)
}


# each path is defined by a set of four choices, e.g. c("1,A", "2,B", "3,A", "2,A")
# traverse this sequence of choices and print details of each step
getPath <- function(mbs, choices, symSet, undef = -1, chosen = NULL, withTotals = TRUE, origmbs = mbs) {
  if (is.null(chosen)) {
    chosen <- rep("",ncol(symSet))
    undef = length(chosen) - 1
  }
  if (length(choices) > 0) {
    choice = choices[1]
    if (length(choices) > 1) choices <- choices[2:length(choices)]
    else choices <- c()
    cs <- strsplit(choice, split = ",")[[1]]
    c <- as.integer(cs[1])
    r <- getRowOfParam(as.character(cs[2]), c, symSet)
    if (undef < 0) {
      undef <- 0
      chosen[c] <- "X"
      mbs <- origmbs[getMasks(symSet, constraints = chosen),]
    }
    chosen[c] <- as.character(cs[2])
    mbs$Choice <- as.character(cs[2])
    mbs$ChDiff <- eval(parse(text = paste("mbs$Diff", r - 2, sep = "")))
    mbs$ChDP <- eval(parse(text = paste("mbs$DP", r - 2, sep = "")))
    mbs$ChTP <- eval(parse(text = paste("mbs$TP", r - 2, sep = "")))
    df1 <- mbs[eval(parse(text = paste("mbs$D", c, sep = ""))) == "X" & mbs$N_ == undef, ]
    df1 <- select(df1, GroupAP, Choice, ChDiff, ChDP, ChTP)
    df2 <- getPath(mbs[eval(parse(text = paste("mbs$D", c, sep = ""))) == symSet[r,c], ], choices, symSet, undef - 1, chosen, FALSE, origmbs = origmbs)
    if (!is.null(df2)) df2 <- select(df2, GroupAP, Choice, ChDiff, ChDP, ChTP)
    if (withTotals) {
      df12 <- rbind(df1, df2)
      df3 <- data.frame(GroupAP = c("","",""), Choice = c("","",""), ChDiff = c("","",""), ChDP = c("","",""), ChTP = c("","",""))
      rownames(df3) <- c("-------" ,"Totals", "Averages")
      n <- nrow(df12)
      df3$GroupAP<- c("", sum(df12$GroupAP), sum(df12$GroupAP) / n)
      df3$ChDiff<- c("", sum(df12$ChDiff), sum(df12$ChDiff) / n)
      df3$ChDP <- c("", sum(df12$ChDP), sum(df12$ChDP) / n)
      df3$ChTP <- c("", sum(df12$ChTP), sum(df12$ChTP) / n)
      return(rbind(df12, df3))
    }
    else {
      return(rbind(df1, df2))
    }
  }
}


getTypes <- function(symSet) {
  entries <- list()
  ncols <- ncol(symSet)
  for (c in 1:ncols) {
    syms <- as.character(as.vector(symSet[3:sum(!is.na(symSet[,c])), c]))
    # for (s in 1:length(syms)) {
    #   syms[s] <- paste(as.character(c), syms[s], sep = ",")
    # }
    entries <- append(entries, list(syms))
  }
  
  return(expand.grid(entries, stringsAsFactors = FALSE))
}


getPerms <- function(symSet) {
  entries <- list()
  ncols <- ncol(symSet)
  for (c in 1:ncols) {
    syms <- as.character(as.vector(symSet[3:sum(!is.na(symSet[,c])), c]))
    for (s in 1:length(syms)) {
      syms[s] <- paste(as.character(c), syms[s], sep = ",")
    }
    entries <- append(entries, list(syms))
  }
  
  types <- expand.grid(entries, stringsAsFactors = FALSE)
  
  perms <- matrix(ncol = ncols)
  for (i in 1:nrow(types)) {
    p1 <- permutations(ncols, ncols, as.character(as.vector(types[i,])), set = TRUE)
    perms <- rbind(perms, p1)
  }
  perms <- data.frame(na.omit(perms), stringsAsFactors = FALSE)
  for (r in 1:nrow(perms)) {
    dims <- as.character(as.vector(perms[r,]))
    for (i in 1:length(dims)) dims[i] <- substring(dims[i], nchar(dims[i]), nchar(dims[i]))
  }
  out <- data.frame(Ch1 = perms[,1])
  for (c in 2:ncols) {
    eval(parse(text = paste("out$Ch", c, " <- perms[, c]", sep = "")))
  }
  return(out)
}
  
getAllPaths <- function(mbs, symSet, avgs = TRUE) {
  out <- getPerms(symSet)
  TGroupAP <- c()
  TChDiff <- c()
  TChDP <- c()
  TChTP <- c()
  for (i in 1:nrow(out)) {
    pathDf <- getPath(mbs, row2CharVec(out[i, ]), symSet, withTotals = FALSE)
    n <- nrow(pathDf)
    TGroupAP <- append(TGroupAP, sum(pathDf$GroupAP) / ifelse(avgs, n, 1))
    TChDiff <- append(TChDiff, sum(pathDf$ChDiff) / ifelse(avgs, n, 1))
    TChDP <- append(TChDP, sum(pathDf$ChDP) / ifelse(avgs, n, 1))
    TChTP <- append(TChTP, sum(pathDf$ChTP) / ifelse(avgs, n, 1))
  }
  out <- cbind(out, TGroupAP, TChDiff, TChDP, TChTP)
  for (c in 1:ncol(symSet)) {
    out[,c] <- as.factor(out[,c])
  }
  return(out)
}

# build symbolset from data frame
getSymbolSet <- function(Data) {
  prefix <- c("X", "_")
  maxLvls <- 0
  for (c in 1:ncol(Data)) {
    lvlLen <- length(levels(Data[,c]))
    if (lvlLen > maxLvls) maxLvls <- lvlLen
  }
  symSet <- data.frame(c1 = rep(NA, maxLvls + 2))
  for (c in 1:ncol(Data)) {
    lvls <- gsub("[^A-Za-z0-9]", "_", levels(Data[,c]))
    syms <- c(prefix, lvls)
    if (length(syms) < maxLvls + 2) {
      syms <- c(syms, rep(NA, maxLvls + 2 - length(syms)))
    }
    eval(parse(text = paste("symSet$c", c, " <- syms", sep = "")))
  }
  colnames(symSet) <- colnames(Data)
  return(symSet)
}


# build type frequency table
getTypeFreqs <- function(Data, dims, symSet) {
  types <- getTypes(symSet)
  colnames(types) <- colnames(Data)[dims]
  # collect count stats for types in the population
  types$Count <- rep(NA, nrow(types))
  for (t in 1:nrow(types)) {
    str <- "df <- filter(Data,"
    for (c in 1:length(dims)) {
      str <- paste(str,"Data[,", dims[c], "] == types[", t, ",", c, "]", ifelse(c == length(dims), " )", " & "), sep = "")
    }
    eval(parse(text = str))
    types$Count[t] <- nrow(df)
  }
  # calculate the percentage prevalence stats for each type
  types <- types %>%
    mutate(PP = Count / sum(Count) * 100) %>%
    select(-Count)
  # change dims to factors
  for (c in 1:(ncol(types) - 1)) {
    types[,c] <- factor(types[,c])
  }
  return(types)
}

