# loading functions that return a list containing a type-frequency table and a symbol set.

loadMB <- function() {
  TF <- read.csv(file = "./data/myers-briggs-dataset-01.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  TF$D1 <- as.factor(TF$D1)
  TF$D2 <- as.factor(TF$D2)
  TF$D3 <- factor(TF$D3, labels = c("F", "T"))
  TF$D4 <- as.factor(TF$D4)
  TF$PPT <- as.double(TF$PPT - 0.26 / 16)
  TF$PPM <- as.double(TF$PPM)
  TF$PPF <- as.double(TF$PPF - 2.6 / 16)
  # Assemble results
  rownames(TF) <- TF$Type
  # Add group id column
  TF$Grp <- rep(NA, nrow(TF))
  TF[TF$D2 == "N" & TF$D4 == "J", ]$Grp <- 1
  TF[TF$D3 == "T" & TF$D4 == "P", ]$Grp <- 2
  TF[TF$D3 == "F" & TF$D4 == "P", ]$Grp <- 3
  TF[TF$D2 == "S" & TF$D4 == "J", ]$Grp <- 4
  TF$Grp <- as.factor(TF$Grp)
  symSet <- data.frame(c1 = c("X", "_", "I", "E"),
                       c2 = c("X", "_", "N", "S"),
                       c3 = c("X", "_", "F", "T"),
                       c4 = c("X", "_", "P", "J") )
  return(list(TF = TF, symSet = symSet))
}


loadCRX <- function() {
  Data <- read.table("./data/crx.data", header=F, sep = ",", na.strings = "?")
  names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation", "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore", "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved")
  Data$Gender <- as.factor(Data$Gender) 
  Data$Age <- as.numeric(Data$Age)
  Data$MonthlyExpenses <- as.integer(Data$MonthlyExpenses) 
  Data$MaritalStatus <- as.factor(Data$MaritalStatus) 
  Data$HomeStatus <- as.factor(Data$HomeStatus) 
  Data$Occupation <- as.factor(Data$Occupation) 
  Data$BankingInstitution <- as.factor(Data$BankingInstitution) 
  Data$YearsEmployed <- as.numeric(Data$YearsEmployed) 
  Data$NoPriorDefault <- as.factor(Data$NoPriorDefault) 
  Data$Employed <- as.factor(Data$Employed) 
  Data$CreditScore <- as.numeric(Data$CreditScore) 
  Data$DriversLicense <- as.factor(Data$DriversLicense)
  Data$AccountType <- as.factor(Data$AccountType)
  Data$MonthlyIncome <- as.integer(Data$MonthlyIncome)
  Data$AccountBalance <- as.numeric(Data$AccountBalance)
  Data$Approved <- as.factor(Data$Approved)
  
  # convert numeric columns to binned factors
  Data$Age <- n2bf(Data$Age, 3)
  Data$MonthlyExpenses <- n2bf(Data$MonthlyExpenses, 3, doubling = TRUE, asint = TRUE)
  Data$YearsEmployed <- n2bf(Data$YearsEmployed, 3, doubling = TRUE)
  Data$CreditScore <- n2bf(Data$CreditScore, 3, doubling = TRUE, asint = TRUE)
  Data$MonthlyIncome <- n2bf(Data$MonthlyIncome, 3, doubling = TRUE, asint = TRUE)
  Data$AccountBalance <- n2bf(Data$AccountBalance, 3, doubling = TRUE, asint = TRUE)
  
  # omit NAs for now but eventually have an NA category for them
  Data <- na.omit(Data)
  
  symSet <- getSymSetFromData(Data)

  # focus on relevant data
  dims <- c(1, 2, 3, 4, 5, 8) # columns of interest
  symSet <- symSet[,dims] # strip down to relevant columns
  symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs

  TF <- getTypeFreqs(Data, symSet, dims)
  
  return(list(TF = TF, symSet = symSet))
}


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
  
  levels(Data[,"Community.Area"]) <- DBLLETTERS[1:length(levels(Data[,"Community.Area"]))]
  
  symSet <- getSymSetFromData(Data)
  
  # focus on relevant data
  dims <- c(2, 5, 6) # columns of interest
  symSet <- symSet[,dims] # strip down to relevant columns
  symSet <- symSet[rowSums(is.na(symSet)) != ncol(symSet), ] # strip rows with all NAs

  # # each yearly dataset is a 'population sample'
  # Data.years <- split(Data, Data$Year)
  # # choose which year to analyse
  # Data <- Data.years[["2015"]]
  
  TF <- getTypeFreqs(Data, symSet, dims)
  return(list(TF = TF, symSet = symSet))
}

getNGrams <- function(symSet, n, asFactor = TRUE) {
  nDims <- ncol(symSet)
  out <- data.frame(matrix(NA, nrow=n, ncol=nDims))
  lines <- rep(0, nDims)
  for (r in 1:n) {
    for (l in 1:nDims) {
      lines[l] <- as.integer(sample(seq(0,sum(!is.na(symSet[,l])) - 3), 1, replace = TRUE))
    }
    out[r,] <- lines
  }
  
  colnames(out) <- paste(rep("D", nDims), seq(nDims), sep = "")
  if (asFactor) {
    for (l in seq(nDims)) {
      out[,l] <- factor(out[,l])
      levels(out[,l]) <- symSet[3:sum(!is.na(symSet[,l])), l]
    }
  }
  return(out)
}

getNGramTF <- function(symSet, n, asFactor = TRUE, withRowNames = TRUE) {
  # randomly generate n n-grams
  seed <- as.numeric(Sys.time())
  print(paste("Seed = ", seed, sep = ""))
  set.seed(seed)
  
  Data <- getNGrams(symSet, n, asFactor)
  
  TF <- getTypeFreqs(Data, symSet, seq(ncol(symSet)))
  if (withRowNames) {
    masks <- c()
    for (r in seq(nrow(TF))) {
      masks <- append(masks, paste(row2CharVec(TF[r,seq(ncol(TF) - 1)]), collapse = ","))
    }
    rownames(TF) <- masks
  }
  return(TF)
}

loadIChing <- function(n = 10000) {
  yy <- c("yin", "yang")
  spec <- list(yy, yy, yy, yy, yy, yy)
  symSet <- getSymSetFromSpec(spec)
  # this is where the hexagrams are randomly generated
  TF <- getNGramTF(symSet, n, withRowNames = FALSE)
  labels = c("1 Force", "2 Field", "3 Sprouting", "4 Enveloping", "5 Attending", "6 Dispute", "7 Legions", "8 Grouping", "9 Small Accumulates", "10 Treading", "11 Pervading", "12 Obstruction", "13 Harmonising People", "14 Great Being", "15 Humbling", "16 Providing For", "17 Following", "18 Corruption", "19 Nearing", "20 Viewing", "21 Biting Through", "22 Adorning", "23 Stripping", "24 Returning", "25 Disentangling", "26 Great Accumulates", "27 Jaws", "28 Great Traverses", "29 Pit", "30 Radiance", "31 Conjoining", "32 Persevering", "33 Retiring", "34 Great Invigorating", "35 Flourishing", "36 Brightness Hiding", "37 Dwelling People", "38 Divergence", "39 Limping", "40 Deliverance", "41 Diminishing", "42 Augmenting", "43 Deciding", "44 Coupling", "45 Great Works", "46 Ascending", "47 Confining", "48 The Well", "49 Skinning", "50 Vessel", "51 Shake", "52 Bound", "53 Gradual Advance", "54 Marrying the Maiden", "55 Abounding", "56 Sojourning", "57 Lady of Fates", "58 Open Expression", "59 Dispersing", "60 Articulating", "61 Centring", "62 Small Traverses", "63 Already Crossing", "64 Not Yet Crossing")
  b2IC = c(2, 24, 7, 19, 15, 36, 46, 11, 16, 51, 40, 54, 62, 55, 32, 34, 8, 3, 29, 60, 39, 63, 48, 5, 45, 17, 47, 58, 31, 49, 28, 43, 23, 27, 4, 41, 52, 22, 18, 26, 35, 21, 64, 38, 56, 30, 50, 14, 20, 42, 59, 61, 53, 37, 57, 9, 12, 25, 6, 10, 33, 13, 44, 1)
  rownames(TF) <- labels[b2IC]
  return(list(TF = TF, symSet = symSet))
}

# yellow vests opinion poll data
loadYV <- function() {
  TF <- read.csv(file = "./data/yellow-vests-opinion-poll-results.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  TF$D1 <- factor(TF$D1, levels = c("s-2","s-1","s0","s1","s2"), ordered = TRUE)
  TF$PP1 <- as.double(TF$PP1)
  TF$PP2 <- as.double(TF$PP2)
  TF$PP3 <- as.double(TF$PP3)
  TF$PP4 <- as.double(TF$PP4)
  TF$PP5 <- as.double(TF$PP5)
  # Assemble results
  rownames(TF) <- TF$Type
  symSet <- data.frame(c1 = c("X", "_", levels(TF$D1)) )
  return(list(TF = TF, symSet = symSet))
}




