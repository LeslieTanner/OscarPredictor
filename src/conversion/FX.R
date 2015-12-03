# Code to pull monthly exchange rates from www.fxtop.com


library(RCurl)
library(XML)
library(stringr)

dataDir <- "/Users/michaellenart/Documents/Oscars/OscarPredictor/data/"

# Function to obtain monthly exchange rates from 1/1980 to 12/2012
get_rates <- function(url, currency){
  url2 <- getURL(url)
  curr <- readHTMLTable(url2)
  curr <- curr[[1]]
  curr <- curr[70:465,1:2]
  rownames(curr) <- 1:nrow(curr)
  names(curr) <- c("Month", currency)
  
  curr$index <- as.numeric(row.names(curr))
  
  rownames(curr) <- 1:nrow(curr)
  return(curr)
}

#function to fix the index of the dataframe with rates
fix_index <- function(rates, index){
  names <- setdiff(names(rates), index)
  rates <- rates[,names]
  return(rates)
}

# obtain EUR
rates <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=EUR&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "EUR")

#obtain GBP
GBP <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=GBP&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "GBP")
rates <- merge(rates, GBP, by="Month")
rates <- fix_index(rates, "index.y")

#obtain SEK
SEK <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=SEK&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "SEK")
rates <- merge(rates, SEK, by="Month")
rates <- fix_index(rates, "index.y")

#obtain ESP
ESP <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=ESP&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "ESP")
rates <- merge(rates, ESP, by="Month")
rates <- fix_index(rates, "index.y")

#obtain DEM
DEM <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=DEM&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "DEM")
rates <- merge(rates, DEM, by="Month")
rates <- fix_index(rates, "index.y")

#obtain ITL
ITL <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=ITL&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "ITL")
rates <- merge(rates, ITL, by="Month")
rates <- fix_index(rates, "index.y")

#obtain JPY
JPY <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=JPY&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "JPY")
rates <- merge(rates, JPY, by="Month")
rates <- fix_index(rates, "index.y")

#obtain SGD
SGD <- get_rates("http://fxtop.com/en/historical-exchange-rates.php?A=1&C1=USD&C2=SGD&MA=1&DD1=01&MM1=01&YYYY1=1980&B=1&P=&I=1&DD2=31&MM2=12&YYYY2=2012&btnOK=Go%21", "SGD")
rates <- merge(rates, SGD, by="Month")
rates <- fix_index(rates, "index.y")

#clean up dataframe by removing index collumn and reindexing rows
rates <- rates[order(rates$index.x, decreasing = TRUE),]
rates <- fix_index(rates, "index.x")
rownames(rates) <- 1:nrow(rates)

# create column with quarter name by splitting mont/year in 2 columns then delete temporary columns
rates$Month <- as.character(rates$Month)
rates$month <- lapply(rates$Month, function(x) unlist(strsplit(x, "/"))[[1]])
rates$year <- ""
rates$year <- lapply(rates$Month, function(x) unlist(strsplit(x, "/"))[[2]])
rates$qtr <- cut(as.numeric(rates$month), breaks=c(0, 3, 6, 9, 12), labels = c("1Q", "2Q", "3Q", "4Q"))
rates$Qtr <- paste(rates$qtr, as.character(rates$year))
names <- setdiff(names(rates),c("month", "year", "qtr"))
rates <- rates[,names]

# rates need to be reformatted inorder to calculate the average
rates$EUR <- as.numeric(as.character(rates$EUR))
rates$GBP <- as.numeric(as.character(rates$GBP))
rates$SEK <- as.numeric(as.character(rates$SEK))
rates$ESP <- as.numeric(as.character(rates$ESP))
rates$DEM <- as.numeric(as.character(rates$DEM))
rates$ITL <- as.numeric(as.character(rates$ITL))
rates$JPY <- as.numeric(as.character(rates$JPY))
rates$SGD <- as.numeric(as.character(rates$SGD))

# # save monthly rates to an RData file
save(rates, file = paste0(dataDir,"FX_rates.RData"))

# calculate average rates by quarter
EUR_Avg <- aggregate(rates$EUR,by = list(rates$Qtr), mean)
names(EUR_Avg) <- c("Qtr", "EUR")
GBP_Avg <- aggregate(rates$GBP,by = list(rates$Qtr), mean)
names(GBP_Avg) <- c("Qtr", "GBP")
SEK_Avg <- aggregate(rates$SEK,by = list(rates$Qtr), mean)
names(SEK_Avg) <- c("Qtr", "SEK")
ESP_Avg <- aggregate(rates$ESP,by = list(rates$Qtr), mean)
names(ESP_Avg) <- c("Qtr", "ESP")
DEM_Avg <- aggregate(rates$DEM,by = list(rates$Qtr), mean)
names(DEM_Avg) <- c("Qtr", "DEM")
ITL_Avg <- aggregate(rates$ITL,by = list(rates$Qtr), mean)
names(ITL_Avg) <- c("Qtr", "ITL")
JPY_Avg <- aggregate(rates$JPY,by = list(rates$Qtr), mean)
names(JPY_Avg) <- c("Qtr", "JPY")
SGD_Avg <- aggregate(rates$SGD,by = list(rates$Qtr), mean)
names(SGD_Avg) <- c("Qtr", "SGD")

# merger quarterly rates into a single dataframe
qrtly_rates <- merge(EUR_Avg, GBP_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, SEK_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, ESP_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, DEM_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, ITL_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, JPY_Avg, by="Qtr")
qrtly_rates <- merge(qrtly_rates, SGD_Avg, by="Qtr")

# save quarterly rates to an RData file
save(qrtly_rates, file = paste0(dataDir,"FX_qrtly.RData"))
