# Code to pull monthly exchange rates from www.fxtop.com


library(RCurl)
library(XML)

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

