# Code to pull cpi from www.usinflationcalculator.com (spot checked against bureau of labor statistics website which would have been used by no table available)


library(RCurl)
library(XML)

url <- getURL("http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/")
cpi <- readHTMLTable(url, stringsAsFactors = FALSE)
cpi <- cpi[[1]]
names(cpi) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "AvgAnnual", "Dec-Dec%Chg", "Avg-Avg%Chg")
cpi <- cpi[3:nrow(cpi),]
cpi$Jan <- as.numeric(cpi$Jan)
cpi$Feb <- as.numeric(cpi$Feb)
cpi$Mar <- as.numeric(cpi$Mar)
cpi$Apr <- as.numeric(cpi$Apr)
cpi$May <- as.numeric(cpi$May)
cpi$June <- as.numeric(cpi$June)
cpi$July <- as.numeric(cpi$July)
cpi$Aug <- as.numeric(cpi$Aug)
cpi$Sep <- as.numeric(cpi$Sep)
cpi$Oct <- as.numeric(cpi$Oct)
cpi$Nov <- as.numeric(cpi$Nov)
cpi$Dec <- as.numeric(cpi$Dc)
cpi$AvgAnnual <- as.numeric(cpi$AvgAnnual)
cpi$`Dec-Dec%Chg` <- as.numeric(cpi$`Dec-Dec%Chg`)
cpi$`Avg-Avg%Chg` <- as.numeric(cpi$`Avg-Avg%Chg`)

# calculate inflation relative to 2012
cpi$inflation <- cpi$AvgAnnual[99]/cpi$AvgAnnual

dataDir <- "/Users/michaellenart/Documents/Oscars/OscarPredictor/data/"
save(cpi, file = paste0(dataDir,"cpi.RData"))









