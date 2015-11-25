# Control File. Calls all other function and saves final result.
# Before running set working directory to location of this file(Controller.R).
source("ExtractMainPage.R")
source("ExtractMovieDetails.R")
mainPage <- extractMainPage()
save(mainPage,file="../../data/mainPageData.RData")
load(file="../../data/mainPageData.RData")
movieDataFrame  <-  extractMovieDetails(mainPage)
save(movieDataFrame,file="../../data/movieData.RData")
