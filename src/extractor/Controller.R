# Control File. Calls all other function and saves final result.
# Before running set working directory to location of this file(Controller.R).
source("ExtractMainPage.R")
source("ExtractMovieDetails.R")
mainPage <- extractMainPage()
movieDataFrame  <-  extractMovieDetails(mainPage)
save(movieDataFrame,file="../../data/movieData.R")
