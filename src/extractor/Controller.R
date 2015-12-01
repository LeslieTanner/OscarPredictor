# Control File. Calls all other function and saves final result.
# Before running set working directory to location of this file(Controller.R).
dataDir <- "~/Git/OscarPredictor/data/"
sourceDir <- "~/Git/OscarPredictor/src/extractor/"

source(paste0(sourceDir,"ExtractMainPage.R"))
source(paste0(sourceDir,"ExtractMovieDetails2.R"))
source(paste0(sourceDir,"ExtractActorDetails.R"))


if(!file.exists(paste0(dataDir,"mainPageData.RData"))){
    mainPage <- extractMainPage()
    save(mainPage, file=paste0(dataDir,"mainPageData.RData"))
} else {
    load(file=paste0(dataDir,"mainPageData.RData"))
}

if(!file.exists(paste0(dataDir,"movieData.RData"))){
    movieDataFrame  <-  extractMovieDetails(mainPage, chunksize = 100, 
                                            startIndex = 1, endIndex = 10,#nrow(mainPage), 
                                            dataDir = dataDir)
    save(movieDataFrame, file=paste0(dataDir,"movieData.RData"))
} else {
    load(file=paste0(dataDir,"movieData.RData"))
}

if(!file.exists(paste0(dataDir,"actorData.RData"))){
    actorData  <-  extractActorDetails(movieDetails = movieDataFrame, chunksize = 100, 
                                       startIndex = 1, endIndex = 10, dataDir = dataDir)
    save(actorData, file=paste0(dataDir,"actorData.RData"))
}
