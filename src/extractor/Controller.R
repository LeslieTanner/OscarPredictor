# Control File. Calls all other function and saves final result.
# Before running set source and data directories:
dataDir <- "../../data/"
sourceDir <- "../../src/extractor/"

source(paste0(sourceDir,"ExtractMainPage.R"))
source(paste0(sourceDir,"ExtractMovieDetails3.R"))
source(paste0(sourceDir,"ExtractActorDetails.R"))


if(!file.exists(paste0(dataDir,"mainPageData.RData"))){
    mainPage <- extractMainPage()
    save(mainPage, file=paste0(dataDir,"mainPageData.RData"))
} else {
    load(file=paste0(dataDir,"mainPageData.RData"))
}

if(!file.exists(paste0(dataDir,"movieData.RData"))){
    movieDataFrame  <-  extractMovieDetails(mainPage, chunksize = 100, 
                                            startIndex = 1, endIndex = nrow(mainPage),
                                            dataDir = dataDir, pause = .5)
    save(movieDataFrame, file=paste0(dataDir,"movieData.RData"))
} else {
    load(file=paste0(dataDir,"movieData.RData"))
}

if(!file.exists(paste0(dataDir,"actorData.RData"))){
    actorData  <-  extractActorDetails(movieDetails = movieDataFrame, chunksize = 100, 
                                       startIndex = 1, 
                                       dataDir = dataDir, pause = .3)
    save(actorData, file=paste0(dataDir,"actorData.RData"))
} else {
    load(file=paste0(dataDir,"actorData.RData"))
}

