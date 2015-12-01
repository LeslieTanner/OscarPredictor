# This function reads and parses the list of years from the 
# filmography for each actor in a list of actors(URLs) in IMDB

#INPUT:
#  - list of URL's for actors appearing in the top 3 list of actors from movies extracted 
#    by ExtractMainPage.
#
#OUTPUT:
#  - .Rdata file containing a list, actorDetails, keyed by actor URL, with each entry a
#    list containing:
#     - ActorID (actor name)
#     - AllYears (int vector of all years actor appeared in an actor role)
#     - FilmYears (years actor appeared in an acting role in films (all years minus TV years))
#     - TVYears (years actor appeared in an acting role in TV shows)
#     - TVMultiYears (years actor appeared in an acting role for more than one year of a TV show)
# The release years had to be a list because the lengths vary for each actor.

library(rvest)
library(magrittr)
library(stringr)

extractActorFilmography <- function(url, pause){
    print(url)
    source.page <- read_html(url)
    
    # actor or actress?
    gender <- source.page %>% 
        html_nodes('#name-job-categories .itemprop') %>%
        html_text()
    gender <- ifelse(sum(str_detect(gender,"Actress")) > 0,"actress","actor")
    
    # Actor name
    ActorID <- source.page %>% 
        html_node(".header .itemprop") %>%
        html_text()
    
    # Years for all films/TV shows where ActorID was an actor.  Take only the first year
    # listed when the form is 1111-2222
    FilmographyYears <- source.page %>%
        html_nodes(paste0("#filmo-head-",gender,"+ .filmo-category-section .year_column")) %>%
        html_text() %>% str_trim() %>% str_extract("[0-9]{4}") %>% as.integer()
    
    # All Filmography info for the films/TV shows
    Filmographies <- source.page %>%
        html_nodes(paste0("#filmo-head-",gender,"+ .filmo-category-section .year_column")) %>% 
        html_text() %>% str_trim()
    
    # Which indices are for TV shows?
    IndicesTV <- str_detect(Filmographies, "\\(TV") %>% which()
    
    # Which TV show indices were for multiple years (main role)?
    IndicesTVMultiYears <- str_detect(Filmographies, "^[0-9]{4}-") %>% which()
    
    # Which indices were likely for films? (not TV)
    IndicesFilm <- setdiff(1:length(Filmographies), IndicesTV)
    
    
    # All years, sorted
    AllYears <- sort(FilmographyYears)
    
    # Film years, sorted
    FilmYears <- sort(FilmographyYears[IndicesFilm])
    
    # TV years, sorted
    TVYears <- sort(FilmographyYears[IndicesTV])
    
    # TV main role years, sorted
    TVMultiYears <- sort(FilmographyYears[IndicesTVMultiYears])
    
    # Combine all into *List* (because of differing lengths)
    list <- list(StarName = ActorID, AllYears = AllYears, FilmYears = FilmYears,
                 TVYears = TVYears, TVMultiYears = TVMultiYears)
    
    # mean wait time of `pause` secs
    Sys.sleep(rgamma(1, 2*pause, 2))
    
    return(list)
}

extractActorDetails <- function(movieDetails, chunksize = 100, 
                                startIndex = 1, endIndex, pause = 1,
                                dataDir = dataDir){
    # Get the unique actor URLs
    StarURLs <- unique(c(as.character(movieDetails$Star1URL),
                         as.character(movieDetails$Star3URL),
                         as.character(movieDetails$Star3URL)))
    StarURLs <- StarURLs[!is.na(StarURLs)]
    names(StarURLs) <- StarURLs
    if(missing(endIndex)){
        endIndex = length(StarURLs)
    }
    
    numberOfChunks <- ceiling((endIndex-startIndex+1)/chunksize)
    actorDetails <- list()
    length(actorDetails) <- numberOfChunks
    
    print(paste0("Extracting actors ",startIndex," to ",endIndex))
    
    for(i in 1:numberOfChunks){
        minIndex = (i - 1)*chunksize + 1
        maxIndex = min(minIndex + chunksize - 1, endIndex, length(StarURLs))
        filename = paste0(dataDir,"actorDetails",minIndex,"to",maxIndex,".RData")
        
        print(filename)
        
        if(!file.exists(filename)){
            actorData <- lapply(StarURLs[minIndex:maxIndex],extractActorFilmography, pause = pause)
            save(actorData, file = filename)
            actorDetails[[i]] <- actorData
        } else {
            load(file = filename)
            actorDetails[[i]] <- actorData
        }
        print("");print("")
    }
    actorData <- Reduce(c, actorDetails)
    
    return(actorData)
}
