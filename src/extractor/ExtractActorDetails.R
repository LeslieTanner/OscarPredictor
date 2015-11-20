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

dataDir <- '~/Git/OscarPredictor/data/'

# imdb_name <- "www.imdb.com"

#url = 'http://www.imdb.com/name/nm0001774/'
extractActorFilmography <- function(url){
    source.page <- read_html(url)
    
    # Actor name
    ActorID <- source.page %>% 
        html_node(".header .itemprop") %>%
        html_text()
    
    # Years for all films/TV shows where ActorID was an actor.  Take only the first year
    # listed when the form is 1111-2222
    FilmographyYears <- source.page %>%
        html_nodes("#filmo-head-actor+ .filmo-category-section .year_column") %>%
        html_text() %>% str_trim() %>% str_extract("[0-9]{4}") %>% as.integer()
    
    # All Filmography info for the films/TV shows
    Filmographies <- source.page %>%
        html_nodes("#filmo-head-actor+ .filmo-category-section .filmo-row") %>% 
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
    list <- list(ActorID = ActorID, AllYears = AllYears, FilmYears = FilmYears,
                 TVYears = TVYears, TVMultiYears = TVMultiYears)
    
    # mean wait time of 4/2 = 2s
    Sys.sleep(rgamma(1, 4, 2))
    return(list)
}

# Read the movie data for the URL's for each actor
load("movieDetails.Rdata")
#movieDetails <- read.csv(paste0(dataDir,"movieDetails.csv"), stringsAsFactors = FALSE)

# Get the unique actor URLs
StarURLs <- unique(c(movieDetails$Star1URL,movieDetails$Star3URL,movieDetails$Star3URL))
StarURLs <- StarURLs[!is.na(StarURLs)]
names(StarURLs) <- StarURLs

# Loop over them, getting the acting role year data for each
starDetails <- lapply(StarURLs, extractActorFilmography)

# Save R.Data
save(list = ls(all.names = TRUE), file = 
         paste0("ExtractActorDetails_", format(Sys.time(), '%m.%d.%Y'),".RData"))

