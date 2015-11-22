#########################################################################
#
# This file contains extractMainPage() function.
#
# This function reads and parses a list of all movies 1972-present on IMDB (url of the list below)
# 
# INPUT:
#  - URL: http://www.imdb.com/list/ls057823854/?start=1&view=compact&sort=release_date_us:desc&defaults=1&defaults=1&release_date=1970,2015
#
# OUTPUT:
#  - Data Frame "mainPageData" which has below columns:
#     - MovieID
#     - MovieURL
#     - MovieTitle
#     - ReleaseYear
#     - MovieRating
#     - MovieNumUserRatings
#
###########################################################################

#Install and load required libraries

# install.packages("rvest")
# install.packages("magrittr")
# install.packages("stringr")
# install.packages("plyr")

library(rvest)
library(magrittr)
library(stringr)
library(plyr)

############################################################################

#The movie list is split into multiple pages on IMDB, each page containing 250 movies
#The below function can process a single such page, as indicated by the startNumber

extractSinglePage <- function(startNumber){
  print(startNumber)
  url <- paste0("http://www.imdb.com/list/ls057823854/?start=", startNumber , 
                "&view=compact&sort=release_date_us:desc&defaults=1&defaults=1&release_date=1970,2015")
  
  source.page <- read_html(url)
  # MovieTitle
  MovieTitle <- source.page %>%
    html_nodes(".title") %>%
    html_text()
  MovieTitle <- MovieTitle[2:length(MovieTitle)]
    
  # MovieURL
  MovieURL <- str_c("http://www.imdb.com", (source.page %>% html_nodes(".title a:nth-child(1)") %>%  
    html_attr("href")))
  
  # MovieID
  MovieID <- str_replace_all(str_replace_all((source.page %>% html_nodes(".title a:nth-child(1)") %>% 
                                                html_attr("href")), "/title/", ""), "/", "")
  # MovieID <- append("MovieID", MovieID)
  
  # Combine all into Data Frame
  df <- data.frame(MovieTitle, MovieID, MovieURL)

  return(df)
  
}

############################################################################

extractMainPage <- function(){

  startNum <- ((0:38)*250)+1
  
  MP_List <- lapply(startNum, extractSinglePage)
  MP_List
  mainPageData <- ldply(MP_List, data.frame)
  return(mainPageData)
  
  #  I am keen to move this to a "data Pre processing" stage.
  # So that we can do preprocessing of all variables in one go. - Nikhil
  
  #mainPageData$ReleaseYear <- as.numeric(as.character(mainPageData$ReleaseYear))
  #MainPage$MovieRating <- as.numeric(as.character(MainPage$MovieRating))
  #MainPage$MovieNumUserRatings <- as.character(MainPage$MovieNumUserRatings)
  #MainPage$MovieNumUserRatings <- str_replace_all(MainPage$MovieNumUserRatings, ",", "")
  #MainPage$MovieNumUserRatings <- str_replace_all(MainPage$MovieNumUserRatings, "-", "")
  #MainPage$MovieNumUserRatings <- as.numeric(MainPage$MovieNumUserRatings)
  
  # Select only movies from years 2000 and higher
  #MainPage <- MainPage[MainPage$ReleaseYear > 1999, ]
  #barplot(table(mainPageData$ReleaseYear))
  
  # Write to CSV file:
  #write.csv(MainPage, file = "../../data/MainPageData.csv", row.names = FALSE)
  
  # Save R.Data
  #save(list = ls(all.names = TRUE), file="ExtractMainPage_11.19.2015.RData")
  
}
############################################################################
