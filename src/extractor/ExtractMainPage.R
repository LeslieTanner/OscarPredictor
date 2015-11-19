#This function reads and parses the first main page in IMDB
#INPUT:
#  - URL: http://www.imdb.com/list/ls057823854/?start=1&view=compact&sort=release_date_us:desc&defaults=1&defaults=1&release_date=1970,2015
#
#OUTPUT:
#  - CSV file "mainPageData.csv" which has below columns:
#     - MovieID
#     - MovieURL
#     - MovieTitle
#     - ReleaseYear
#     - MovieRating
#     - MovieNumUserRatings

library(rvest)
library(magrittr)
library(stringr)

# imdb_name <- "www.imdb.com"


extractMainPage <- function(url){
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
  
  # ReleaseYear
  ReleaseYear <- source.page %>%
    html_nodes(".year") %>%
    html_text()
  ReleaseYear <- ReleaseYear[2:(length(ReleaseYear) - 1)]
  
  # MovieRating
  MovieRating <- source.page %>%
    html_nodes(".user_rating") %>%
    html_text()
  MovieRating <- MovieRating[2:length(MovieRating)]
  
  # MovieNumUserRatings
  MovieNumUserRatings <- source.page %>%
    html_nodes(".num_votes") %>%
    html_text()
  MovieNumUserRatings <- MovieNumUserRatings[2:length(MovieNumUserRatings)]

  # Combine all into Data Frame
  df <- data.frame(MovieTitle, MovieID, MovieURL, ReleaseYear, MovieRating, MovieNumUserRatings)

  return(df)
  
}

# MainPage1 <- extractMainPage(url1)
# MainPage2 <- extractMainPage(url2)

MP <- c()
item <- c(1, 251, 501, 751, 1001, 1251, 1501, 1751, 2001, 2251, 
          2501, 2751, 3001, 3251, 3501, 3751, 4001, 4251, 4501)

for (i in item) {
  # Change URL to scrape multiple pages
  url <- paste0("http://www.imdb.com/list/ls057823854/?start=", i , 
                "&view=compact&sort=release_date_us:desc&defaults=1&defaults=1&release_date=1970,2015")
  
  # Combine scraped data frames from each page into one data frame
  MP <- rbind(MP, extractMainPage(url))

}

MainPage <- MP


MainPage$ReleaseYear <- as.numeric(as.character(MainPage$ReleaseYear))
MainPage$MovieRating <- as.numeric(as.character(MainPage$MovieRating))
MainPage$MovieNumUserRatings <- as.character(MainPage$MovieNumUserRatings)
MainPage$MovieNumUserRatings <- str_replace_all(MainPage$MovieNumUserRatings, ",", "")
MainPage$MovieNumUserRatings <- str_replace_all(MainPage$MovieNumUserRatings, "-", "")
MainPage$MovieNumUserRatings <- as.numeric(MainPage$MovieNumUserRatings)

# Select only movies from years 2000 and higher
MainPage <- MainPage[MainPage$ReleaseYear > 1999, ]
barplot(table(MainPage$ReleaseYear))

# Write to CSV file:
write.csv(MainPage, file = "../../data/MainPageData.csv", row.names = FALSE)

# Save R.Data
save(list = ls(all.names = TRUE), file="ExtractMainPage_11.19.2015.RData")



