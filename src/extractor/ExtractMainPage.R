#This function reads and parses the first main page in IMDB
#INPUT:
#  - URL: http://www.imdb.com/list/ls057823854/?start=1&view=compact&sort=release_date_us:desc&defaults=1&defaults=1&release_date=1970,2015&scb=0.7988388554658741
#
#OUTPUT:
#  - CSV file "mainPageData.csv" which has below columns:
#     - MovieID
#     - MovieURL
#     - MovieTitle
#     - ReleaseYear
#     - MovieRating
#     - MovieNumUserRatings
#     - ReleaseDateUS

extractMainPage <- function(url){}
