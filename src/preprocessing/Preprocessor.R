# This code takes as input the dataframes mainPage, and movieDataFrame, and the list actorData, and
# does the following:
# -incorporate 1 column of film counts and 1 column of TV counts for each of the top 3 stars.
#x -set NA's to 0 for NumberOfCriticsRated
# -convert foreign currencies to USD
# -convert USD to inflation-adjusted 2015 USD
#x -add 0-1 columns for specific factor values (genre, language(English/non)).
# -add interatction term columns for factors with few levels
# -remove factor columns which are too sparse
#x -replace missing values of regressors with estimates where appropriate (number of critics)
# -replace missing values of regressors with NA strings where appropriate (i.e. for MPAA rating)
# -reduce movie data to complete cases
library(stringr)
library(data.table)

dataDir <- "~/Git/OscarPredictor/data/"
load(file=paste0(dataDir,"movieData.RData"))
load(file=paste0(dataDir,"actorData.RData"))
load(file=paste0(dataDir,"mainPageData.RData"))

# Add movie titles
dim(movieDataFrame)
dim(mainPage)
names(mainPage)
movieDataFrame$MovieTitle = mainPage$MovieTitle
names(movieDataFrame)

########
# Explotatory
summary(movieDataFrame)
hist(movieDataFrame$DurationMins, xlim = c(0,480), breaks = 300)
par(mfrow = c(1,2))
hist(movieDataFrame$NumberOfCriticsRated, xlim = c(0,800), breaks = 150)
hist(movieDataFrame$NumberOfUsersRated, xlim = c(0,1000), breaks = 150)
hist(movieDataFrame$NumberOfOscars, xlim = c(0,13), ylim = c(0,500))

sum(movieDataFrame$ReleaseYear == 2014, na.rm = TRUE)
sum(movieDataFrame$ReleaseYear == 2013, na.rm = TRUE)
sum(movieDataFrame$ReleaseYear == 2012, na.rm = TRUE)
sum(movieDataFrame$ReleaseYear == 2011, na.rm = TRUE)
sum(movieDataFrame$ReleaseYear == 2010, na.rm = TRUE)
sum(movieDataFrame$ReleaseYear > 2020, na.rm = TRUE)

movieDataFrame$ReleaseYear[movieDataFrame$ReleaseYear > 2020]
movieDataFrame$MovieURL[movieDataFrame$ReleaseYear > 2020]
movieDataFrame[which(movieDataFrame$NumberOfCriticsRated >800),]
movieDataFrame[which(movieDataFrame$DurationMins > 480),]
sum(movieDataFrame$DurationMins > 480, na.rm = TRUE)
which(movieDataFrame$DurationMins > 600)

languages <- movieDataFrame$Language
bool <- sapply(languages, function(str) str_detect(str,"English"))
sum(bool, na.rm = TRUE)/length(languages)
bool <- sapply(languages, function(str) str_detect(str,"^English"))
sum(bool, na.rm = TRUE)/length(languages)
# We'll create a flag for English at the beginning of the string, based on slightly 
# better discriminative power

########
# Add the flag for English
movieDataFrame$English <- 0
movieDataFrame$English[bool] = 1

########
# Remove years > 2013 (incomplete data) and NA values
movieDataFrame <- movieDataFrame[movieDataFrame$ReleaseYear < 2013,]
movieDataFrame <- movieDataFrame[!is.na(movieDataFrame$ReleaseYear),]
summary(movieDataFrame$ReleaseYear)
dim(movieDataFrame)

########
# Throw out the longest most meaningless movie of all time
movieDataFrame <- movieDataFrame[movieDataFrame$DurationMins < 1000,]
dim(movieDataFrame)

########
# Remove duplicates
movieDataFrame <- movieDataFrame[!duplicated(movieDataFrame$MovieURL),]
dim(movieDataFrame)

########
# Fill NA with 0 for number of critics
movieDataFrame$NumberOfCriticsRated[is.na(movieDataFrame$NumberOfCriticsRated)] = 0
summary(movieDataFrame$NumberOfCriticsRated)

########
# Basically, throw out a bunch of TV trash
sum(is.na(movieDataFrame$NumberOfUsersRated) & !is.na(movieDataFrame$IMDBRating))
summary(movieDataFrame$NumberOfUsersRated)
summary(movieDataFrame$IMDBRating)
movieDataFrame[is.na(movieDataFrame$NumberOfUsersRated) & !is.na(movieDataFrame$IMDBRating),][7:10,]

movieDataFrame <- movieDataFrame[!is.na(movieDataFrame$NumberOfUsersRated),]
dim(movieDataFrame)

########
genres <- unique(c(as.character(movieDataFrame$Genre1),as.character(movieDataFrame$Genre2), as.character(movieDataFrame$Genre3)))
genres <- genres[!is.na(genres)]
length(genres)
m <- matrix(0,nrow = nrow(movieDataFrame), ncol = length(genres))
genres_df <- data.frame(m)
names(genres_df) = genres

for( i  in 1:nrow(movieDataFrame)){
    g1 <- as.character(movieDataFrame$Genre1[i])
    g2 <-  as.character(movieDataFrame$Genre2[i])
    g3  <- as.character(movieDataFrame$Genre3[i])
    if(!is.na(g1))  genres_df[i,g1] <-  1
    if(!is.na(g2))  genres_df[i,g2] <-  1
    if(!is.na(g3))  genres_df[i,g3] <-  1
}

movieDataFrame <- cbind(movieDataFrame,genres_df)
names(movieDataFrame)

########
# Film and TV counts
getYearSummary <- function(i){
    url1 = movieDataFrame$Star1URL[i]
    url2 = movieDataFrame$Star2URL[i]
    url3 = movieDataFrame$Star3URL[i]
    urls = c(url1,url2,url3)
    year = movieDataFrame$ReleaseYear[i]
    
    filmAppearances = sapply(urls, function(url) sum(actorData[[url]]$FilmYears < year))
    return(list(MeanFilmAppearances = mean(filmAppearances), MaxFilmAppearances = max(filmAppearances)))
}

filmAppearances <- lapply(1:nrow(movieDataFrame), getYearSummary)
length(filmAppearances)
filmAppearances <- rbindlist(filmAppearances)
dim(filmAppearances)
names(filmAppearances)

movieDataFrame <- cbind(movieDataFrame,filmAppearances)
names(movieDataFrame)

#######
# remove columns no longer needed: genres, starURLs, starIDs, starNames, directorID, directorName,directorURL
names <- setdiff(names(movieDataFrame), c("Language","ReleaseDate","Genre1","Genre2","Genre3",
                                          "DirectorName","DirectorID","DirectorURL",
                                          "Star1Name","Star1ID","Star1URL",
                                          "Star2Name","Star2ID","Star2URL",
                                          "Star3Name","Star3ID","Star3URL"))
movieDataFrame <- movieDataFrame[,names]
names(movieDataFrame)

# make a new copy of the clean data
movieDFClean <- movieDataFrame

########
# Deal with MPAA
table(movieDFClean$MPAA)
sum(is.na(movieDFClean$MPAARating))

movieDFClean$RatingG <- 0
movieDFClean$RatingPG <- 0
movieDFClean$RatingR <- 0
movieDFClean$RatingNA <- 0
movieDFClean$RatingG[movieDFClean$MPAARating == "G"] = 1
movieDFClean$RatingPG[movieDFClean$MPAARating == "PG"] = 1
movieDFClean$RatingR[movieDFClean$MPAARating == "R"] = 1
movieDFClean$RatingNA[is.na(movieDFClean$MPAARating)] = 1

movieDFClean <- movieDFClean[,setdiff(names(movieDFClean), "MPAARating")]
names(movieDFClean)

save(movieDFClean, file = paste0(dataDir,"movieDFClean.RData"))

