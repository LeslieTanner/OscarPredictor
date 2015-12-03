# This code takes as input the dataframes mainPage, and movieDataFrame, and the list actorData, and
# does the following:
#x -incorporate 1 column of film counts and 1 column of TV counts for each of the top 3 stars.
#x -set NA's to 0 for NumberOfCriticsRated
# -convert foreign currencies to USD
# -convert USD to inflation-adjusted 2015 USD
#x -add 0-1 columns for specific factor values (genre, language(English/non)).
# -add interaction term columns for factors with few levels
# -remove factor columns which are too sparse
#x -replace missing values of regressors with estimates where appropriate (number of critics)
#x -replace missing values of regressors with NA strings where appropriate (i.e. for MPAA rating)
#x -reduce movie data to complete cases
#x -remove TV rows?

library(stringr)
library(data.table)

dataDir <- "~/Git/OscarPredictor/data/"
load(file=paste0(dataDir,"movieData.RData"))
load(file=paste0(dataDir,"actorData.RData"))
load(file=paste0(dataDir,"mainPageData.RData"))

###########
# Does what it says!
addFactorColumns <- function(data, names, maxLevels = 8, minDensity = 0, NAValue = "NA"){
    for(name in names){
        data[[name]][is.na(data[[name]])] = "NA"
    }
    
    # throw out factors with more than maxLevels, if specified
    if(!missing(maxLevels)){
        levelCounts <- lapply(names, function(name) length(unique(data[[name]])))
        names(levelCounts) <- names
        names <- setdiff(names, names(levelCounts)[levelCounts > maxLevels])
    }
    
    # throw out levels with less than minDensity
    if(minDensity != 0){
        levelCounts <- lapply(names, function(name) table(data[[name]]))
        names(levelCounts) <- names
        levelCounts <- lapply(levelCounts, function(table) table/nrow(data))
        levelCounts <- lapply(levelCounts, function(table) table[table > minDensity])
    }
    
    # add the columns if there are any left
    #####
    if(length(levelCounts) > 0){
        for(factorName in names(levelCounts)){
            table = levelCounts[[factorName]]
            for(levelName in names(table)){
                level = table[[levelName]]
                print(factorName)
                print(levelName)
                columnName = paste0(factorName,".",as.character(levelName))
                print(columnName)
                column <-  integer(nrow(data))
                column[data[[factorName]] == levelName] = 1
                data[[columnName]] = column
            }
        }
    }
    
    data <- data[,setdiff(names(data), names)]
    return(data)
}

###########
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
languages <- movieDataFrame$Language
bool <- sapply(languages, function(str) str_detect(str,"^English"))
movieDataFrame$English[bool] = 1

########
# Remove years > 2012 (incomplete data) and NA values
# This should help with bias since the later years are poorly sampled
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
# replace missing values of IMDBRating and NumberOfUsersRated
par(mfrow = c(1,2))
hist(movieDataFrame$NumberOfUsersRated)
median = median(movieDataFrame$NumberOfUsersRated, na.rm = TRUE)
median
movieDataFrame$NumberOfUsersRated[is.na(movieDataFrame$NumberOfUsersRated)] = median
summary(movieDataFrame$NumberOfUsersRated)

hist(movieDataFrame$IMDBRating)
median = median(movieDataFrame$IMDBRating, na.rm = TRUE)
median
movieDataFrame$IMDBRating[is.na(movieDataFrame$IMDBRating)] = median
summary(movieDataFrame$IMDBRating)

# # Basically, throw out a bunch of TV trash
# sum(is.na(movieDataFrame$NumberOfUsersRated) & !is.na(movieDataFrame$IMDBRating))
# summary(movieDataFrame$NumberOfUsersRated)
# summary(movieDataFrame$IMDBRating)
# movieDataFrame$TV[is.na(movieDataFrame$NumberOfUsersRated) & !is.na(movieDataFrame$IMDBRating)]
# 
# missingratings <- which(is.na(movieDataFrame$NumberOfUsersRated) & !is.na(movieDataFrame$IMDBRating))
# tv <- which(str_detect(movieDataFrame$TV, "TV"))
# missingratings
# tv
# setdiff(missingratings,tv)
# 
# movieDataFrame <- movieDataFrame[!is.na(movieDataFrame$NumberOfUsersRated),]
# dim(movieDataFrame)

########
genres <- unique(c(as.character(movieDataFrame$Genre1),as.character(movieDataFrame$Genre2), as.character(movieDataFrame$Genre3)))
genres <- genres[!is.na(genres)]
length(genres)
genreslist <- (c(as.character(movieDataFrame$Genre1),as.character(movieDataFrame$Genre2), as.character(movieDataFrame$Genre3)))
# counts of all the genres
table(genreslist)

# how many shorts get oscars? a lot!
sum(movieDataFrame$Genre2 == "Short" & movieDataFrame$NumberOfOscars >0, na.rm = TRUE)

# make a dataframe of genre 0-1 values
m <- matrix(0,nrow = nrow(movieDataFrame), ncol = length(genres))
genres_df <- data.frame(m)
names(genres_df) = genres

# for(name in names(genres_df)){
#     print(name)
#     genres_df[[name]] = as.integer(movieDataFrame$Genre1 == name | movieDataFrame$Genre2 == name | movieDataFrame$Genre3 == name)
#
# }

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
summary(filmAppearances)
sum(filmAppearances$MaxFilmAppearances > 200)
movieDataFrame$MovieURL[filmAppearances$MaxFilmAppearances > 200]

movieDataFrame <- cbind(movieDataFrame,filmAppearances)
names(movieDataFrame)

########
# Deal with MPAA
table(movieDataFrame$MPAA)
sum(is.na(movieDataFrame$MPAARating))

movieDataFrame$RatingG <- 0
movieDataFrame$RatingPG <- 0
movieDataFrame$RatingR <- 0
movieDataFrame$RatingNC17 <- 0
movieDataFrame$RatingNA <- 0

movieDataFrame$RatingG[movieDataFrame$MPAARating == "G"] = 1
movieDataFrame$RatingPG[movieDataFrame$MPAARating == "PG"] = 1
movieDataFrame$RatingPG13[movieDataFrame$MPAARating == "PG-13"] = 1
movieDataFrame$RatingR[movieDataFrame$MPAARating == "R"] = 1
movieDataFrame$RatingNC17[movieDataFrame$MPAARating == "NC-17"] = 1
movieDataFrame$RatingNA[is.na(movieDataFrame$MPAARating)] = 1


########
# add TV factor columns

table(movieDataFrame$TV,movieDataFrame$NumberOfOscars)

#######
# remove columns no longer needed: genres, starURLs, starIDs, starNames, directorID, directorName,directorURL
names <- setdiff(names(movieDataFrame), c("Language","ReleaseDate","Genre1","Genre2","Genre3",
                                          "DirectorName","DirectorID","DirectorURL",
                                          "Star1Name","Star1ID","Star1URL",
                                          "Star2Name","Star2ID","Star2URL",
                                          "Star3Name","Star3ID","Star3URL","MPAARating"))
movieDataFrame <- movieDataFrame[,names]
names(movieDataFrame)


#######################
# make a new copy of the clean data and save
movieDFClean <- movieDataFrame

save(movieDFClean, file = paste0(dataDir,"movieDFClean.RData"))
