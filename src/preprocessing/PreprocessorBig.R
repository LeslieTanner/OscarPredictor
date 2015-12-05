# This code takes as input the dataframes mainPage, and movieDataFrame, and the list actorData, and
# does the following:
#x -incorporate 1 column of film counts and 1 column of TV counts for each of the top 3 stars.
#x -set NA's to 0 for NumberOfCriticsRated
#x -convert foreign currencies to USD
#x -convert USD to inflation-adjusted 2012 USD
#x -add 0-1 columns for specific factor values (genre, language(English/non)).
#? -add interaction term columns for factors with few levels
#x -remove factor columns which are too sparse
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
load(file=paste0(dataDir,"cpi.RData"))
load(file=paste0(dataDir,"FX_qrtly.RData"))
class(movieDataFrame) <- setdiff(class(movieDataFrame),"data.table")

###########
# Functions

# Does what it says!
addFactorColumns <- function(data, names, maxLevels=20, minDensity = 0, NAValue = "NA"){
    class <- class(data)
    
    if("data.table" %in% class(data)){
        class(data) <- setdiff(class(data),"data.table")
    }
    
    for(name in names){
        data[[name]] <- as.character(data[[name]])
        data[[name]][is.na(data[[name]])] = NAValue
    }
    
    levelCounts <- lapply(names, function(name) table(data[[name]]))
    names(levelCounts) <- names
    
    # throw out levels with less than minDensity
    if(minDensity != 0){
        levelCounts <- lapply(levelCounts, function(tables) tables/nrow(data))
        levelCounts <- lapply(levelCounts, function(tables) tables[tables > minDensity])
    }
    
    # throw out factors with more than maxLevels, if specified
    if(!missing(maxLevels)){
        levelCounts2 <- sapply(names, function(name) length(levelCounts[[name]]))
        names(levelCounts2) <- names
        names <- setdiff(names, names(levelCounts2)[levelCounts2 > maxLevels])
    }
    
    # add the columns if there are any left
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
    names <- setdiff(names(data), names(levelCounts))
    data <- data[,names]
    if("data.table" %in% class){ class(data) <- class }
    return(data)
}

addInteractionTerms <- function(data, names, minDensity = 0){
    class <- class(data)
    
    if("data.table" %in% class(data)){
        class(data) <- setdiff(class(data),"data.table")
    }
    
    bool <- unlist(lapply(data[,names], function(column) is.numeric(column)))
    names <- names[bool]
    n = nrow(data)
    
    for(i in 1:(length(names) - 1)){
        for(j in (i+1):length(names)){
            column = data[[names[i]]]*data[[names[j]]]
            if(sum(column > 0, na.rm = TRUE)/n > minDensity){
                data[[paste0(names[i],".",names[j])]] <- column
            }
        }
    }
    if("data.table" %in% class){ class(data) <- class }
    return(data)
}


###########
# Add movie titles
dim(movieDataFrame)
dim(mainPage)
names(mainPage)
sum(movieDataFrame$MovieURL == as.character(mainPage$MovieURL))
movieDataFrame$MovieTitle = (mainPage$MovieTitle)
names(movieDataFrame)

movieDataFrame <- data.frame(movieDataFrame, stringsAsFactors = FALSE)

########
# Exploratory
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
# median = median(movieDataFrame$NumberOfUsersRated, na.rm = TRUE)
# median
# movieDataFrame$NumberOfUsersRated[is.na(movieDataFrame$NumberOfUsersRated)] = median
# summary(movieDataFrame$NumberOfUsersRated)

hist(movieDataFrame$IMDBRating)
# median = median(movieDataFrame$IMDBRating, na.rm = TRUE)
# median
# movieDataFrame$IMDBRating[is.na(movieDataFrame$IMDBRating)] = median
# summary(movieDataFrame$IMDBRating)

movieDataFrame$MovieURL[which(is.na(movieDataFrame$NumberOfUsersRated))]
# replace manually
numberOfUsers <- c(177,305,21,269,652,53,36,1248,170,143,
                   270,112,15,207,41,126,57,68,28,130,
                   236,128,85,104,25,NA,163,147,109,333)
which(is.na(movieDataFrame$NumberOfUsersRated))
movieDataFrame$MovieTitle[(is.na(movieDataFrame$NumberOfUsersRated))]
movieDataFrame$MovieURL[(is.na(movieDataFrame$NumberOfUsersRated))]
movieDataFrame$NumberOfUsersRated[(is.na(movieDataFrame$NumberOfUsersRated))] <- numberOfUsers
movieDataFrame <- movieDataFrame[!is.na(movieDataFrame$NumberOfUsersRated),]
dim(movieDataFrame)

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
    
    filmAppearances = sapply(urls, function(url) sum(actorData[[url]]$AllYears < year) - sum(actorData[[url]]$TVMultiYears < year))
    allAppearances = sapply(urls, function(url) sum(actorData[[url]]$AllYears < year))
    return(list(MeanFilmAppearances = mean(filmAppearances, na.rm = TRUE), 
                MaxFilmAppearances = max(filmAppearances, na.rm = TRUE),
                MeanAllAppearances = mean(allAppearances, na.rm = TRUE), 
                MaxAllAppearances = max(allAppearances, na.rm = TRUE)))
}

filmAppearances <- lapply(1:nrow(movieDataFrame), getYearSummary)
length(filmAppearances)
filmAppearances <- rbindlist(filmAppearances)
class(filmAppearances) <- setdiff(class(filmAppearances),"data.table")
dim(filmAppearances)
names(filmAppearances)
summary(filmAppearances)

# inspect high values
which(filmAppearances$MaxFilmAppearances > 200)
movieDataFrame$MovieURL[filmAppearances$MaxFilmAppearances > 200]
movieDataFrame$Star1Name[filmAppearances$MaxFilmAppearances > 200]
## turns out all this data is real, just tabulated differently e.g. for Mel Blanc
# who starred in multiple episodes of Bugs Bunny in the 60's, each tabulated separately

movieDataFrame <- cbind(movieDataFrame,filmAppearances)
names(movieDataFrame)
dim(movieDataFrame)

########
# Deal with MPAA
table(movieDataFrame$MPAA, useNA = "ifany")
sum(is.na(movieDataFrame$MPAARating))

# half the na values for ratings are for tv movies
table(movieDataFrame$MPAARating, movieDataFrame$TV, useNA = "ifany")
# every tv category gets oscars
table(movieDataFrame$NumberOfOscars, movieDataFrame$TV, useNA = "ifany")

# movieDataFrame$RatingG <- 0
# movieDataFrame$RatingPG <- 0
# movieDataFrame$RatingR <- 0
# movieDataFrame$RatingNC17 <- 0
# movieDataFrame$RatingNA <- 0
# 
# movieDataFrame$RatingG[movieDataFrame$MPAARating == "G"] = 1
# movieDataFrame$RatingPG[movieDataFrame$MPAARating == "PG"] = 1
# movieDataFrame$RatingPG13[movieDataFrame$MPAARating == "PG-13"] = 1
# movieDataFrame$RatingR[movieDataFrame$MPAARating == "R"] = 1
# movieDataFrame$RatingNC17[movieDataFrame$MPAARating == "NC-17"] = 1
# movieDataFrame$RatingNA[is.na(movieDataFrame$MPAARating)] = 1

movieDataFrame <- addFactorColumns(movieDataFrame,names = c("MPAARating"),maxLevels = 100,minDensity = 0)
dim(movieDataFrame)

########
# add TV factor columns
table(movieDataFrame$TV,movieDataFrame$NumberOfOscars)
movieDataFrame$isOnTV <- as.integer(!is.na(movieDataFrame$TV))
summary(movieDataFrame$isOnTV)

#######
# convert currencies
movieDataFrame$ReleaseDate
dates <- movieDataFrame$ReleaseDate
dates <- str_trim(str_replace(dates,"([^\n]+)\\n.*","\\1"))

dates1 <- as.Date(strptime(dates, "%d %B %Y")) + 30
dates2 <- as.Date(strptime(dates, "%B %Y")) + 30
dates3 <- as.Date(strptime(dates, "%Y")) + 30
dates1[is.na(dates1)] <- dates2[is.na(dates1)]
dates1[is.na(dates1)] <- dates3[is.na(dates1)]

quarters <- paste0(quarter(dates1),"Q ",year(dates1))
length(quarters)
typeof(quarters)

unique(movieDataFrame$BoxOfficeGrossCurr)
movieDataFrame$BoxOfficeGrossCurr[movieDataFrame$BoxOfficeGrossCurr == "£"] = "GBP"
movieDataFrame$BoxOfficeBudgetCurr[movieDataFrame$BoxOfficeBudgetCurr == "£"] = "GBP"
movieDataFrame$BoxOfficeBudgetCurr[movieDataFrame$BoxOfficeBudgetCurr == "€"] = "EUR"

amounts <- sapply(1:length(quarters), function(i){
    quarter = quarters[i]
    curr = movieDataFrame$BoxOfficeGrossCurr[i]
    amount = movieDataFrame$BoxOfficeGross[i]
    if(!is.na(curr) & curr != "$"){
        amount = amount/qrtly_rates[[curr]][qrtly_rates$Qtr == quarter]
        amount = amount[1]
    }
    amount
})

hist(amounts)
movieDataFrame$MovieTitle[which.max(amounts)]
# Avatar

BoxOfficeGross <- amounts

unique(movieDataFrame$BoxOfficeOpeningWeekendCurr)
movieDataFrame$BoxOfficeOpeningWeekendCurr[movieDataFrame$BoxOfficeOpeningWeekendCurr == "£"] = "GBP"

amounts <- sapply(1:length(quarters), function(i){
    quarter = quarters[i]
    curr = movieDataFrame$BoxOfficeOpeningWeekendCurr[i]
    amount = movieDataFrame$BoxOfficeOpeningWeekend[i]
    if(!is.na(curr) & curr != "$"){
        amount = amount/qrtly_rates[[curr]][qrtly_rates$Qtr == quarter]
        amount = amount[1]
    }
    amount
})

hist(amounts)
movieDataFrame$MovieTitle[which.max(amounts)]
# The Avengers

BoxOfficeOpeningWeekend <- amounts


amounts <- sapply(1:length(quarters), function(i){
    quarter = quarters[i]
    curr = movieDataFrame$BoxOfficeBudgetCurr[i]
    amount = movieDataFrame$BoxOfficeBudget[i]
    if(!is.na(curr) & curr != "$"){
        amount = amount/qrtly_rates[[curr]][qrtly_rates$Qtr == quarter]
        amount = amount[1]
    }
    amount
})

BoxOfficeBudget <- amounts

years <- year(as.Date(strptime(movieDataFrame$ReleaseYear, "%Y")) + 30)
hist(years)

inflation <- sapply(1:nrow(movieDataFrame), function(i) cpi$inflation[match(years[i],cpi$Year)])
BoxOfficeOpeningWeekend <- BoxOfficeOpeningWeekend*inflation
BoxOfficeGross <- BoxOfficeGross*inflation
BoxOfficeBudget <- BoxOfficeBudget*inflation

movieDataFrame$BoxOfficeGross2012 <- BoxOfficeGross
movieDataFrame$BoxOfficeOpeningWeekend2012 <- BoxOfficeOpeningWeekend
movieDataFrame$BoxOfficeBudget2012 <- BoxOfficeBudget

movieDataFrame$MovieTitle[order(BoxOfficeOpeningWeekend, decreasing = T)[1:10]]
movieDataFrame$MovieTitle[order(BoxOfficeGross, decreasing = T)[1:10]]
movieDataFrame$MovieTitle[order(BoxOfficeBudget, decreasing = T)[1:10]]

# months <- month(strptime(dates[1:10], "%d %B %Y"))
# months2 <- month(strptime(dates[1:10], "%B %Y"))
# years <- year(strptime(dates[1:10], "%d %B %Y"))
# years2 <- year(strptime(dates[1:10], "%B %Y"))
# years3 <- year(strptime(dates[1:10], "%Y"))
# months[is.na(months)] = months2[is.na(months)]
# years[is.na(years)] = years2[is.na(years)]
# years[is.na(years)] = years3[is.na(years)]
names(movieDataFrame)

#######
# add release month or quarter factors
dates <- movieDataFrame$ReleaseDate
dates <- str_trim(str_replace(dates,"([^\n]+)\\n.*","\\1"))

dates1 <- as.Date(strptime(dates, "%d %B %Y"))
dates2 <- as.Date(strptime(dates, "%B %Y"))
dates3 <- as.Date(strptime(dates, "%Y"))
dates1[is.na(dates1)] <- dates2[is.na(dates1)]
dates1[is.na(dates1)] <- dates3[is.na(dates1)]

months <- month(dates1)
quarters <- quarter(dates1)

movieDataFrame$ReleaseMonth = months
movieDataFrame$ReleaseQuarter = quarters
# This puts Q1 = 3:5, Q2 = 6:8, Q3 = 9:11.  We should exclude rows in Q1 when regressing 
# oscars on gross, since this would not be known at the time of the oscars.
offsetquarters <- floor((months - 3)/3) + 1
offsetquarters[offsetquarters == 0] = 4
unique(offsetquarters)
movieDataFrame$ReleaseQuarterOffset = offsetquarters

names(movieDataFrame)
summary(movieDataFrame$ReleaseYear)
summary(movieDataFrame$ReleaseMonth)
summary(movieDataFrame$ReleaseQuarter)
summary(movieDataFrame$ReleaseQuarterOffset)

oscarsByMonth <- table(movieDataFrame$ReleaseMonth, movieDataFrame$NumberOfOscars, useNA = "ifany")
oscarsByQuarter <- table(movieDataFrame$ReleaseQuarter, movieDataFrame$NumberOfOscars, useNA = "ifany")
oscarsByQuarterOffset <- table(movieDataFrame$ReleaseQuarterOffset, movieDataFrame$NumberOfOscars, useNA = "ifany")
oscarsByMonth/rowSums(oscarsByMonth)
oscarsByQuarter/rowSums(oscarsByQuarter)
oscarsByQuarterOffset/rowSums(oscarsByQuarterOffset)

dim(movieDataFrame)

# Add quarter factors
movieDataFrame <- addFactorColumns(movieDataFrame, names = c("ReleaseQuarter","ReleaseQuarterOffset"),maxLevels = 12,
                                        minDensity = .05,NAValue = "NA")

#######
# add aspect ratio factor
aspectRatios <- str_replace_all(movieDataFrame$AspectRatio, "[\\s]+","")

table(aspectRatios,useNA = "ifany")/nrow(movieDataFrame)

sum(table(aspectRatios)/nrow(movieDataFrame) > .01)
movieDataFrame <- addFactorColumns(movieDataFrame,names = "AspectRatio",minDensity = .01,
                                    NAValue = "NA")

#######
# Add interaction terms between:
# IsOnTV, MPAARating, AspectRatio, ReleaseQuarterOffset?
# TV-Rating, Quarter-Rating, Aspect-Rating, TV-Quarter
table <- table(movieDataFrame$isOnTV,movieDataFrame$)
table/rowSums(table)

names = str_detect(names(movieDataFrame),"MPAA|QuarterOffset|OnTV|Aspect")
names = names(movieDataFrame)[names]
movieDataFrame <- addInteractionTerms(movieDataFrame, names, minDensity = .05)

#######
# Add a binary response for oscars
movieDataFrame$WonOscar <- as.integer(movieDataFrame$NumberOfOscars > 0)

#######
# remove columns no longer needed: genres, starURLs, starIDs, starNames, directorID, directorName,directorURL
names(movieDataFrame)
names <- setdiff(names(movieDataFrame), c("Language","ReleaseDate","ReleaseYear","Country",
                                          "Genre1","Genre2","Genre3",
                                          "DirectorName","MovieURL",
                                          "DirectorID","DirectorURL",
                                          "Star1Name","Star1ID","Star1URL",
                                          "Star2Name","Star2ID","Star2URL",
                                          "Star3Name","Star3ID","Star3URL",
                                          "MPAARating", "TV", 
                                          "BoxOfficeBudget", "BoxOfficeBudgetCurr",
                                          "BoxOfficeOpeningWeekend","BoxOfficeOpeningWeekendCurr",
                                          "BoxOfficeGross","BoxOfficeGrossCurr"))
movieDataFrame <- movieDataFrame[,names]
names(movieDataFrame)

#######################
# make a new copy of the clean data and save
movieDFCleanBig <- movieDataFrame

save(movieDFCleanBig, file = paste0(dataDir,"movieDFCleanBig.RData"))
