library(leaps)
library(stringr)
library(lubridate)

dataDir <- "~/Git/OscarPredictor/data/"
load(paste0(dataDir,"movieDFClean.RData"))

bootStrapTestError <- function(dataframe, regressors, response, iterations = 200, percent.train= .9, returnList = T){
    model.formula <- as.formula(paste0(response," ~ `",paste0(regressors, collapse = "` + `"),'`'))
    testErrors <- sapply(1:iterations, function(i){
        indices <- sample(1:nrow(dataframe), round(percent.train*nrow(dataframe)))
        train.data <- dataframe[indices,]
        test.data <- dataframe[-indices,]
        lm.train <- lm(model.formula,data = train.data)
        testError <- mean((predict.lm(lm.train, newdata = test.data) - test.data[[response]])^2)
        testError
    })
    if(returnList){return(testErrors)}
    return(mean(testErrors))
}

#######
# add a factor for quarters
dates <- movieDFClean$ReleaseDate
dates <- str_trim(str_replace(dates,"([^\n]+)\\n.*","\\1"))

dates1 <- as.Date(strptime(dates, "%d %B %Y"))
dates2 <- as.Date(strptime(dates, "%B %Y"))
dates3 <- as.Date(strptime(dates, "%Y"))
dates1[is.na(dates1)] <- dates2[is.na(dates1)]
dates1[is.na(dates1)] <- dates3[is.na(dates1)]

quarters <- quarter(dates1)
movieDFClean$ReleaseQuarter = as.factor(quarters)

######
# somehow we ended up with NAs for pg13
movieDFClean$RatingPG13[is.na(movieDFClean$RatingPG13)] = 0


#####
# names for regressor-response pair plots
names <- names(movieDFClean)

names <- setdiff(names, c("MovieURL","ReleaseYear","MovieTitle",
                          "ReleaseDate","NumberOfOscars","Nominations",
                          "RatingNA","AspectRatio.NA"))

response = "BoxOfficeGross2012"

########
# pair plots
length(names)
par(mfrow = c(7,7))
par(mar = c(1,1,2,1))
plot.new()
for(name in names){
    plot(x =movieDFClean[[name]], y =movieDFClean$BoxOfficeGross2012, main = name, cex = .2, pch =".")
}

#########
par(mfrow = c(7,7))
par(mar = c(1,1,2,1))
plot.new()
for(name in names){
    if(min(movieDFClean[[name]],na.rm = T) == 0 & max(movieDFClean[[name]], na.rm = T) == 1){
        plot(x = as.factor(movieDFClean[[name]]), y = movieDFClean$NumberOfOscars, main = name, cex = .2, pch =".")
    }else{
    plot(x =movieDFClean[[name]], y = log(movieDFClean$BoxOfficeGross2012), main = name, cex = .2, pch =".")
    }
}

#########
# names <- names(movieDFClean)
# names <- setdiff(names, c("MovieURL","ReleaseYear","MovieTitle",
#                           "ReleaseDate","Nominations",
#                           "RatingNA","AspectRatio.NA"))
# 
# par(mfrow = c(7,7))
# par(mar = c(1,1,2,1))
# plot.new()
# for(name in names){
#     if(!is.factor(movieDFClean[[name]]) & min(movieDFClean[[name]],na.rm = T) == 0 & max(movieDFClean[[name]], na.rm = T) == 1){
#         plot(x = as.factor(movieDFClean[[name]]), y = movieDFClean$NumberOfOscars, main = name, cex = .2, pch =".")
#     }else{
#         plot(x =movieDFClean[[name]], y = (movieDFClean$NumberOfOscars), main = name, cex = .2, pch =".")
#    }
# }
# 


# add in squares of numerical variables
for(name in names){
    if(!is.factor(movieDFClean[[name]])){
        r = range(movieDFClean[[name]], na.rm = TRUE)
        if(r[2]- r[1] > 2){
            movieDFClean[[paste0(name,"Sq")]] = movieDFClean[[name]]^2
        }
    }
}
movieDFClean <- movieDFClean[,setdiff(names(movieDFClean), "BoxOfficeGross2012Sq")]

names(movieDFClean)

######
# Log response
movieDFClean$BoxOfficeGross2012Ln <- log(movieDFClean$BoxOfficeGross2012)

#######
# Take out NA's
summary(movieDFClean)

completeIndices <- complete.cases(movieDFClean)
sum(complete.cases(movieDFClean))
movieDFClean <- movieDFClean[completeIndices,]
dim(movieDFClean)

# shorts ended up all gone
movieDFClean <- movieDFClean[,setdiff(names(movieDFClean), "Short")]


#######
# Train a full model as a first pass to identify useful variables
names <- names(movieDFClean)

names <- setdiff(names, c("MovieURL","ReleaseYear","MovieTitle",
                          "ReleaseDate","NumberOfOscars","Nominations",
                          "RatingNA","AspectRatio.NA"))

response = "BoxOfficeGross2012"

names <- setdiff(names,c(response,"BoxOfficeGross2012Ln"))
fullModelFormula <- as.formula(paste0(response," ~ `",paste0(names, collapse = "` + `"),'`'))

fullModel <- lm(fullModelFormula, data = movieDFClean)

summary(fullModel)
par(mfrow = c(2,2))
plot(fullModel)
# funnel-shaped and super heavy tails in the QQ.  Let's try the log response.

testError <- bootStrapTestError(dataframe = trainData, regressors = names, response = response)
mean(testError)
mean((fullModel$residuals)^2)

par(mfrow = c(1,1))
par(mar = c(2,2,2,2))
hist(testError)
# Non-normal test errors too


#######
# Train a full model but use log response
names <- names(movieDFClean)

names <- setdiff(names, c("MovieURL","ReleaseYear","MovieTitle",
                          "ReleaseDate","NumberOfOscars","Nominations",
                          "RatingNA","AspectRatio.NA"))

response = "BoxOfficeGross2012Ln"

names <- setdiff(names,c(response,"BoxOfficeGross2012"))

fullModelFormula <- as.formula(paste0(response," ~ `",paste0(names, collapse = "` + `"),'`'))

fullModelLn <- lm(fullModelFormula, data = movieDFClean)

summary(fullModelLn)
par(mfrow = c(2,2))
plot(fullModelLn)
# slightly light tails, but better resid/fitted plot

testError <- bootStrapTestError(dataframe = movieDFClean, regressors = names, response = response)
mean(testError)

par(mfrow = c(1,1))
par(mar = c(2,2,2,2))
hist(testError)

#######
# Train a full model using sqrt of response
names <- names(movieDFClean)
names <- setdiff(names, c("MovieURL","ReleaseYear","MovieTitle",
                          "ReleaseDate","NumberOfOscars","Nominations",
                          "RatingNA","AspectRatio.NA"))

# We tried other powers of the response and the fourth root seemed to show the least 
# problems with adequacy (QQ plot and scale-location)
# there are still slight problems- double-bow
movieDFClean$BoxOfficeGross20124thRoot <- (movieDFClean$BoxOfficeGross2012)^(1/4)

response = "BoxOfficeGross20124thRoot"

movieDFClean <- movieDFClean[,setdiff(names(movieDFClean),"BoxOfficeGross2012Sqrt")]
movieDFClean <- movieDFClean[,setdiff(names(movieDFClean),"BoxOfficeGross2012Ln")]
names <- setdiff(names,c(response,"BoxOfficeGross2012","BoxOfficeGross2012Ln","BoxOfficeGross2012Sqrt"))

fullModelFormula4thRoot <- as.formula(paste0(response," ~ `",paste0(names, collapse = "` + `"),'`'))

fullModelFourthRoot <- lm(fullModelFormula4thRoot, data = movieDFClean)

summary(fullModelFourthRoot)
par(mfrow = c(2,2))
plot(fullModelFourthRoot)

names(movieDFClean)
names
response
testError <- bootStrapTestError(dataframe = movieDFClean, regressors = names, response = response)
mean(((fullModelFourthRoot$fitted.values)^4 - movieDFClean[[response]])^2)
mean(testError)

par(mfrow = c(1,1))
par(mar = c(2,2,2,2))
hist(testError)

###########
# pull out the best variables
table <- summary(fullModelFourthRoot)$coefficients
namesSignifFullModel <- rownames(table)[table[,"Pr(>|t|)"] < .05]
namesSignifFullModel
namesSignifFullModel <- str_replace_all(namesSignifFullModel, '`', "")

response = "BoxOfficeGross20124thRoot"
signifFullModelFormula <- as.formula(paste0(response," ~ `",paste0(namesSignifFullModel, collapse = "` + `"),'`'))

signifFullModel <- lm(signifFullModelFormula,data = movieDFClean)
summary(signifFullModel)
par(mfrow = c(2,2))
plot(signifFullModel)


namesSignifFullModel2 <- setdiff(namesSignifFullModel,c("Drama","Comedy"))
response = "BoxOfficeGross20124thRoot"
signifFullModelFormula2 <- as.formula(paste0(response," ~ `",paste0(namesSignifFullModel2, collapse = "` + `"),'`'))

signifFullModel2 <- lm(signifFullModelFormula2,data = movieDFClean)
summary(signifFullModel2)
par(mfrow = c(2,2))
plot(signifFullModel2)


#######
# training and testing split
set.seed(2342208)
trainIndices <- sample(1:nrow(movieDFClean), round(.9*nrow(movieDFClean)))

trainData <- movieDFClean[trainIndices,]
testData <- movieDFClean[-trainIndices,]

#########
# Try forward selection

subsets <- regsubsets(fullModelFormula4thRoot,data = trainData,nbest = 3,method = "forward")
subsetsSummary <- summary(subsets)
plot(subsetsSummary$cp)

dim(subsetsSummary$outmat)

namesForward8 <- colnames(subsetsSummary$outmat)[subsetsSummary$outmat[22,] == "*"]

intersect(namesForward8, namesSignifFullModel)

######
# Diagnostics
library(car)
vif(fullModelFourthRoot)

#########
# Summary:
# we ran the full model and then pulled 




