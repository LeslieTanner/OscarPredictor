library(leaps)
library(stringr)
library(lubridate)
#library(ROCR)
##############


dataDir <- "../../data/"
load(paste0(dataDir,"movieDFClean.RData"))

#TODO
F1Score <- function(train.data,test.data, regressors, response, threshold=0.5){
  model.formula <- as.formula(paste0(response," ~ `",paste0(regressors, collapse = "` + `"),'`'))
  glm <- glm(model.formula,data = train.data,family="binomial")
  predictions <- predict(glm,newdata=test.data,type="response")
  predictions <- ifelse(predictions>=threshold,1,0)
  actuals <- test.data[,response]
  #pred <- prediction(predictions, test.data[,response])
  #F1 <- performance(pred,"f")
  TN = sum(actuals==0 & predictions==0)
  FP = sum(actuals==0 & predictions==1)
  FN = sum(actuals==1 & predictions==0)
  TP = sum(actuals==1 & predictions==1)
  Pre = TP/(TP+FP)
  Rec = TP/(TP+FN)
  F1 = 2*((Pre*Rec)/(Pre+Rec))
  return(F1)
}

#####
movieDFClean$WonOscar <- ifelse(movieDFClean$NumberOfOscars>0,1,0)

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
                          "RatingNA","AspectRatio.NA","BoxOfficeGross2012"))

response = "WonOscar"

#movieDFClean <- movieDFClean[,setdiff(names(movieDFClean), names)]

#######

# add in squares of numerical variables
for(name in names){
  if(!is.factor(movieDFClean[[name]])){
    r = range(movieDFClean[[name]], na.rm = TRUE)
    if(r[2]- r[1] > 2){
      movieDFClean[[paste0(name,"Sq")]] = movieDFClean[[name]]^2
    }
  }
}

names(movieDFClean)

#######

# Take out NA's
summary(movieDFClean)

completeIndices <- complete.cases(movieDFClean[,names])
sum(complete.cases(movieDFClean[,names]))
movieDFClean <- movieDFClean[completeIndices,]
dim(movieDFClean)

#shorts ended up all gone 
movieDFClean <- movieDFClean[,setdiff(names(movieDFClean), "Short")]

#######
# Separate Data into train and test

indices <- movieDFClean$ReleaseYear==2012 #| movieDFClean$ReleaseYear==2011
trainData <- movieDFClean[!indices,]
testData <- movieDFClean[indices,]

#######
# Run Full Model

names <- setdiff(names,c(response,"Short"))
fullModelFormula <- as.formula(paste0(response," ~ `",paste0(names, collapse = "` + `"),'`'))


fullModel <- glm(fullModelFormula,data=movieDFClean,family="binomial")
summary(fullModel)
F1Score(trainData,testData,names,response,threshold = 0.6)
#F1Score = 0.6153846, threshold = 0.6 , above and below this decreased F1 score.

#Analysis of Deviance Residuals
res.dev <- fullModel$deviance / fullModel$df.residual
res.dev #0.2156

p <- fullModel$deviance / qchisq(0.975,fullModel$df.residual)
p #0.2063


#######
# Extract Significant regressors from full model + OpeningWeekend2012

table <- summary(fullModel)$coefficients
namesSignifFullModel <- rownames(table)[table[,"Pr(>|z|)"] < .05]
namesSignifFullModel <- str_replace_all(namesSignifFullModel, '`', "")
namesSignifFullModel <- c(namesSignifFullModel[2:length(namesSignifFullModel)],"BoxOfficeOpeningWeekend2012")
namesSignifFullModel

######
# Logistic Regression only on regressors significant on previous model
response = "WonOscar"
signifFullModelFormula <- as.formula(paste0(response," ~ `",paste0(namesSignifFullModel, collapse = "` + `"),'`'))

signifFullModel <- glm(signifFullModelFormula,data=movieDFClean,family="binomial")
summary(signifFullModel)
F1Score(trainData,testData,namesSignifFullModel,response, threshold = 0.6)
#F1Score = 0.72, threshold = 0.6

#Analysis of Deviance Residuals
res.dev <- signifFullModel$deviance / signifFullModel$df.residual
res.dev #0.2276

p <- signifFullModel$deviance / qchisq(0.975,signifFullModel$df.residual)
p #0.2179


###########
# Extract Significant regressors from previous model

table <- summary(signifFullModel)$coefficients
namesSignifFullModel2 <- rownames(table)[table[,"Pr(>|z|)"] < .05]
namesSignifFullModel2 <- str_replace_all(namesSignifFullModel2, '`', "")
namesSignifFullModel2 <- namesSignifFullModel2[2:length(namesSignifFullModel2)]
namesSignifFullModel2

######
# Logistic Regression on the significant variables

response = "WonOscar"
signifFullModelFormula2 <- as.formula(paste0(response," ~ `",paste0(namesSignifFullModel2, collapse = "` + `"),'`'))

signifFullModel2<- glm(signifFullModelFormula2,data=movieDFClean,family="binomial")
summary(signifFullModel2)
F1Score(trainData,testData,namesSignifFullModel2,response, threshold = 0.6)
# F1Score = 0.72, we simplified the model, but didnt lose any predictive power.

#Analysis of Deviance Residuals
res.dev <- signifFullModel2$deviance / signifFullModel2$df.residual
res.dev #0.2281

p <- signifFullModel2$deviance / qchisq(0.975,signifFullModel2$df.residual)
p #0.2183

# Given that the ratio of deviance/df is not close to unity although the p-value is higher than 0.05, 
# we do have cause to question the adequacy of the model

#######
# Predictions for Oscars in 2012
predictions <- predict(signifFullModel2,newdata=testData,type="response")

testData$predictions <- predictions
testData <- testData[order(testData$predictions,decreasing=TRUE),]
testData[1:15,c("MovieTitle","predictions")]

