library(leaps)
library(stringr)
library(lubridate)
#library(ROCR)
##############


dataDir <- "../../data/"
load(paste0(dataDir,"movieDFClean.RData"))
load(paste0(dataDir,"movieDFClean2016.RData"))

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

# Given that the ratio of deviance/df is not close to unity although the p-value is higher than 0.05, we do have cause to question the adequacy of the model


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

# Given that the ratio of deviance/df is not close to unity although the p-value is higher than 0.05, we do have cause to question the adequacy of the model


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

# Given that the ratio of deviance/df is not close to unity although the p-value is higher than 0.05, we do have cause to question the adequacy of the model

#######
# Predictions for Oscars in 2012
predictions <- predict(signifFullModel2,newdata=testData,type="response")

testData$predictions <- predictions
testData <- testData[order(testData$predictions,decreasing=TRUE),]
testData[1:20,c("MovieTitle","predictions")]

testData$binaryPrediction <- ifelse(testData$predictions>=0.5,1,0)
testData$TN <- ifelse(testData$WonOscar==0 & testData$binaryPrediction==0, 1, 0)
testData$FP <- ifelse(testData$WonOscar==0 & testData$binaryPrediction==1, 1, 0)
testData$FN <- ifelse(testData$WonOscar==1 & testData$binaryPrediction==0, 1, 0)
testData$TP <- ifelse(testData$WonOscar==1 & testData$binaryPrediction==1, 1, 0)
sum(testData$TN) #215
sum(testData$FN) #2
sum(testData$TP) #10
sum(testData$FP) #5
      
Precision = sum(testData$TP)/(sum(testData$TP)+sum(testData$FP))
# 0.6666667
Recall = sum(testData$TP)/(sum(testData$TP)+sum(testData$FN))
# 0.8333333
Accuracy = (sum(testData$TP) + sum(testData$TN))/(sum(testData$TP)+sum(testData$FP)+sum(testData$TN)+sum(testData$FN))
#  0.9698276

#######
# Legit regressors

response = "WonOscar"
namesSignifFullModel3 <- setdiff(namesSignifFullModel2,c("IMDBRating","NumberOfUsersRated"))
signifFullModelFormula3 <- as.formula(paste0(response," ~ `",paste0(namesSignifFullModel3, collapse = "` + `"),'`'))


signifFullModel3<- glm(signifFullModelFormula3,data=movieDFClean,family="binomial")
summary(signifFullModel3)
F1Score(trainData,testData,namesSignifFullModel3,response, threshold = 0.6)
# Predictions for Oscars in 2012
predictions <- predict(signifFullModel3,newdata=testData,type="response")

testData$predictions <- predictions
testData <- testData[order(testData$predictions,decreasing=TRUE),]
testData[1:20,c("MovieTitle","predictions")]

testData$binaryPrediction <- ifelse(testData$predictions>=0.5,1,0)
testData$TN <- ifelse(testData$WonOscar==0 & testData$binaryPrediction==0, 1, 0)
testData$FP <- ifelse(testData$WonOscar==0 & testData$binaryPrediction==1, 1, 0)
testData$FN <- ifelse(testData$WonOscar==1 & testData$binaryPrediction==0, 1, 0)
testData$TP <- ifelse(testData$WonOscar==1 & testData$binaryPrediction==1, 1, 0)
sum(testData$TN) #211
sum(testData$FN) #5
sum(testData$TP) #7
sum(testData$FP) #9

Precision = sum(testData$TP)/(sum(testData$TP)+sum(testData$FP))
# 0.4375
Recall = sum(testData$TP)/(sum(testData$TP)+sum(testData$FN))
# 0.5833333
Accuracy = (sum(testData$TP) + sum(testData$TN))/(sum(testData$TP)+sum(testData$FP)+sum(testData$TN)+sum(testData$FN))
#  0.9396552


##############
# Compare with Actual Oscar Winners of 2012.  See what our prediction was for actual winners:
winners2012 <- c("Lincoln", "Les Misérables", "Django Unchained", "Zero Dark Thirty", "Skyfall",
                 "Argo", "Silver Linings Playbook", "Amour", "Life of Pi", "Anna Karenina", "Brave",
                 "Curfew", "Inocente", "Paper Man", "Searching for Sugar Man")

# which(testData$MovieTitle == winners2012[1])

testData[c(1,3,4,5,7,8,13,17,19,27,33,49), c("MovieTitle", "predictions")]





######### Oscar winners for 2016.... Guess:
movieDFClean2016$Musical <- 0
predictions <- predict(signifFullModel3,newdata=movieDFClean2016,type="response")

movieDFClean2016$predictions <- predictions
movieDFClean2016 <- movieDFClean2016[order(movieDFClean2016$predictions,decreasing=TRUE),]
movieDFClean2016 <- as.data.frame(movieDFClean2016)
movieDFClean2016[, c("MovieTitle","predictions")]


# Next, categorize by actual nomination category:
names(movieDFClean2016)
df <- as.data.frame(cbind(movieDFClean2016$MovieTitle, 
                          movieDFClean2016$MovieURL,
                          movieDFClean2016$predictions))
names(df) <- c("MovieTitle", "MovieURL", "predictions")

# Un-factorize
df$MovieTitle <- as.character(df$MovieTitle)
df$MovieURL <- as.character(df$MovieURL)
df$predictions <- as.character(df$predictions)

# movieDataFrame$RatingG[movieDataFrame$MPAARating == "G"] = 1

# Leading Actor
df$LeadActor <- 0
df$LeadActor[df$MovieTitle == "Trumbo"] = 1
df$LeadActor[df$MovieTitle == "The Martian"] = 1
df$LeadActor[df$MovieTitle == "The Revenant"] = 1
df$LeadActor[df$MovieTitle == "Steve Jobs"] = 1
df$LeadActor[df$MovieTitle == "The Danish Girl"] = 1

# Supporting Actor
df$SupportActor <- 0
df$SupportActor[df$MovieTitle == "The Big Short"] = 1
df$SupportActor[df$MovieTitle == "The Revenant"] = 1
df$SupportActor[df$MovieTitle == "Spotlight"] = 1
df$SupportActor[df$MovieTitle == "Bridge of Spies"] = 1
df$SupportActor[df$MovieTitle == "Creed"] = 1

# Leading Actress
df$LeadActress <- 0
df$LeadActress[df$MovieTitle == "Carol"] = 1
df$LeadActress[df$MovieTitle == "Room"] = 1
df$LeadActress[df$MovieTitle == "Joy"] = 1
df$LeadActress[df$MovieTitle == "45 Years"] = 1
df$LeadActress[df$MovieTitle == "Brooklyn"] = 1

# Supporting Actress
df$SupportActress <- 0
df$SupportActress[df$MovieTitle == "The Hateful Eight"] = 1
df$SupportActress[df$MovieTitle == "Carol"] = 1
df$SupportActress[df$MovieTitle == "Spotlight"] = 1
df$SupportActress[df$MovieTitle == "The Danish Girl"] = 1
df$SupportActress[df$MovieTitle == "Steve Jobs"] = 1

# Animated Feature
df$Animated <- 0
df$Animated[df$MovieTitle == "Anomalisa"] = 1
df$Animated[df$MovieTitle == "Boy & the World"] = 1
df$Animated[df$MovieTitle == "Inside Out"] = 1
df$Animated[df$MovieTitle == "Shaun the Sheep Movie"] = 1
df$Animated[df$MovieTitle == "Omoide no Mânî"] = 1

# Best Picture
df$BestPicture <- 0
df$BestPicture[df$MovieTitle == "The Big Short"] = 1
df$BestPicture[df$MovieTitle == "Bridge of Spies"] = 1
df$BestPicture[df$MovieTitle == "Brooklyn"] = 1
df$BestPicture[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$BestPicture[df$MovieTitle == "The Martian"] = 1
df$BestPicture[df$MovieTitle == "The Revenant"] = 1
df$BestPicture[df$MovieTitle == "Room"] = 1
df$BestPicture[df$MovieTitle == "Spotlight"] = 1

# Cinematography
df$Cinematography <- 0
df$Cinematography[df$MovieTitle == "Carol"] = 1
df$Cinematography[df$MovieTitle == "The Hateful Eight"] = 1
df$Cinematography[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$Cinematography[df$MovieTitle == "The Revenant"] = 1
df$Cinematography[df$MovieTitle == "Sicario"] = 1

# Costume Design
df$Costume <- 0
df$Costume[df$MovieTitle == "Carol"] = 1
df$Costume[df$MovieTitle == "Cinderella"] = 1
df$Costume[df$MovieTitle == "The Danish Girl"] = 1
df$Costume[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$Costume[df$MovieTitle == "The Revenant"] = 1

# Directing
df$Director <- 0
df$Director[df$MovieTitle == "The Big Short"] = 1
df$Director[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$Director[df$MovieTitle == "The Revenant"] = 1
df$Director[df$MovieTitle == "Room"] = 1
df$Director[df$MovieTitle == "Spotlight"] = 1

# Film Editing
df$Editing <- 0
df$Editing[df$MovieTitle == "The Big Short"] = 1
df$Editing[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$Editing[df$MovieTitle == "The Revenant"] = 1
df$Editing[df$MovieTitle == "Spotlight"] = 1
df$Editing[df$MovieTitle == "Star Wars: Episode VII - The Force Awakens"] = 1

# Foreign Language
df$Foreign <- 0
df$Foreign[df$MovieTitle == "Embrace of the Serpent"] = 1
df$Foreign[df$MovieTitle == "Mustang"] = 1
df$Foreign[df$MovieTitle == "Saul fia"] = 1
df$Foreign[df$MovieTitle == "Theeb"] = 1
df$Foreign[df$MovieTitle == "Krigen"] = 1

# Makeup and Hair
df$MakeupHair <- 0
df$MakeupHair[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$MakeupHair[df$MovieTitle == 
                "The 100-Year-Old Man Who Climbed Out the Window and Disappeared"] = 1
df$MakeupHair[df$MovieTitle == "The Revenant"] = 1

# Production Design
df$ProductionDesign <- 0
df$ProductionDesign[df$MovieTitle == "Bridge of Spies"] = 1
df$ProductionDesign[df$MovieTitle == "The Danish Girl"] = 1
df$ProductionDesign[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$ProductionDesign[df$MovieTitle == "The Martian"] = 1
df$ProductionDesign[df$MovieTitle == "The Revenant"] = 1

# Sound Editing
df$SoundEditing <- 0
df$SoundEditing[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$SoundEditing[df$MovieTitle == "The Martian"] = 1
df$SoundEditing[df$MovieTitle == "The Revenant"] = 1
df$SoundEditing[df$MovieTitle == "Sicario"] = 1
df$SoundEditing[df$MovieTitle == "Star Wars: Episode VII - The Force Awakens"] = 1

# Sound Mixing
df$SoundMixing <- 0
df$SoundMixing[df$MovieTitle == "Bridge of Spies"] = 1
df$SoundMixing[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$SoundMixing[df$MovieTitle == "The Martian"] = 1
df$SoundMixing[df$MovieTitle == "The Revenant"] = 1
df$SoundMixing[df$MovieTitle == "Star Wars: Episode VII - The Force Awakens"] = 1

# Visual Effects
df$VisualEffects <- 0
df$VisualEffects[df$MovieTitle == "Ex Machina"] = 1
df$VisualEffects[df$MovieTitle == "Mad Max: Fury Road"] = 1
df$VisualEffects[df$MovieTitle == "The Martian"] = 1
df$VisualEffects[df$MovieTitle == "The Revenant"] = 1
df$VisualEffects[df$MovieTitle == "Star Wars: Episode VII - The Force Awakens"] = 1

# Music
df$Music <- 0
df$Music[df$MovieTitle == "Bridge of Spies"] = 1
df$Music[df$MovieTitle == "Carol"] = 1
df$Music[df$MovieTitle == "The Hateful Eight"] = 1
df$Music[df$MovieTitle == "Sicario"] = 1
df$Music[df$MovieTitle == "Star Wars: Episode VII - The Force Awakens"] = 1

# Original Screenplay
df$Screenplay <- 0
df$Screenplay[df$MovieTitle == "Bridge of Spies"] = 1
df$Screenplay[df$MovieTitle == "Ex Machina"] = 1
df$Screenplay[df$MovieTitle == "Inside Out"] = 1
df$Screenplay[df$MovieTitle == "Spotlight"] = 1
df$Screenplay[df$MovieTitle == "Straight Outta Compton"] = 1

# Documentary (Short)
df$DocShort <- 0
df$DocShort[df$MovieTitle == "Body Team 12"] = 1
df$DocShort[df$MovieTitle == "Chau, beyond the lines"] = 1
df$DocShort[df$MovieTitle == "Claude Lanzmann: Spectres of the Shoah"] = 1
df$DocShort[df$MovieTitle == "A Girl in the River: The Price of Forgiveness"] = 1
df$DocShort[df$MovieTitle == "Last Day of Freedom"] = 1

# Documentary (Feature)
df$Documentary <- 0
df$Documentary[df$MovieTitle == "Amy"] = 1
df$Documentary[df$MovieTitle == "Cartel Land"] = 1
df$Documentary[df$MovieTitle == "The Look of Silence"] = 1
df$Documentary[df$MovieTitle == "What Happened, Miss Simone?"] = 1
df$Documentary[df$MovieTitle == "Winter on Fire: Ukraine's Fight for Freedom"] = 1

# Short (Animated)
df$ShortAnimated <- 0
df$ShortAnimated[df$MovieTitle == "Historia de un oso"] = 1
df$ShortAnimated[df$MovieTitle == "Prologue"] = 1
df$ShortAnimated[df$MovieTitle == "Sanjay's Super Team"] = 1
df$ShortAnimated[df$MovieTitle == "Mi ne mozhem zhit bez kosmosa"] = 1
df$ShortAnimated[df$MovieTitle == "World of Tomorrow"] = 1

# Short (Live Action)
df$ShortLive <- 0
df$ShortLive[df$MovieTitle == "Ave Maria"] = 1
df$ShortLive[df$MovieTitle == "Day One"] = 1
df$ShortLive[df$MovieTitle == "Alles wird gut"] = 1
df$ShortLive[df$MovieTitle == "Shok"] = 1
df$ShortLive[df$MovieTitle == "Stutterer"] = 1

# Song
df$Song <- 0
df$Song[df$MovieTitle == "Fifty Shades of Grey"] = 1
df$Song[df$MovieTitle == "Racing Extinction"] = 1
df$Song[df$MovieTitle == "Youth"] = 1
df$Song[df$MovieTitle == "The Hunting Ground"] = 1
df$Song[df$MovieTitle == "Spectre"] = 1

# Adapted Screenplay
df$Adaptation <- 0
df$Adaptation[df$MovieTitle == "The Big Short"] = 1
df$Adaptation[df$MovieTitle == "Brooklyn"] = 1
df$Adaptation[df$MovieTitle == "Carol"] = 1
df$Adaptation[df$MovieTitle == "The Martian"] = 1
df$Adaptation[df$MovieTitle == "Room"] = 1

n <- names(df[,4:27])

# # Factorize Oscar Categories
# for (i in 1:length(n)) {
#   print(paste("Factorizing:", n[i]))
#   df[ ,n[i]] <- factor(df[ ,n[i]])
# }

# df$MovieTitle[which((df$LeadActor) == 1)]

file1 <- paste0(dataDir, "2016CategoryPreds.txt")
sink(file = file1, append = FALSE)
  for (i in 1:length(n)) {
    print(paste("Oscar Category:", n[i]))
    a <- df$MovieTitle[which(df[ , n[i]] == 1)]
    b <- df$predictions[which(df[ , n[i]] == 1)]
    df2 <- as.data.frame(cbind(a, b))
    names(df2) <- c("Movie Title", "Oscar Prediction")
    print(df2)
    cat("\n")
  }
sink()

