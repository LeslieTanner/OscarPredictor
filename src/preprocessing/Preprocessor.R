# This code takes as input the dataframes mainPage, and movieDataFrame, and the list actorData, and
# does the following:
# -incorporate 1 column of film counts and 1 column of TV counts for each of the top 3 stars.
# -convert foreign currencies to USD
# -convert USD to inflation-adjusted 2015 USD
# -add 0-1 columns for specific factor values (genre).
# -add interatction term columns for factors with few levels
# -replace missing values of regressors with estimates where appropriate
# -replace missing values of regressors with NA strings where appropriate (i.e. for MPAA rating)
# -reduce movie data to complete cases


