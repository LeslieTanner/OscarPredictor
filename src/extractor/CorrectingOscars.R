###################
# Correct Oscar Details

# Take in "movieData.RData" file and then correct how many oscars by going to awards page of movie URL

# Return "movieCorrectOscar.RData"  _ I had a method 2, but that one is incorrect.

library(rvest)
library(stringr)
# library(xml2)

setwd("C:/Users/Leslie Tanner/Documents/STAT6021/OscarProject/OscarPredictor/data/")

dataDir <- "~/STAT6021/OscarProject/OscarPredictor/data/"
load(file=paste0(dataDir,"movieData.RData"))

# Look at this after loading Movie data frame.  
barplot(table(movieDataFrame$NumberOfOscars))
a <- aggregate(NumberOfOscars ~ ReleaseYear, data = movieDataFrame, sum)
barplot(a$NumberOfOscars)
a
# Right now, this shows 109 Oscar Wins in 2011.  This is wrong.  should be around 24

urlsWithOscars <- which(movieDataFrame$NumberOfOscars > 0)

movieDataFrame$CorrectOscar <- 0

for (i in urlsWithOscars) {
  tryCatch({  
    # Method 1:
    url <- movieDataFrame$MovieURL[i]
    source.page <- read_html(url)
    
    numOscarWins <- source.page %>% 
      html_nodes("#titleAwardsRanks b") %>%
      html_text()
    
    if (((str_detect(as.character(numOscarWins), "Oscar") | 
          (str_detect(as.character(numOscarWins), "Oscars"))) & 
         (str_detect(as.character(numOscarWins), "Won")))) 
    {
      num <- str_extract(as.character(numOscarWins), "[0-9]+")
    } else {
      num <- "0"
    }
    movieDataFrame$CorrectOscar[i] <- as.integer(num)  
    
  }, error=function(e){})
  print(i)
}

# Look at this after loading Movie data frame.  
barplot(table(movieDataFrame$CorrectOscar))
a <- aggregate(CorrectOscar ~ ReleaseYear, data = movieDataFrame, sum)
barplot(a$CorrectOscar)
a

# movieDataFrame$MovieURL[which(movieDataFrame$CorrectOscar > 0 & movieDataFrame$ReleaseYear == 2012)]

save(list = ls(all.names = TRUE), file="movieCorrectOscar.RData")

# Write to CSV file:
write.csv(movieDataFrame, file = "movieCorrectOscar.csv", row.names = FALSE)





# ######################################
# #Method 2
# 
# movieDataFrame$CorrectOscar <- 0
# 
# for (i in 1:nrow(movieDataFrame)) {
#   
#     if (movieDataFrame$NumberOfOscars[i] > 0) {
# 
#     # Method 2:    
#     url <- paste0(movieDataFrame$MovieURL[i], "awards?ref_=tt_awd")
#     source.page <- read_html(url)
#     
#     numOscarWins <- source.page %>% 
#       html_nodes(".awards:nth-child(5) .award_description , .awards:nth-child(5) b") %>%  
#       html_text()
#     
#     if (numOscarWins[1] != "Won") {
#       numOscarWins <- 0
#     } else {
#       if (any(numOscarWins == "Nominated")) {
#         numOscarWins <- (which(numOscarWins == "Nominated") - 1) - 1
#       } else {
#         numOscarWins <- length(numOscarWins) - 1
#       }
#     }
#     
#     movieDataFrame$CorrectOscar[i] <- numOscarWins
#   
#   } else {
# 
#     movieDataFrame$CorrectOscar[i] <- 0
#     
#   }
#   print(i)
# }
# 
# 
# # Look at this after loading Movie data frame.  
# barplot(table(movieDataFrame$CorrectOscar))
# a <- aggregate(CorrectOscar ~ ReleaseYear, data = movieDataFrame, sum)
# barplot(a$CorrectOscar)
# a
# 
# movieDataFrame$MovieURL[which(movieDataFrame$CorrectOscar > 0 & movieDataFrame$ReleaseYear == 2012)]
# 
# save(list = ls(all.names = TRUE), file="movieOscarCorrect_method2.RData")



