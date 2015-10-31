# Acquiring, Processing, Describing Text
# Text as Data - day 2
# Fall 2015
# Michele Claibourn


# 1. Acquire state of the union addresses
# Adapted from http://francojc.github.io/web-scraping-with-rvest/

setwd("Box Sync/mpc/DataServices/DS6559/")
# If directory for data doesn't exist, create it
if (!file.exists("sotuext")) {
  dir.create("sotutext")
}
setwd("sotutext")

# Load rvest and set working directory
#install.packages(c("rvest", "magrittr"))
library(rvest)
library(magrittr)

# Load the source page
source.page <- read_html("http://www.presidency.ucsb.edu/sou.php")

# Get link URLs
url1 <- source.page %>% # %>% is the pipe operator, it feeds the result to the next function
  html_nodes(".ver12:nth-child(3) a") %>%  # get the CSS nodes
  html_attr("href") # extract the URLs
url2 <- source.page %>% # again for the 2nd column
  html_nodes(".ver12:nth-child(4) a") %>%  
  html_attr("href") 
url3 <- source.page %>% # and the third column
  html_nodes(".ver12:nth-child(5) a") %>%  
  html_attr("href") 
url4 <- source.page %>% # and the fourth column
  html_nodes(".ver12:nth-child(6) a") %>%  
  html_attr("href") # extract the URLs
urls <- c(url1, url2, url3, url4) # combine them
head(urls)
# Get link text
link1 <- source.page %>% # feed `source.page` to the next step
  html_nodes(".ver12:nth-child(3) a") %>%  # get the CSS nodes
  html_text() # extract the link text
link2 <- source.page %>%
  html_nodes(".ver12:nth-child(4) a") %>%  
  html_text() 
link3 <- source.page %>%
  html_nodes(".ver12:nth-child(5) a") %>%  
  html_text() 
link4 <- source.page %>%
  html_nodes(".ver12:nth-child(6) a") %>%  
  html_text() 
links <- c(link1, link2, link3, link4)
head(links)
# Combine `links` and `urls` into a data.frame
sotu <- data.frame(links=links, urls=urls, stringsAsFactors=FALSE)
head(sotu)

# Subset to include "Modern Presidency" 1934-present
sotu.m <- subset(x=sotu, links %in% 1934:2015) # FDR to Obama
# Append appropriate party label to downloaded file
# Create vector for political party
republicans <- c(1953:1960, 1970:1974, 1974:1977, 1981:1988, 1989:1992, 2001:2008)
# Loop through each link in our sotu data.frame (nrow(sotu.m)) and 
# grab the html (read_html()), isolating node with text (".displaytext") 
# and extracting text (html_text)
for(i in seq(nrow(sotu.m))) {
  text <- read_html(sotu.m$urls[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text

  # Find the political party of this link
  # use republican vector (and ifelse() statement) to determine 
  # if text should be marked Rep or Dem 
  party <- ifelse(test = sotu.m$links[i] %in% republicans,
                  yes = "republican", no = "democrat")

  # Create the file name
  filename <- paste0(party, "-", sotu.m$links[i], ".txt")
  # and send the output to an appropriately named file
  sink(file = filename) %>% # open file to write 
    cat(text)  # write the file
  sink() # close the file
}


# 2. Readability
# For fun, and an opportunity for more exposure to working with R
# A loop, a data frame, some strings, and graphs!

# Readability scores require the text in the original structure
library(koRpus)
# split text up into its semantic tokens and tag with POS  
tagged.obama15 <- tokenize("democrat-2015.txt", lang="en")
str(describe(tagged.obama15))
# apply hyphenation algorithm to estimate syllables
hyph.txt.obama15 <- hyphen(tagged.obama15)
# apply readability algorithm to Obama 2015 sotu 
fk.readbl.obama15 <- readability(tagged.obama15, hyphen=hyph.txt.obama15, index="Flesch.Kincaid")
# view results
fk.readbl.obama15
# find element storing the grade level result
str(fk.readbl.obama15)
fk.readbl.obama15@Flesch.Kincaid$grade

# Now write a loop to do this for all of the years and save in a data frame
sou.files <- DirSource() # list files in working directory, sotutext

# create empty data frame to hold results
sou.grade <- data.frame(speech=character(length(sou.files$filelist)),
                 grade=numeric(length(sou.files$filelist)), stringsAsFactors=FALSE) 

for(i in 1:length(sotu.files$filelist)){
  tagged.sou <- tokenize(sou.files$filelist[i], lang="en")
  hyph.txt.sou <- hyphen(tagged.sou)
  fk.readbl.sou <- readability(tagged.sou, hyphen=hyph.txt.sou, index="Flesch.Kincaid")
  sou.grade$speech[i] <- sou.files$filelist[i]
  sou.grade$grade[i] <- fk.readbl.sou@Flesch.Kincaid$grade
}

# Extract string in speech to create variables for party and year
#install.packages("stringr")
library(stringr)
sou.grade$party <- str_extract(sou.grade$speech, "[a-z]+")
sou.grade$year <- str_extract(sou.grade$speech, "[0-9]+")
# Fix the  formats
sou.grade$party <- as.factor(sou.grade$party)
sou.grade$year <- as.numeric(sou.grade$year)

# And plot!
#install.packages("ggplot2")
library(ggplot2)
p <- ggplot(sou.grade, aes(x = year, y = grade))
p + geom_point()
# Add color for party
p + geom_point(aes(color=party))
p + geom_point() + facet_wrap(~party)

# Which SOTU is the most "readable"?
subset(sou.grade, grade==min(grade))


# 3. ADDED: ngrams
library(tm) # for DirSource function
library(ngram)
setwd("sotutext")
# Read in the republican addresses addresses
rep.files <- DirSource(pattern="republican") # create a list of files to read; only those with republican in filename
rep.text <- rep(NA,length(rep.files$filelist)) # create an empty vector
for(i in 1:length(rep.files$filelist)){
  rep.text[i] <- readLines(rep.files$filelist[i])  
}
repsous <- paste(rep.text)
ngr <- ngram(repsous, n=3)
babble(ngr, 200, seed=012171)


# 4. Create a corpus
#If starting here, set working directory
#setwd("Box Sync/mpc/DataServices/DS6559/sotutext/")
sotu.dir <- getwd() # assign the working directory to an object
dir(sotu.dir) # print contents of directory to screen
sotu.corpus <- Corpus(DirSource(sotu.dir)) # create corpus from files in directory
summary(sotu.corpus)
sotu.corpus[[1]]$meta$id # contains file name party-year.txt
sotu.corpus[[1]]$content # contains text of address


# 5. Save this for later!
save.image("../sotu_1934to2015.RData")
# But not in working directory with text files -- go up one folder
rm(list=ls())

