

## Miles Davis on allmusic.com
## ("http://www.allmusic.com/artist/miles-davis-mn0000423829/discography")

library(httr)
library(XML)
url <- 'http://www.allmusic.com/artist/miles-davis-mn0000423829/discography'
rawpage <- GET(url)
cleanpage <- htmlParse(rawpage)


##  1)  Extract informations about: year, title, label
##########
year <- xpathSApply(cleanpage, "//td[@class='year']", xmlValue)
year
title <- xpathSApply(cleanpage, "//td[@class='title']", xmlValue)
title
label <- xpathSApply(cleanpage, "//td[@class='label']", xmlValue)
label

## OPTIONAL: tables can be extracted with readHTMLTable
table <- readHTMLTable(cleanpage)
str(table)
table <- table[[1]] # Why do this? 
table[,3:5]

##  2) Time to cleanup!

## Remove \n in year
year <- gsub(pattern = "\\n", replacement = "", x = year)
#or without naming the arguments
year <- gsub("\\n", "", year)
year
## Remove extra spaces
year <- gsub(pattern= " ", replacement = "", x = year) 
year
year <- gsub("^\\s+|\\s+$", "", year)  
year
year <- as.numeric(year)
year

## Remove \n in title
title <- gsub("\\n", "", title)  
title

## Remove extra spaces in title
# Attention: We do not want to remove all spaces
title <- gsub("^\\s+|\\s+$", "", title) 
title
## Remove \n in label
label <- gsub("\\n", "",label)
label
  
## Remove extra spaces in label
label <- gsub("^\\s+|\\s+$","",label)
label
  
  # copy and pasting is tedious - try to make it into a function!
  clean_string <- function(unclean_vector){
  res <- gsub("\\n","",unclean_vector)
  res <- gsub("^\\s+|\\s+$","", res)
      return(res)
  }

year <- xpathSApply(cleanpage, "//td[@class='year']", xmlValue)
year <- clean_string(unclean_vector = year)
year

## 3) Store them in a dataframe (difference with a matrix?)
miles <- data.frame(year, title, label, stringsAsFactors = F)
head(miles)

## 4) Only preserve albums signed with Prestige
## Using grep, write a function that selects the right label
grep("Prestige", miles$label)    

## Using indexing, create a new dataset
df_prestige <- miles[grep("Prestige",miles$label), ]
df_prestige

## 5) Keep titles that have Miles/miles in it
## Using grep, write a function that selects the right title
grep("[Mm]iles", miles$title)    
## Using indexing, create a new dataset
dat <- miles[grep("[Mm]iles", miles$title), ] 
dat
dim(dat)


## 6) Extract what comes after Miles in the title
gsub("^(.*[Mm]iles) (.*)", "\\2", dat$title)

## 6) Extract the first words that comes after Miles in the title "?"为了限定数量
gsub("^(.*[Mm]iles)(\\s?)(\\w+|&).*","\\3",dat$title)

gsub("^(.*[Mm]iles)(\\s?)([a-zA-Z0-9&]+).*", "\\3", dat$title)
## Extract the first letter that comes after Miles in the title
gsub("^(.*[Mm]iles)(\\s?)(\\w+?|&).*","\\3",dat$title)


## 7) Extract what comes before Miles in the title
gsub("^(.*[Mm]iles) (.*)", "\\1", dat$title)
gsub("^(.*[Mm]iles)(\\s?)(\\w+|&).*","\\1",dat$title)



##### Back to the Movie Director names. 
##From day 4, download the file with the name of move directors
##Separate first and last name
##Using the babynames packages, find for each =year of birth of the director= the gender of their first name, replace it
##Give the proportion of male in the top 250. In the top 10. 
##Give the probability of being male in the top 250. In the top 10. 




########################### THE END ##################################