# Establishing the working directory
setwd("D:/Code/R/Cleaning")

# Call libraries
#library(reshape)
#library(lubridate)
#library(plyr)
#library(stringr)

# Opening raw data files
load("raw/rawComics.Rdata")
#sales$X <- NULL

# Extraction of the titles column
titles <- as.character(comics$Issues_Title)

# Replacement of the extended latin alphabet
Encoding(titles) <- "latin1" # Encoding the list as latin alphabet
titles <- iconv(titles, "latin1", "ASCII//TRANSLIT") # Transliteration of the names

# Basic standarization of the entries
titles <- gsub(pattern = "^[[:space:]]+|[[:space:]]+$", replacement = "", x = titles) # Eliminate all the leading and trailing spaces 
titles <- gsub(pattern = "[[:space:]]+", replacement = " ", x = titles) # Eliminate all the duplicated spaces
titles <- gsub("[[:cntrl:]]", " ", titles)# Remove control characters
titles <- tolower(titles) # Change all the strings to lower-case

### Specific cleaning routines for: signs
titles <- gsub(pattern = " \\& ", replacement = " and ", x = titles)
titles <- gsub(pattern = "\\&", replacement = " and ", x = titles)
titles <- gsub(pattern = "([[:digit:]]+|[[:digit:]]+ )(-)([[:digit:]]+)", replacement = "\\1to\\3", x = titles)
titles <- gsub(pattern = "\\'97", replacement = "1997", x = titles)
titles <- gsub(pattern = "\\%", replacement = " porcent", x = titles)
titles <- gsub(pattern = "([[:digit:]]*)(\\.)([[:digit:]])", replacement = "\\1 \\3", x = titles)
titles <- gsub(pattern = " 1\\/2", replacement = " half", x = titles)
titles <- gsub(pattern = "w\\/", replacement = "with", x = titles)
titles <- gsub(pattern = "\\/", replacement = " ", x = titles)
titles <- gsub(pattern = "\\@", replacement = "at", x = titles)

### Specific cleaning routines for: VOLUME
titles <- gsub(pattern = "vol\\.ume", replacement = "VOL", x = titles) # Optional - 65 hits
titles <- gsub(pattern = "([[:space:]]+)(vol|volume)([[:space:]]+|$)", replacement = " VOL ", x = titles) # extends vol word to volume
titles <- gsub(pattern = "( |\\()(vol\\.)", replacement = " VOL", x = titles) # extends vol. to volume
titles <- gsub(pattern = "( VOL)([[:digit:]]|a)", replacement = " VOL \\2", x = titles) # inserts a space in vol[alnum] 

### Specific cleaning routines for: comics abbreviations
titles <- gsub(pattern = "(\\()(o\\/a|oa)(\\))", replacement = "OA", x = titles)
titles <- gsub(pattern = "(\\#)([[:digit:]]+)", replacement = "NUM \\2", x = titles)
titles <- gsub(pattern = "([[:space:]]+|\\()(cvr|cover|covers)([[:space:]]+|\\,|\\)|$)", replacement = " CVR ", x = titles)
titles <- gsub(pattern = "([[:space:]]+)(ed|edition)([[:space:]]+|\\.|\\,|\\)|$)", replacement = " ED ", x = titles)
titles <- gsub(pattern = "([[:space:]]+)(hc|hca|hcc|hardcover)([[:space:]]+|$)", replacement = " HC ", x = titles) # replace hc, hca, hcc for hardcover 
titles <- gsub(pattern = "([[:space:]]+)(tp|tpb)([[:space:]]+|$)", replacement = " TPB ", x = titles)
titles <- gsub(pattern = "(\\()(res|resolication|resolicitation)(\\))|\\*", replacement = " RES ", x = titles)

#Erasing aditional spaces created
titles <- gsub(pattern = "[[:punct:]]", replacement = "", x = titles)
titles <- gsub(pattern = "^[[:space:]]+|[[:space:]]+$", replacement = "", x = titles) # Eliminate all the leading and trailing spaces 
titles <- gsub(pattern = "[[:space:]]+", replacement = " ", x = titles) # Eliminate all the duplicated spaces

comics$Issues_Title <- titles

# Output visualizations
wordList <- unlist(strsplit(titles, " "))# Create a list of words
df.wordList2 <- data.frame(table(wordList)) # Store the list as a data frame

rm(titles)
rm(wordList)
