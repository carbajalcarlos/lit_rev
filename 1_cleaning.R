# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix, quietly = TRUE)
require(stringdist, quietly = TRUE)
require(tm, quietly = TRUE)
require(countrycode, quietly = TRUE)

# Loading data from .bib file
bg_raw <- readFiles("1_data/wos-digital_innovation-2000-2018.bib")

# ----- Parsing the data -----
# Listing individual entries 
index <- c(1, grep(pattern = "^$", x = bg_raw), length(bg_raw)+1)
bg_list <- list()
for (i in 1:(length(index)-1)) {
  temp <- list(x = bg_raw[(index[i]+1):(index[i+1]-1)])
  bg_list <- c(bg_list, temp)
}

# Retrieving all the fields used in the list
fields <- character()
for (i in 1:length(bg_list)) {
  index <- grep(pattern = " = {" , x = bg_list[[i]], fixed = TRUE)
  temp <- bg_list[[i]][index]
  temp <- gsub(pattern = "(^.*) = (.*)",replacement = "\\1", x = temp)
  temp <- tolower(trimws(x = temp, which = "both"))
  fields <- c(fields, temp)
}
fields <- as.data.frame(table(fields), stringsAsFactors = FALSE)
fields <- fields[order(fields$fields, decreasing = FALSE), ]
fields <- c("entry-type", fields$fields)

# Creation of bibliography dataframe
bg_df <- data.frame(matrix(data = NA, ncol = length(fields), nrow = length(bg_list)), 
                    stringsAsFactors = FALSE)
colnames(bg_df) <- make.names(fields)
for (i in 1:length(bg_list)) {
  index <- grep(pattern = " = {" , x = bg_list[[i]], fixed = TRUE)
  fields <- bg_list[[i]][index]
  fields <- gsub(pattern = "(^.*) = (.*)",replacement = "\\1", x = fields)
  fields <- make.names(tolower(trimws(x = fields, which = "both")))
  # Filling individual attributes
  bg_df$entry.type[i] <- gsub(pattern = "^@(.*)\\{(.*)",replacement = "\\1", x = bg_list[[i]][1])
  index <- c(index, length(bg_list[[i]]))
  for (j in 1:length(fields)) {
    pat <- paste(c("^", fields[j], "$"), collapse = "")
    column <- grep(pattern = pat, x = colnames(bg_df))
    clean <- c("abstract", "author")
    if (is.element(el = fields[j], set = clean)) {
      temp <- paste(bg_list[[i]][index[j]:(index[j+1]-1)], collapse = "")
      temp <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temp)
    } else {
      temp <- paste(bg_list[[i]][index[j]:(index[j+1]-1)], collapse = ";")
    }
    if (fields[j] == "author") {
      temp <- gsub(pattern = ".*\\{(.*)\\}.*",replacement = "\\1", x = temp)
    } else  {
      temp <- gsub(pattern = ".*\\{{2}(.*)\\}{2}.*",replacement = "\\1", x = temp)
    }
    bg_df[i,column] <- tolower(temp)
  }
}

# ----- Extraction of unique authors -----
# Organizing by year
bg_df <- bg_df[order(bg_df$year, bg_df$author), ]

# Extracting unique authors
authors <- unlist(strsplit(bg_df$author, split = " and "))
authors <- authors[!duplicated(authors)]
authors <- as.data.frame(authors, stringsAsFactors = FALSE)
colnames(authors) <- "name"
authors$length <- nchar(authors$name)
authors$phonetic <- phonetic(authors$name)
# Deduplication using phonetic and string distance measurements
tokens <- as.data.frame(table(authors$phonetic), stringsAsFactors = FALSE)
tokens <- subset(x = tokens, subset = Freq > 1)
tokens <- tokens$Var1
for (i in 1:length(tokens)) {
  index <- grep(pattern = tokens[i], x = authors$phonetic)
  subset.a <- authors[-index, ]
  subset.b <- authors[index, ]
  temp <- stringdistmatrix(a = subset.b$name, b = subset.b$name, method = "jw")
  temp <- data.frame(which(temp < 0.10, arr.ind = TRUE))
  temp <- subset(x = temp, subset = !(row == col))
  if (nrow(temp) == 0) { next }
  # Reduction to only one value without multiple parameter selection
  subset.c <- subset.b[temp$row, ]
  subset.b <- subset.b[-temp$row, ]
  subset.c <- subset(x = subset.c, subset = length == max(length))
  subset.c <- subset.c[1, ]
  subset.b <- rbind(subset.b, subset.c)
  authors <- rbind(subset.a, subset.b)
}


# ----- Manual corrections
authors$original <- authors$name
# Missing commas
index <- grep(pattern = "[,]", x = authors$name, invert = TRUE)
authors$name[index] <- gsub(pattern = "(^[[:alpha:]]+)(.*$)",
                            replacement = "\\1,\\2", x = authors$name[index])
index <- grep(pattern = "(^.+),(.+,.+$)", x = authors$name)
authors$name[index] <- gsub(pattern = "(^.+),(.+,.+$)",
                            replacement = "\\1\\2", x = authors$name[index])

# abbrviated second names
index <- grep(pattern = "^.*, [[:alpha:]]{2}$", x = authors$name)
if (length(index) != 0) {
  for (i in index) {
    temp <- gsub(pattern = "(^.*, [[:alpha:]]{1})([[:alpha:]]{1}$)",
                 replacement = "\\2", x = authors$name[i])
    temp <- iconv(x = temp, from = "UTF-8", "ASCII//TRANSLIT") # Transliteration of the names
    if (is.element(el = temp, set = c("a", "e", "i", "o", "u")) == FALSE) {
      authors$name[i]<- gsub(pattern = "(^.*, [[:alpha:]]{1})([[:alpha:]]{1}$)",
                                 replacement = "\\1 \\2", x = authors$name[i])
    }
  }
}
# Removing general signs
index <- grep(pattern = "[\\.()]", x = authors$name)
authors$name[index]<- gsub(pattern = "[\\.()]", replacement = "", x = authors$name[index])

# ----- Authors name abbrviation
authors$abbr <-authors$name
for (i in 1:nrow(authors)) {
  #temp <- trimws(unlist(strsplit("carbajal,", split = ",")), which = "both")
  temp <- trimws(unlist(strsplit(authors$abbr[i], split = ",")), which = "both")
  if (length(temp) > 1) {
    tokens <- trimws(unlist(strsplit(temp[2], split = " ")), which = "both")
    name <- paste(c(toupper(temp[1]), ", "), collapse = "")
    for (j in 1:length(tokens)) {
      name <- paste(c(name, toupper(substr(tokens[j], 1, 1))), collapse = "")
    }
    authors$abbr[i] <- name
  } else {
    authors$abbr[i] <- toupper(temp)
  }
}

# Distinguishing authors
#authors$abbr <- c(authors$abbr[1:87], authors$abbr[1:87], authors$abbr[1:174])
authors$a.dup <- authors$abbr
index <- duplicated(authors$a.dup)
i<-2
while(sum(index) != 0) {
  authors$a.dup[index] <- paste(authors$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(authors$a.dup)
}

# Constructing final structure
authors <- authors[order(authors$a.dup), ]
authors$local.id <- paste("a", 1:nrow(authors),sep = "_")
authors <- subset(x = authors, select = c("local.id", "name", "a.dup", "phonetic", "original"))
colnames(authors) <- c("local.id", "name", "short", "phonetic", "original")
rownames(authors) <- authors$local.id

# ----- Extraction of unique journals -----
# Creating unique journals
journals <- subset(x = bg_df, select = c("journal", "journal.iso"))
colnames(journals) <- c("original", "iso")
journals <- journals[!is.na(journals$original), ]
journals <- journals[!duplicated(journals$original), ]

# Cleaning names
journals$name <- journals$original
index <- grep(pattern = "\\&", x = journals$name)
journals$name[index] <- gsub(pattern = "\\\\&", replacement = "and", journals$name[index])
index <- grep(pattern = "[-]", x = journals$name)
journals$name[index] <- gsub(pattern = "[-]", replacement = " ", journals$name[index])
# manual changes
index <- grep(pattern = "bmj", x = journals$name)
journals$name[index] <- gsub(pattern = "bmj", replacement = "", journals$name[index])
index <- grep(pattern = "(^| )it ", x = journals$name)
journals$name[index] <- gsub(pattern = "it", replacement = "I T", journals$name[index])
index <- grep(pattern = "mis", x = journals$name)
journals$name[index] <- gsub(pattern = "mis", replacement = "M I S", journals$name[index])

# Triming and removing double spaces
journals$name <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = journals$name), which = "both")

# abbrviation of journals names
journals$abbr <- journals$name
journals$abbr <- removeWords(x = journals$abbr, stopwords(kind = "en"))
journals$abbr <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = journals$abbr))
for (i in 1:nrow(journals)) {
  #i <- grep(pattern = "ambio", x = journals$name)
  temp <- unlist(strsplit(x = journals$abbr[i], split = " "))
  name <- character()
  if (length(temp) > 1) {
    for (j in 1:length(temp)) {
      name <- paste(c(name, toupper(substr(temp[j], 1, 1))), collapse = "")
    }
  } else if (nchar(temp) > 1) {
    name <- paste(c(toupper(substr(temp, 1, 1)), tolower(substr(temp, 2, 3))), collapse = "")
  } else {
    name <- toupper(substr(temp, 1, 1))
  }
  journals$abbr[i] <- name
}

# Distinguishing Journals
journals$a.dup <- journals$abbr
index <- duplicated(journals$a.dup)
i<-2
while(sum(index) != 0) {
  journals$a.dup[index] <- paste(journals$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(journals$a.dup)
}

# Constructing final structure
journals <- journals[order(journals$a.dup), ]
journals$local.id <- paste("j", 1:nrow(journals),sep = "_")
journals <- subset(x = journals, select = c("local.id", "name", "a.dup", "iso", "original"))
colnames(journals) <- c("local.id", "name", "abbr", "iso", "original")
rownames(journals) <- journals$local.id

# ----- Extraction affiliation information -----
original <- unlist(strsplit(bg_df$affiliation, split = ";"))
# Removing unwanted information
index <- grep(pattern = "(reprint author)", x = original)
original <- gsub(pattern = " \\(reprint author\\)", replacement = "", x = original)
index <- grep(pattern = "[[:space:]]+", x = original)
original <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = original), which = "both")
# Removing names in the list
for (i in 1:nrow(authors)) {
  #i <- grep(pattern = "zhiqiang", x = authors$original)
  #index <- grep(pattern = "zhiqiang", x = original)
  index <- grep(pattern = authors$original[i], x = original, fixed = TRUE)
  original[index] <- gsub(pattern = authors$original[i], replacement = "",
                          x = original[index], fixed = TRUE)
  index <- grep(pattern = tolower(authors$short[i]), x = original, fixed = )
  original[index] <- gsub(pattern = tolower(authors$short[i]), replacement = "",
                          x = original[index], fixed = TRUE)
}
# Removing empty symbols
universities <- as.data.frame(table(original), stringsAsFactors = FALSE)
#index <- grep(pattern = "^,", x = universities$original)
#universities$original[index] <- gsub(pattern = "^, *(.*$)", replacement = "\\1", x = universities$original[index])
# Removing empty entries
index <- grep(pattern = "^$", x = universities$original)
universities <- universities[-index,]

# ----- Extracting affiliation names
universities$name <- NA
# Extracting university names
index <- grep(pattern = "univ", x = universities$original)
universities$name[index] <- trimws(gsub(pattern = ".*?([^,]*(?=univ[ ,]).*)", replacement = "\\1",
                                        x = universities$original[index], perl = TRUE), which = "both")
# Extracting college names
index <- grepl(pattern = "coll", x = universities$original)
index <- index & is.na(universities$name)
universities$name[index] <- trimws(gsub(pattern = ".*?([^,]*(?=coll[ ,]).*)", replacement = "\\1",
                                        x = universities$original[index], perl = TRUE), which = "both")
# Extracting museums names
index <- grepl(pattern = "museum", x = universities$original)
index <- index & is.na(universities$name)
universities$name[index] <- gsub(pattern = ".*,(.*museum*$)", replacement = "\\1",
                                 x = universities$original[index])

# Extracting isolated-by-commas names
index <- grepl(pattern = "^, .*", x = universities$original)
index <- index & is.na(universities$name)
universities$name[index] <- gsub(pattern = "^, (.*$)", replacement = "\\1",
                                 x = universities$original[index])

# Descarting improbable names
index <- which(is.na(x = universities$name))
for (i in index) {
  temp <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = universities$original[i])
  if (nchar(temp)<3) {
    universities$original[i] <- trimws(gsub(pattern = "^[^,]+,(.*)", replacement = "\\1", x = universities$original[i]), which = "both")
  }
}
# Comparing with know institutions
pat <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = universities$name)
index <- which(is.na(x = universities$name))
for (i in index) {
  temp <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = universities$original[i])
 if (is.element(el = temp, set = pat)) {
   universities$name[i] <- universities$original[i]
 }
}
# Removing not recognized names
index <- which(is.na(x = universities$name))
universities <- universities[-index,]


# Clenaing institutions names
universities <- universities[!duplicated(universities$name), ]
for (i in 1:nrow(universities)) {
  #i <- grep(pattern = "wireless", x = universities$name)
  temp <- gsub(pattern = "^([^ ]+) .*", replacement = "\\1", x = universities$name[i])
  if (nchar(temp)<3) {
    universities$name[i] <- trimws(gsub(pattern = "^[^ ]+ (.*)", replacement = "\\1", x = universities$name[i]), which = "both")
  }
}
universities <- universities[!duplicated(universities$name), ]

#save.set <- universities
#universities <- save.set

# ----- Extracting additional information
universities$add.info <- universities$name
# Institution
universities$inst <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = universities$add.info)
universities$add.info <- trimws(gsub(pattern = "^[^,]+,(.*)", replacement = "\\1", x = universities$add.info), which = "both")
# Country
universities$country <- NA
index <- grep(pattern = ".*\\.$", x = universities$add.info)
universities$country[index] <- trimws(sub(pattern = ".*,([^,]+).$",
                                          replacement = "\\1",
                                          x = universities$add.info[index]), which = "both")
universities$add.info <- trimws(sub(pattern = "(.*),[^,]+.$",
                                    replacement = "\\1",
                                    x = universities$add.info[index]), which = "both")
# Department
universities$dept <- NA
index <- grep(pattern = "[,]", x = universities$add.info)
for (i in index) {
  #i <- grep(pattern = "[[:digit:]]+,.*", x = universities$add.info)[1]
  temp <- trimws(gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = universities$add.info[i]), which = "both")
  tokens <- gsub(pattern = "\\D", replacement = "", x = temp)
  if (nchar(tokens) == 0) {
    universities$dept[i] <- temp
    universities$add.info[i] <- trimws(gsub(pattern = "^[^,]+,(.*)",
                                            replacement = "\\1", x = universities$add.info[i]), which = "both")
  }
}


# additional info
universities$country <-
universities$add.info <- ""


universities$add.info <- ""
for (i in 1:nrow(universities)) {
  temp <- gsub(pattern = "[^,]", replacement = "", x = universities$sub.inst[i])
}


temp <- trimws(gsub(pattern = "[^,.]", replacement = "", x = universities$add.info), which = "both")
pat <- as.data.frame(x = table(temp), stringsAsFactors = FALSE)
index <- which(x = temp == "")

View(universities$add.info[index])


# Extracting known intitution names 
temp <- trimws(sub(pattern = "(^ *[[:alnum:]]+)( |,).*$", replacement = "\\1", na.omit(universities$name)), which = "both" )
temp <- temp[!duplicated(temp)]
for (i in 1:length(temp)) {
  #i <- grep(pattern = "kpmg", x = temp)
  index <- grepl(pattern = temp[i], x = universities$original)
  index <- index & is.na(universities$name)
  pat <- paste(c(".*(", temp[i], ".*$)"), collapse = "")
  universities$name[index] <- gsub(pattern = pat, replacement = "\\1",
                                   x = universities$original[index])
}

# Cleaning institute names
universities$add.info

temp <- gsub(pattern = "^(.*?),(.*)$", replacement = "\\1", x = universities$name)
temp <- gsub(pattern = "[[:alpha:]]+", replacement = "A", x = temp)
temp <- as.data.frame(x = table(temp), stringsAsFactors = FALSE)  



# ----- Extracting countries
original <- gsub(pattern = ".*(,|[0-9]+)([^,]+).$", replacement = "\\2", x = universities$original)
original <- trimws(original, which = "both")
countries <- as.data.frame(table(original), stringsAsFactors = FALSE)
countries$name <- countries$original
# Manual cleaning
index <- grep(pattern = "usa$", x = countries$name)
countries$name[index] <- "united states"
index <- grep(pattern = "scotland$", x = countries$name)
countries$name[index] <- "United Kingdom"
index <- grep(pattern = "england$", x = countries$name)
countries$name[index] <- "United Kingdom"
index <- grep(pattern = "china$", x = countries$name)
countries$name[index] <- "china"
# cleaning 
countries$name <- trimws(tolower(countries$name), which = "both")
countries$name <- iconv(countries$name, from = "UTF-8", to = "ASCII//TRANSLIT")

pat <- trimws(tolower(countrycode::codelist$country.name.en), which = "both")
pat <- iconv(pat, from = "UTF-8", to = "ASCII//TRANSLIT")
pat <- gsub(pattern = "[[:punct:]]", replacement = "", x = pat)

index <- integer()
for (i in 1:nrow(countries)) {
  temp <- stringdist(a = countries$name[i], b = pat, method = "jw")
  if (length(which(temp < 0.10)) == 0) {
    index <- c(index, i)
  }
}
if (length(index) > 0 ) {
  countries <- countries[-index, ]
}

# Complementing information
countries$continent <- ""
countries$iso <- ""
countries$num <- 0
for (i in 1:nrow(countries)) {
  index <- which(x = pat == countries$name[i])[1]
  countries$continent[i] <- codelist$continent[index]
  countries$iso[i] <- codelist$iso3c[index]
  countries$num[i] <- codelist$iso3n[index]
}
# Constructing final structure
countries <- countries[order(countries$iso), ]
countries$local.id <- paste("c", 1:nrow(countries),sep = "_")
countries <- subset(x = countries, select = c("local.id", "name", "iso", "num", "continent", "original"))
colnames(countries) <- c("local.id", "name", "abbr", "iso_num", "continent", "original")
rownames(countries) <- countries$local.id

  

# ----- Reconstructing of the bibliography information -----
# Entry type
bg_df$entry.type <- as.factor(bg_df$entry.type)
levels(bg_df$entry.type) <- c("article", "book", "proceeding")
