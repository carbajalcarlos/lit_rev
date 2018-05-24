# ----- Initialization of the project -----
# Loading required libraries
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
original <- unlist(strsplit(bg_df$author, split = " and "))
original <- original[!duplicated(original)]
authors <- as.data.frame(original, stringsAsFactors = FALSE)
authors$name <- authors$original
authors$order <- 1:nrow(authors)
authors$length <- nchar(authors$original)
authors$phonetic <- phonetic(authors$original)
# Deduplication using phonetic and string distance measurements
tokens <- as.data.frame(table(authors$phonetic), stringsAsFactors = FALSE)
tokens <- subset(x = tokens, subset = Freq > 1)
tokens <- tokens$Var1
for (i in 1:length(tokens)) {
  #i <- grep(pattern = "juellskielse, g", x = authors$name); i <- grep(pattern = authors$phonetic[i], x = tokens)
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
  index <- which(subset.c$length == max(subset.c$length))[1]
  subset.c$name <- subset.c$name[index]
  subset.b <- rbind(subset.b, subset.c)
  authors <- rbind(subset.a, subset.b)
}

# ----- Manual corrections
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
authors$unique <- !duplicated(authors$name)
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
#authors$abbr <- c(authors$abbr[177:352], authors$abbr[265:352], authors$abbr[265:352])
authors$a.dup <- authors$abbr
#index[authors$unique == FALSE] <- FALSE
index <- duplicated(authors$a.dup) & authors$unique
i<-2
while(sum(index) != 0) {
  authors$a.dup[index] <- paste(authors$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(authors$a.dup) & authors$unique
}
# Replacing non unique authors ID
temp <- which(authors$unique == FALSE)
for (i in temp) {
  index <- grep(pattern = authors$name[i], x = authors$name, fixed = TRUE)
  index <- index[which(authors$unique[index] == TRUE)[1]]
  authors$a.dup[i] <- authors$a.dup[index]
}

# Constructing final structure
authors <- authors[order(authors$order), ]
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
institutions <- as.data.frame(table(original), stringsAsFactors = FALSE)
#index <- grep(pattern = "^,", x = institutions$original)
#institutions$original[index] <- gsub(pattern = "^, *(.*$)", replacement = "\\1", x = institutions$original[index])
# Removing empty entries
index <- grep(pattern = "^$", x = institutions$original)
institutions <- institutions[-index,]

# ----- Extracting affiliation names
institutions$name <- NA
# Extracting university names
index <- grep(pattern = "univ", x = institutions$original)
institutions$name[index] <- trimws(gsub(pattern = ".*?([^,]*(?=univ[ ,]).*)", replacement = "\\1",
                                        x = institutions$original[index], perl = TRUE), which = "both")
# Extracting college names
index <- grepl(pattern = "coll", x = institutions$original)
index <- index & is.na(institutions$name)
institutions$name[index] <- trimws(gsub(pattern = ".*?([^,]*(?=coll[ ,]).*)", replacement = "\\1",
                                        x = institutions$original[index], perl = TRUE), which = "both")
# Extracting museums names
index <- grepl(pattern = "museum", x = institutions$original)
index <- index & is.na(institutions$name)
institutions$name[index] <- gsub(pattern = ".*,(.*museum*$)", replacement = "\\1",
                                 x = institutions$original[index])

# Extracting isolated-by-commas names
index <- grepl(pattern = "^, .*", x = institutions$original)
index <- index & is.na(institutions$name)
institutions$name[index] <- gsub(pattern = "^, (.*$)", replacement = "\\1",
                                 x = institutions$original[index])

# Descarting improbable names
index <- which(is.na(x = institutions$name))
for (i in index) {
  temp <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = institutions$original[i])
  if (nchar(temp)<3) {
    institutions$original[i] <- trimws(gsub(pattern = "^[^,]+,(.*)", replacement = "\\1", x = institutions$original[i]), which = "both")
  }
}
# Comparing with know institutions
pat <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = institutions$name)
index <- which(is.na(x = institutions$name))
for (i in index) {
  temp <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = institutions$original[i])
 if (is.element(el = temp, set = pat)) {
   institutions$name[i] <- institutions$original[i]
 }
}
# Removing not recognized names
index <- which(is.na(x = institutions$name))
institutions <- institutions[-index,]


# Clenaing institutions names
institutions <- institutions[!duplicated(institutions$name), ]
for (i in 1:nrow(institutions)) {
  #i <- grep(pattern = "wireless", x = institutions$name)
  temp <- gsub(pattern = "^([^ ]+) .*", replacement = "\\1", x = institutions$name[i])
  if (nchar(temp)<3) {
    institutions$name[i] <- trimws(gsub(pattern = "^[^ ]+ (.*)", replacement = "\\1", x = institutions$name[i]), which = "both")
  }
}
institutions <- institutions[!duplicated(institutions$name), ]

#save.set <- institutions
#institutions <- save.set

# ----- Extracting additional information
institutions$add.info <- institutions$name

# manual fixes
index <- grep(pattern = "^univ london ", x = institutions$add.info)
institutions$add.info[index] <- gsub(pattern = "univ london", replacement = "univ london,", x = institutions$add.info[index])

# Institution
institutions$inst <- gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = institutions$add.info)
institutions$add.info <- trimws(gsub(pattern = "^[^,]+,(.*)", replacement = "\\1", x = institutions$add.info), which = "both")
# Manual cleaning
index <- grep(pattern = "\\\\&", x = institutions$inst)
institutions$inst[index] <- gsub(pattern = "\\\\&", replacement = "and", x = institutions$inst[index])

# Generating abbreviations
pat <- institutions$inst[!duplicated(institutions$inst)]
tokens <- as.data.frame(pat, stringsAsFactors = FALSE)
tokens$abbr <- tokens$pat
tokens$abbr <- gsub(pattern = " and ", replacement = " & ", x = tokens$abbr)
tokens$abbr <- removeWords(x = tokens$abbr, stopwords(kind = "en"))
tokens$abbr <- gsub(pattern = "[[:space:]]+", replacement = " ", x = tokens$abbr)
for (i in 1:nrow(tokens)) {
  temp <- toupper(unlist(strsplit(tokens$abbr[i], split = " ")))
  if (length(temp) > 1) {
    name <- character()
    for (j in temp) {
      name <- paste(c(name, substr(j, 1, 1)), collapse = "")
    }
  } else {
    name <- substr(j, 1, 1)
  }
  tokens$abbr[i] <- name
}
# Differentiation
#authors$abbr <- c(authors$abbr[1:87], authors$abbr[1:87], authors$abbr[1:174])
tokens$dup <- tokens$abbr
index <- duplicated(tokens$dup)
i<-2
while(sum(index) != 0) {
  tokens$dup[index] <- paste(tokens$abbr[index], i, sep = "_")
  i <- i+1
  index <- duplicated(tokens$dup)
}
# Replacement
institutions$inst.abbr <- NA
for (i in 1:nrow(tokens)) {
  index <- grep(pattern = tokens$pat[i], x = institutions$inst)
  institutions$inst.abbr[index] <- tokens$dup[i]
}

# Country
institutions$country <- NA
index <- grep(pattern = ".*\\.$", x = institutions$add.info)
institutions$country[index] <- trimws(sub(pattern = ".*,([^,]+).$",
                                          replacement = "\\1",
                                          x = institutions$add.info[index]), which = "both")
institutions$add.info <- trimws(sub(pattern = "(.*),[^,]+.$",
                                    replacement = "\\1",
                                    x = institutions$add.info[index]), which = "both")
# Department
institutions$dept <- NA
index <- grep(pattern = "[,]", x = institutions$add.info)
for (i in index) {
  #i <- grep(pattern = "[[:digit:]]+,.*", x = institutions$add.info)[1]
  temp <- trimws(gsub(pattern = "^([^,]+),.*", replacement = "\\1", x = institutions$add.info[i]), which = "both")
  tokens <- gsub(pattern = "\\D", replacement = "", x = temp)
  if (nchar(tokens) == 0) {
    institutions$dept[i] <- temp
    institutions$add.info[i] <- trimws(gsub(pattern = "^[^,]+,(.*)",
                                            replacement = "\\1", x = institutions$add.info[i]), which = "both")
  }
}
# Manual cleaning
index <- grep(pattern = "\\\\&", x = institutions$dept)
institutions$dept[index] <- gsub(pattern = "\\\\&", replacement = "and", x = institutions$dept[index])

# ----- Countries

# Manual cleaning
# USA 
index <- grep(pattern = "usa$", x = institutions$country)
temp <- trimws(gsub(pattern = "(.*)usa$", replacement = "\\1", x = institutions$country[index]), which = "both")
institutions$add.info[index] <- paste(institutions$add.info[index], temp, sep = ", ")
institutions$country[index] <- "united states"
# China
index <- grep(pattern = "china$", x = institutions$country)
institutions$country[index] <- "china"
# United Kingdom
index <- grep(pattern = "scotland$", x = institutions$country)
institutions$add.info[index] <- paste(institutions$add.info[index], "scotland", sep = ", ")
institutions$country[index] <- "united kingdom"
index <- grep(pattern = "england$", x = institutions$country)
institutions$add.info[index] <- paste(institutions$add.info[index], "england", sep = ", ")
institutions$country[index] <- "united kingdom"

# formatting 
institutions$country  <- trimws(tolower(institutions$country), which = "both")
institutions$country <- iconv(institutions$country, from = "UTF-8", to = "ASCII//TRANSLIT")

pat <- trimws(tolower(codelist$country.name.en), which = "both")
pat <- iconv(pat, from = "UTF-8", to = "ASCII//TRANSLIT")
pat <- gsub(pattern = "[[:punct:]]", replacement = "", x = pat)

index <- integer()
for (i in 1:nrow(institutions)) {
  temp <- stringdist(a = institutions$country[i], b = pat, method = "jw")
  if (length(which(temp < 0.10)) == 0) {
    index <- c(index, i)
  }
}
if (length(index) > 0 ) {
  institutions$country[index] <- NA
}

# Complementing information
institutions$continent <- ""
institutions$country.abbr <- ""
institutions$country.num <- 0
tokens <- institutions$country[!duplicated(institutions$country)]
for (i in tokens) {
  index <- which(x = pat == i)[1]
  temp <- grep(pattern = i, x = institutions$country)
  institutions$continent[temp] <- codelist$continent[index]
  institutions$country.abbr[temp] <- codelist$iso3c[index]
  institutions$country.num[temp] <- codelist$iso3n[index]
}

# Constructing final structure
institutions <- institutions[order(institutions$continent, institutions$inst), ]
institutions$local.id <- paste("aff", 1:nrow(institutions), sep = "_")
institutions <- subset(x = institutions, select = c("local.id", "inst", "inst.abbr", "dept", "add.info",
                                                    "continent", "country", "country.abbr", "country.num",
                                                    "name"))
colnames(institutions) <- c("local.id", "name", "abbr", "dept", "add.info",
                            "continent", "country", "country.abbr", "country.num",
                            "original")
rownames(institutions) <- institutions$local.id


# ----- Extracting keywords and research area information -----
# Formatting author keywords
index <- grep(pattern = ";   ", x = bg_df$keywords, fixed = TRUE)
bg_df$keywords[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$keywords[index])
bg_df$keywords <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = bg_df$keywords), which = "both" )
# Counting author keywords
bg_df$keywords.num <- NA
index <- which(is.na(bg_df$keywords) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(bg_df$keywords[i], split = ";"))
  bg_df$keywords.num[i] <- length(temp)
}
# Extracting unique author keywords
kw.author <- trimws(unlist(strsplit(bg_df$keywords, split = ";")), which = "both")
kw.author <- as.data.frame(table(kw.author), stringsAsFactors = FALSE)

# Formating WoS keywords
index <- grep(pattern = ";   ", x = bg_df$keywords.plus, fixed = TRUE)
bg_df$keywords.plus[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$keywords.plus[index])
bg_df$keywords.plus <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = bg_df$keywords.plus), which = "both" )
# Counting wos keywords
bg_df$keywords.plus.num <- NA
index <- which(is.na(bg_df$keywords.plus) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(bg_df$keywords.plus[i], split = ";"))
  bg_df$keywords.plus.num[i] <- length(temp)
}
# Extracting unique wos keywords
kw.wos <- trimws(unlist(strsplit(bg_df$keywords.plus, split = ";")), which = "both")
kw.wos <- as.data.frame(table(kw.wos), stringsAsFactors = FALSE)

# Formating research areas
index <- grep(pattern = ";   ", x = bg_df$research.areas, fixed = TRUE)
bg_df$research.areas[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$research.areas[index])
index <- grep(pattern = "\\\\&", x = bg_df$research.areas)
bg_df$research.areas[index] <- gsub(pattern = "\\\\&", replacement = "and", x = bg_df$research.areas[index])
bg_df$research.areas <- trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = bg_df$research.areas), which = "both" )
# Counting wos keywords
bg_df$research.areas.num <- NA
index <- which(is.na(bg_df$research.areas) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(bg_df$research.areas[i], split = ";"))
  bg_df$research.areas.num[i] <- length(temp)
}
# Extracting unique research areas
ra.wos <- trimws(unlist(strsplit(bg_df$research.areas, split = ";")), which = "both")
ra.wos <- as.data.frame(table(ra.wos), stringsAsFactors = FALSE)

# Formating categories
index <- grep(pattern = ";   ", x = bg_df$web.of.science.categories, fixed = TRUE)
bg_df$web.of.science.categories[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$web.of.science.categories[index])
index <- grep(pattern = "\\\\&", x = bg_df$web.of.science.categories)
bg_df$web.of.science.categories[index] <- gsub(pattern = "\\\\&", replacement = "and", x = bg_df$web.of.science.categories[index])
bg_df$web.of.science.categories <- trimws(gsub(pattern = "[[:space:]]+",
                                               replacement = " ", x = bg_df$web.of.science.categories), which = "both" )
# Counting wos categories
bg_df$wos.categories.num <- NA
index <- which(is.na(bg_df$web.of.science.categories) == FALSE)
for (i in index) {
  temp <- unlist(strsplit(bg_df$web.of.science.categories[i], split = ";"))
  bg_df$wos.categories.num[i] <- length(temp)
}
# Extracting unique wos categories
cat.wos <- trimws(unlist(strsplit(bg_df$web.of.science.categories, split = ";")), which = "both")
cat.wos <- as.data.frame(table(cat.wos), stringsAsFactors = FALSE)

# ----- Standardization of dataset entries -----
# authors name
bg_df$author.full <- NA
bg_df$author.shrt <- NA
bg_df$author.numb <- NA
bg_df$author.fail <- 0
# Routine to find and construct clean authors name
for (i in 1:nrow(bg_df)) {
  #i <- grep(pattern = "juellskielse", x = bg_df$author)
  temp <- trimws(unlist(strsplit(bg_df$author[i], split = "and")), which = "both")
  bg_df$author.numb[i] <- length(temp)
    for (j in temp) {
    index <- grep(pattern = j, x = authors$original, fixed = TRUE)
    if (length(index) != 0) {
      bg_df$author.full[i] <- paste(na.omit(c(bg_df$author.full[i], authors$name[index])), collapse = "; ")
      bg_df$author.shrt[i] <- paste(na.omit(c(bg_df$author.shrt[i], authors$short[index])), collapse = "; ")
    } else {
      bg_df$author.fail[i] <- bg_df$author.fail[i]+1
      bg_df$author.full[i] <- paste(na.omit(c(bg_df$author.full[i], j)), collapse = "; ")
      bg_df$author.shrt[i] <- paste(na.omit(c(bg_df$author.full[i], toupper(j))), collapse = "; ")
    }
    #j<-temp[2]
  }
  # Debugging rutine
  #bg_df$author[i];bg_df$author.fail[i]; bg_df$author.full[i]; bg_df$author.shrt[i];
  #i<-i+1
}

# Article title
#Removing unwanted signs
index <- grep(pattern = ";   ", x = bg_df$title, fixed = TRUE)
bg_df$title[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$title[index])
index <- grep(pattern = ".*\\.$", x = bg_df$title)
bg_df$title[index] <- gsub(pattern = "\\.$", replacement = "", x = bg_df$title[index])
index <- grep(pattern = "r\\&d", x = bg_df$title, fixed = TRUE)
bg_df$title[index] <- gsub(pattern = "r\\\\&d", replacement = "R&D", x = bg_df$title[index])
index <- grep(pattern = "[`{'}]+", x = bg_df$title)
bg_df$title[index] <- gsub(pattern = "[`{'}]", replacement = "", x = bg_df$title[index])
index <- grep(pattern = " - +", x = bg_df$title)
bg_df$title[index] <- gsub(pattern = "- +", replacement = "-", x = bg_df$title[index])
index <- !grepl(pattern = "-and", x = bg_df$title) & grepl(pattern = " +-[[:alpha:]]+", x = bg_df$title)
bg_df$title[index] <- gsub(pattern = "( -)", replacement = ": ", x = bg_df$title[index])
# Capitilising special characters
index <- grep(pattern = "[:] [[:alpha:]]", x = bg_df$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^:]+: ([[:alpha:]]).*", replacement = "\\1", x = bg_df$title[i]))
  bg_df$title[i] <- gsub(pattern = "(^[^;]+: )[[:alpha:]](.*)",
                         replacement = paste(c("\\1", temp, "\\2"), collapse = ""), x = bg_df$title[i])
}
index <- grep(pattern = "[?] [[:alpha:]]", x = bg_df$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^?]+\\? ([[:alpha:]]).*", replacement = "\\1", x = bg_df$title[i]))
  bg_df$title[i] <- gsub(pattern = "(^[^?]+?) *\\? [[:alpha:]](.*)",
                         replacement = paste(c("\\1? ", temp, "\\2"), collapse = ""), x = bg_df$title[i])
}
index <- grep(pattern = "[.] [[:alpha:]]", x = bg_df$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^.]+\\. ([[:alpha:]]).*", replacement = "\\1", x = bg_df$title[i]))
  bg_df$title[i] <- gsub(pattern = "(^[^.]+)\\. [[:alpha:]](.*)",
                         replacement = paste(c("\\1: ", temp, "\\2"), collapse = ""), x = bg_df$title[i])
}
index <- grep(pattern = "[!] [[:alpha:]]", x = bg_df$title)
for (i in index) {
  temp <- toupper(gsub(pattern = "^[^!]+! ([[:alpha:]]).*", replacement = "\\1", x = bg_df$title[i]))
  bg_df$title[i] <- gsub(pattern = "(^[^!]+! )[[:alpha:]](.*)",
                         replacement = paste(c("\\1", temp, "\\2"), collapse = ""), x = bg_df$title[i])
}
# Capitilising first letter
for (i in 1:nrow(bg_df)) {
  bg_df$title[i] <- paste(c(toupper(substr(bg_df$title[i], 1, 1)), substr(bg_df$title[i], 2, nchar(bg_df$title[i]))), collapse = "")
}

#save.set <- bg_df
#bg_df <- save.set

# Editing Journals
# authors name
bg_df$journal.clean <- NA
bg_df$journal.abbr <- NA
bg_df$journal.fail <- 0
# Routine to find and construct clean authors name
tokens <- unique(na.omit(bg_df$journal.iso))
for (i in tokens) {
  index <- which(x = journals$iso == i)
  # Replacing the valid info
  temp <- which(x = bg_df$journal.iso == i)
  if (length(index) != 0) {
    bg_df$journal.clean[temp] <- journals$name[index]
    bg_df$journal.abbr[temp] <- journals$abbr[index]
  } else {
    bg_df$journal.clean[temp] <- bg_df$journal[temp]
    bg_df$journal.fail[temp] <- bg_df$journal.fail[temp]+1
  }
}

# Formating entry type
bg_df$entry.type <- as.factor(bg_df$entry.type)
levels(bg_df$entry.type) <- c("article", "book", "proceeding")
bg_df$entry.type <- as.character(bg_df$entry.type)

# Fails report
temp <- paste(c("Authors formating completed with", 
                sum(bg_df$author.fail != 0), 
                "entries missing"), collapse = " ")
cat(temp)

temp <- paste(c("Journals formating completed with", 
                sum(bg_df$journal.fail != 0), 
                "entries missing"), collapse = " ")
cat(temp)

# Removing and storing information 
rm(bg_list)
rm(subset.a)
rm(subset.b)
rm(subset.c)
rm(bg_raw)
rm(clean)
rm(column)
rm(fields)
rm(i)
rm(j)
rm(index)
rm(name)
rm(original)
rm(pat)
rm(temp)
rm(tokens)

# # Saving the rare.comics subset
save(file = "2_blackbox/1_bg_ds.Rdata", list = "bg_df")
save.image(file = "2_blackbox/ws_cleaing.Rdata")