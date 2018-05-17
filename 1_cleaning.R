# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix, quietly = TRUE)
require(stringdist, quietly = TRUE)

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


# ----- Manual corrections -----
authors$original <- authors$name
# Missing commas
index <- grep(pattern = "[,]", x = authors$name, invert = TRUE)
authors$name[index] <- gsub(pattern = "(^[[:alpha:]]+)(.*$)",
                            replacement = "\\1,\\2", x = authors$name[index])
index <- grep(pattern = "(^.+),(.+,.+$)", x = authors$name)
authors$name[index] <- gsub(pattern = "(^.+),(.+,.+$)",
                            replacement = "\\1\\2", x = authors$name[index])

# Abbreviated second names
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

# ----- Authors name abbreviation -----
authors$abbre <-authors$name
# Removing general signs
index <- grep(pattern = "[\\.()]", x = authors$abbre)
authors$abbre[index]<- gsub(pattern = "[\\.()]", replacement = "", x = authors$abbre[index])
for (i in 1:nrow(authors)) {
  temp <- trimws(unlist(strsplit(authors$abbre[i], split = ",")), which = "both")
  temp2 <- trimws(unlist(strsplit(temp[2], split = " ")), which = "both")
  name <- paste(c(toupper(temp[1]), ", "), collapse = "")
  if (length(temp2) != 0) {
    for (j in 1:length(temp2)) {
      name <- paste(c(name, toupper(substr(temp2[j], 1, 1))), collapse = "")
    }
  }
  authors$abbre[i] <- name
}

# Distinguishing authors
#authors$abbre <- c(authors$abbre[1:87], authors$abbre[1:87], authors$abbre[1:174])
authors$a.dup <- authors$abbre
index <- duplicated(authors$a.dup)
i<-2
while(sum(index) != 0) {
  authors$a.dup[index] <- paste(authors$abbre[index], i, sep = "_")
  i <- i+1
  index <- duplicated(authors$a.dup)
}

# Entry type
bg_df$entry.type <- as.factor(bg_df$entry.type)
levels(bg_df$entry.type) <- c("article", "book", "proceeding")
