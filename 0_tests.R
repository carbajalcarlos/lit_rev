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

# ----- Formating attributes -----
# Organizing by year
bg_df <- bg_df[order(bg_df$year, bg_df$author), ]

# Extracting unique authors
author <- unlist(strsplit(bg_df$author, split = " and "))
author <- author[!duplicated(author)]
author <- as.data.frame(author, stringsAsFactors = FALSE)
author$length <- nchar(author$author)
author$phonetic <- phonetic(author$author)

tokens <- as.data.frame(table(author$phonetic), stringsAsFactors = FALSE)
tokens <- subset(x = tokens, subset = Freq > 1)
tokens <- tokens$Var1
for (i in 1:length(tokens)) {
  index <- grep(pattern = tokens[i], x = author$phonetic)
  subset.a <- author[-index, ]
  subset.b <- author[index, ]
  temp <- stringdistmatrix(a = subset.b$author, b = subset.b$author, method = "jw");
  temp <- data.frame(which(temp < 0.10, arr.ind = TRUE))
  temp <- subset(x = temp, subset = !(row == col))
  if (nrow(temp) == 0) { next }
  subset.c <- subset.b[temp$row, ]
  subset.b <- subset.b[-temp$row, ]
  subset.c <- subset(x = subset.c, subset = length == max(length))
  subset.c <- subset.c[1, ]
  subset.b <- rbind(subset.b, subset.c)
  author <- rbind(subset.a, subset.b)
}

# Entry type
bg_df$entry.type <- as.factor(bg_df$entry.type)
levels(bg_df$entry.type) <- c("article", "book", "proceeding")
