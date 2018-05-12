# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix)
require(stringdist)

# Importing data from WoS-export
bm <- readFiles("0_data/wos-digital_innovation-2000-2018.bib")
bm <- convert2df(bm, dbsource = "isi", format = "bibtex")

# Sampling
if (F) {
  working.set <- bm[sample(x = nrow(bm), size = ceiling(nrow(bm)/10)), ]
} else{
  working.set <- bm
}

rownames(working.set) <- 1:nrow(working.set)

# Extracting full name
working.set$aut_num <- 1
working.set$aut_com <- ""

for (i in 1:nrow(working.set)) {
  authors <- unlist(strsplit(working.set$AU[i], ';'))
  working.set$aut_num[i] <- length(authors)
  for (j in 1:length(authors)) {
    names <- unlist(strsplit(x = authors[j], split = ' '))
    names[2] <- substr(names[2], 1, 1)
    patron <- paste(c(".*", names[1], "[,]* (", names[2], "[[:alpha:]]*[.]*[[:space:]]*[[:alpha:]]*[.]*[[:space:]]*[[:alpha:]]*)(.*)"), collapse = "")
    if (grepl(pattern = patron, x = working.set$C1[i]) == TRUE) {
      patron <- gsub(pattern = patron, replacement = "\\1", x = working.set$C1[i])
      patron <- paste(unlist(strsplit(x = patron, split = '[[:punct:]]')), collapse = "")
      names[2] <- trimws(x = patron, which = "both")
    }
    working.set$aut_com[i] <- paste(c(working.set$aut_com[i], names[1], ", ", names[2], "; "), collapse = "")
  }
  working.set$aut_com[i] <- gsub(pattern = "(.*); $", replacement = "\\1", x = working.set$aut_com[i])
}

# ----- duplicates -----
for (i in 1:length(bm)) {
  if (class(bm[ ,i]) == "character") {
    bm[ ,i] <- tolower(bm[ ,i])
  }
}

names <- unlist(strsplit(bm$AU, ';'))# Create a list of words
df.authors <- data.frame(table(names)) # Store the list as a data frame

for (i in nrow(df.authors)) {
  prueba <- stringdist(a = df.authors$names[i], b = df.authors$names, method = 'cosine', q = 3)
  prueba <- data.frame(which(x = prueba < 0.25, arr.ind = TRUE))
}


comparison <- stringdistmatrix(a = df.wordList$wordList, b = df.wordList$wordList, method = 'cosine', q = 3)
comparison <- data.frame(which(x = comparison < 0.25, arr.ind = TRUE))
index <- duplicated(x = comparison$row)
index <- comparison$row[index]
index <- comparison$row %in% index

comparison2 <- comparison[index, ]
comparison2 <- comparison2[order(comparison2$row), ]
comparison2$row <- df.wordList$wordList[comparison2$row]
comparison2$col <- df.wordList$wordList[comparison2$col]


# Blibliometric analysis
bm_analysis <- biblioAnalysis(bm, sep = ";")
bm_summary <- summary(object = bm_analysis, k = 10, pause = FALSE)
plot(x = bm_analysis, k = 10, pause = TRUE)

# References analysis
