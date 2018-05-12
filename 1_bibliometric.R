# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix)
require(stringdist)

# Importing data from WoS-export
bm <- readFiles("0_data/wos-digital_innovation-2000-2018.bib")
bm <- convert2df(bm, dbsource = "isi", format = "bibtex")

# Data preparation 
rownames(bm) <- 1:nrow(bm)

# Sampling
if (F) {
  working.set <- bm[sample(x = nrow(bm), size = ceiling(nrow(bm)/5)), ]
} else{
  working.set <- bm
}

working.set$C1[161] <- gsub(pattern = ".,", replacement = ",;", x = working.set$C1[161], fixed = TRUE)

# ----- Extracting full data -----
working.set$aut_num <- 1
working.set$aut <- working.set$AU
working.set$aut_ful <- ""
working.set$aut_com <- 0

for (i in 1:nrow(working.set)) {
  names <- character()
  if (is.na(working.set$C1[i]) == F) {
    info <- unlist(strsplit(working.set$C1[i], ';'))
    for (j in 2:length(info)) {
      corte <- unlist(strsplit(x = info[j], split = ','))
      corte <- trimws(x = corte, which = 'both')
      corte <- paste(corte[1:2], collapse = ", ")
      names <- c(names, corte)
    }
  } else {
    info <- unlist(strsplit(working.set$AU[i], ';'))
    for (j in 1:length(info)) {
      corte <- unlist(strsplit(x = info[j], split = ' '))
      corte <- trimws(x = corte, which = 'both')
      corte <- paste(corte[1:2], collapse = ", ")
      names <- c(names, corte)
    }
  }
  working.set$aut_num[i] <- length(names)
  working.set$aut_ful[i] <- paste(names, collapse = "; ")
  working.set$aut_com[i] <- stringdist(a = working.set$AU[i], b = working.set$aut_ful[i], method = "jw")
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
