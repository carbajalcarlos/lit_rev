# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix, quietly = TRUE)
require(stringdist, quietly = TRUE)

# Importing data from WoS-export
bm <- readFiles("0_data/wos-digital_innovation-2000-2018.bib")
bm <- convert2df(bm, dbsource = "isi", format = "bibtex")

# ----- Creation of working dataset -----
# Sampling
if (F) {
  working.set <- bm[sample(x = nrow(bm), size = ceiling(nrow(bm)/10)), ]
} else{
  working.set <- bm
}
# Operation of rows
rownames(working.set) <- 1:nrow(working.set)
working.set$DT <- factor(working.set$DT)
levels(working.set$DT) <- c("article", "book", "proceeding")
working.set$CR <- gsub(pattern = '.   ', replacement = "; ", x = working.set$CR, fixed = TRUE)

# ----- Data cleaning -----
# Extracting full name
working.set$aut_num <- 1
working.set$aut_com <- ""
working.set$aut_len <- 0
working.set$aut_mis <- 0
working.set$cr1_mis <- 0
  
for (i in 1:nrow(working.set)) {
  authors <- unlist(strsplit(working.set$AU[i], ';'))
  working.set$aut_num[i] <- length(authors)
  for (j in 1:length(authors)) {
    names <- unlist(strsplit(x = authors[j], split = ' '))
    names[2] <- substr(names[2], 1, 1)
    patron <- paste(c(".*", names[1], "[,]* (", names[2], "[[:alpha:]]*[.]*[[:space:]]*[[:alpha:]]*[.]*[[:space:]]*[[:alpha:]]*)(.*)"), collapse = "")
    if (grepl(pattern = patron, x = working.set$C1[i]) == TRUE & is.na(working.set$C1[i]) == FALSE) {
      patron <- gsub(pattern = patron, replacement = "\\1", x = working.set$C1[i])
      patron <- paste(unlist(strsplit(x = patron, split = '[[:punct:]]')), collapse = "")
      names[2] <- trimws(x = patron, which = "both")
    } else {
      working.set$aut_mis[i] <- working.set$aut_mis[i]+1
    }
    working.set$aut_len[i] <- working.set$aut_len[i] + nchar(paste(names, collapse = " "))
    working.set$aut_com[i] <- paste(c(working.set$aut_com[i], names[1], ", ", names[2], "; "), collapse = "")
  }
  working.set$aut_com[i] <- gsub(pattern = "(.*); $", replacement = "\\1", x = working.set$aut_com[i])
  working.set$aut_len[i] <- working.set$aut_len[i]/working.set$aut_num[i]
}

# Cleaining duplicated books
index <- grep(pattern = "book", x = working.set$DT)
working.set.a <- working.set[index, ]
working.set <- working.set[-index, ]

books <- data.frame(table(working.set.a$SO))
books <- subset(x = books, subset = Freq > 1)

for (i in 1:nrow(books)) {
  index <- grep(pattern = books$Var1[i], x = working.set.a$SO)
  working.set.b <- working.set.a[index, ]
  working.set.a <- working.set.a[-index, ]
  # Find the most relevant entry to use as base
  index <- grep(pattern = "book$", x = working.set.b$DT2, ignore.case = TRUE)
  entry <- working.set.b[index, ]
  working.set.b <- working.set.b[-index, ]
  
  # Fulfil the possible NA values
  focus.index <- grep(pattern = "TRUE", x = is.na(entry))
  for (j in focus.index) {
    if (all(is.na(working.set.b[,j])) == FALSE) {
      lookout <- max(nchar(working.set.b[,j]), na.rm = TRUE)
      index <- grep(pattern = lookout, x = nchar(working.set.b[,j]), ignore.case = TRUE)
      entry[1,j] <- working.set.b[index[1],j]
    } 
  }
  
  # Fulfil appending values
  #grep(pattern = "^AB$", x = colnames(entry))
  focus.index <- c(1, 27)
  for (j in focus.index) {
    temp <- trimws(x = unlist(strsplit(x = c(entry[1,j], working.set.b[,j]), split = ';')), which = 'both')
    if (all(is.na(temp)) == FALSE) {
      temp <- as.data.frame(temp, stringsAsFactors = FALSE)
      temp$order <- 1:nrow(temp)
      temp <- temp[!duplicated(temp$temp), ]
      temp$lenght <- nchar(temp$temp)
      temp$phonetic <- ""
      for (k in 1:nrow(temp)) {
        temp$phonetic[k] <- paste(phonetic(unlist(strsplit(x = temp$temp[k], split = " "))), collapse = "")
        temp$phonetic[k] <- substr(temp$phonetic[k], 1, 5)
      }
      temp <- temp[order(temp$phonetic, temp$lenght, decreasing = TRUE), ]
      index <- !duplicated(temp$phonetic)
      temp <- temp[index, ]
      temp <- temp[order(temp$order), ]
      entry[1,j] <- paste(temp$temp, collapse = "; ")
    }
  }
  
  # authors recount
  entry$aut_num <-  max(length(unlist(strsplit(x = entry$AU, split = ';'))), 
                        length(unlist(strsplit(x = entry$aut_com, split = ';'))),
                        na.rm = TRUE)
  
  # total times cited
  entry$TC <- sum(c(entry$TC, working.set.b$TC), na.rm = TRUE)
  
  
  
  # Fulfil decoupling values
  #grep(pattern = "^CR$", x = colnames(entry))
  focus.index <- c(10, 11, 12, 13, 14, 17)
  for (j in focus.index) {
    temp <- trimws(x = unlist(strsplit(x = c(entry[1,j], working.set.b[,j]), split = ';')), which = 'both')
    if (all(is.na(temp)) == FALSE) {
      temp <- data.frame(table(na.omit(temp)))
      entry[1,j] <- paste(temp$Var1, collapse = "; ")
    }
  }
  # Reconstruction of C1 files pending
  working.set.a <- rbind(working.set.a, entry)
}
working.set <- rbind(working.set, working.set.a)
working.set$AU <- gsub(pattern = ';', replacement = "; ", x = working.set$AU, fixed = TRUE)

# removing temporal variables
rm(books)
rm(entry)
rm(temp)
rm(working.set.a)
rm(working.set.b)
rm(authors)
rm(focus.index)
rm(index)
rm(i)
rm(j)
rm(k)
rm(lookout)
rm(names)
rm(patron)

# ----- Blibliometric analysis -----
bm_analysis <- biblioAnalysis(working.set, sep = "; ")
bm_summary <- summary(object = bm_analysis, k = 10, pause = FALSE)
plot(x = bm_analysis, k = 10, pause = TRUE)

# References analysis
