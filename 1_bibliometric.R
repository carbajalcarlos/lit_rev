# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix)
require(stringdist)

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
  append <- character()
  #grep(pattern = "^AB$", x = colnames(entry))
  focus.index <- c(1, 12, 27)
  for (j in focus.index) {
    for (k in 1:nrow(working.set.b)) {
      if (is.na(working.set.b[k,j]) == FALSE) {
        if (stringdist(a = entry[1,j], b = working.set.b[k,j], method = "jw") < .25) {
          if (nchar(entry[1,j]) < nchar(working.set.b[k,j])) {
            entry[1,j] <- working.set.b[k,j]
          } 
        } else {
          append  <- c(append, working.set.b[k,j])
        }
      }
    }
  } # 
  entry[1,j] < paste(c(entry[1,j], append), collapse = "; ")
  
  # authors recount
  entry$aut_num <-  max(length(unlist(strsplit(x = entry$AU, split = ';'))), 
                        length(unlist(strsplit(x = entry$AU, split = ';'))),
                        na.rm = TRUE)
  
  # total times cited
  entry$TC <- sum(c(entry$TC, working.set.b$TC), na.rm = TRUE)
  
  # Fulfil decoupling values
  #grep(pattern = "^CR$", x = colnames(entry))
  focus.index <- c(10, 11, 14, 17)
  for (j in focus.index) {
    temp <- trimws(x = unlist(strsplit(x = c(entry[1,j], working.set.b[,j]), split = ';')), which = 'both')
    if (all(is.na(temp)) == FALSE) {
      temp <- data.frame(table(na.omit(temp)))
      entry[1,j] <- paste(temp$Var1, collapse = "; ")
      }
  }
  
  # safe.set.a <- working.set.a
  # safe.set.b <- working.set.b
  # safe.set.e <- entry
  
  working.set.a <- safe.set.a
  working.set.b <- safe.set.b
  entry <- safe.set.e
  
  # Reconstruction of metadata
  temp <- trimws(x = unlist(strsplit(x = c(entry$C1, working.set.b$C1), split = ';')), which = 'both')
  if (all(is.na(temp)) == FALSE) {
    temp <- data.frame(table(na.omit(temp)))
    temp$Var1 <- as.character(temp$Var1)
    index.rep <- grep(pattern = "(REPRINT AUTHOR)", x = temp$Var1)
    index.oth <- grep(pattern = "(REPRINT AUTHOR)", x = temp$Var1, invert = TRUE)
  }
  authors <- trimws(x = unlist(strsplit(x = entry$aut_com, split = ';')), which = 'both')
  
  for (l in 1:length(authors)) {
    
  }
  
  
  for (l in index.oth) {
    grep(pattern = authors[l], x = )
  }
  
}



working.set <- rbind(working.set.a, working.set.b)


# Blibliometric analysis
bm_analysis <- biblioAnalysis(bm, sep = ";")
bm_summary <- summary(object = bm_analysis, k = 10, pause = FALSE)
plot(x = bm_analysis, k = 10, pause = TRUE)

# References analysis
