# ===== Initialization ====
# Loading datasets
load(file = "0_input/1_collapseComics.RData")
load(file = "1_blackbox/6_missingDatasets.RData")
load(file = "1_blackbox/7_IndexArrayB.RData")
load(file = "1_blackbox/8_comics.RData")
# Loading libraries
require(stringdist)

# ===== Extracting information =====
comparisonB <- index.arrayB
names(comparisonB) <- c("c.id", "s.id")
comparisonB <- comparisonB[with(comparisonB, order(s.id, c.id)), ]
# ----- Extracting data for the comparisonB matrix
# Title info
comparisonB$s.title <- missing.sales$title[comparisonB$s.id]
comparisonB$c.title <- missing.comics$title[comparisonB$c.id]
comparisonB$title.std <- stringdist(a = comparisonB$s.title,
                                   b = comparisonB$c.title,
                                   method = "cosine", q = 2)
# Publisher info
comparisonB$s.publisher <- missing.sales$publisher[comparisonB$s.id]
comparisonB$c.publisher <- missing.comics$publisher[comparisonB$c.id]
comparisonB$publisher.std <- stringdist(a = comparisonB$s.publisher, 
                                       b = comparisonB$c.publisher, 
                                       method = "jw")
# Issue info
comparisonB$s.issue <- as.numeric(missing.sales$issue.man[comparisonB$s.id])
comparisonB$c.issue <- missing.comics$issue.num[comparisonB$c.id]
comparisonB$issue.dif <- abs(comparisonB$s.issue - comparisonB$c.issue)
# Price info
comparisonB$s.price <- missing.sales$price[comparisonB$s.id]
comparisonB$c.price <- missing.comics$cover.price[comparisonB$c.id]
comparisonB$price.dif <- abs(comparisonB$s.price - comparisonB$c.price)
# Date info
comparisonB$s.date <- missing.sales$date[comparisonB$s.id]
comparisonB$c.date <- missing.comics$date.man[comparisonB$c.id]
comparisonB$date.dif <- as.numeric(abs(comparisonB$s.date - comparisonB$c.date)/(60*60*24*(365/12)))  # Difference in months
# Imputation marker
comparisonB$imp.issue <- is.na(comparisonB$s.issue)
comparisonB$imp.date <- is.na(comparisonB$c.date)

# ===== Evaluation =====
# Removing double imputation pairs()
comparisonB <- subset(x = comparisonB, subset = imp.date == FALSE & imp.issue == FALSE)
# Calculating similarity score
comparisonB$eval <- 0
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$title.std < 0.30, yes = 1, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$publisher.std < 0.1, yes = 1, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$issue.dif <= 0.1, yes = 3, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$price.dif < 0.5, yes = 1, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$date.dif < 4, yes = 1, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$imp.issue, yes = -1, no = 0)
comparisonB$eval <- comparisonB$eval + ifelse(test = comparisonB$imp.date, yes = -1, no = 0)

# ===== Selection =====
# Ordering by priority of the cualificators
comparisonB <- comparisonB[with(comparisonB, order(-eval,
                                                issue.dif,
                                                title.std,
                                                publisher.std,
                                                price.dif,
                                                date.dif,
                                                imp.date,
                                                imp.issue,
                                                c.id,
                                                s.id)), ]
# Extracting the best match for every sales ID
comparisonB$dup <- duplicated(x = comparisonB$s.id)
index <- which(comparisonB$dup == FALSE)
matchesB <- comparisonB[index,]
# Removing duplicated comics id
matchesB$dup <- duplicated(x = matchesB$c.id)
index <- which(matchesB$dup == FALSE)
matchesB <- matchesB[index,]
# Removing unreliable matchesB
matchesB <- subset(x = matchesB, subset = eval >= max(matchesB$eval)-1)

# Measuring efficacyB of the matching process
efficacyB <- round(nrow(matchesB)/nrow(missing.sales)*100, digits = 2)

# ===== Preparing datasets =====
# Comics dataset
done.comicsB <- missing.comics[, c("id", "comic.id", "index.local", "title",
                                 "type", "issue.num", "var", "print",
                                 "series.num", "vol.num",
                                 "genre", "genre.count", "mature",
                                 "cover.date", "mask.date",
                                 "year.start", "year.end", "year.span",
                                 "cover.price", "price.2011", "price.2012",
                                 "price.2013", "price.2014",
                                 "con.artist", "con.writer",
                                 "con.colorist", "con.inker", "con.letterer",
                                 "con.cover.artist", "con.cover.inker",
                                 "publisher","editor",
                                 "copyright", "circulation",
                                 "appearances", "appearances.count",
                                 "storylines", "storylines.count",
                                 "notes", "comments")]
variations <- data.frame(table(collapseComics$firstVersionID)+1)
names(variations) <- c("id", "var.count")
done.comicsB$var.count <- is.element(done.comicsB$id, variations$id)
index <- which(done.comicsB$var.count == TRUE)
sub.comics <- done.comicsB[index, -41]
sub.comics <- merge(x = sub.comics, y = variations, by = "id")
done.comicsB <- done.comicsB[-index, ]
done.comicsB <- rbind(done.comicsB, sub.comics)
# Sales dataset
done.salesB <- missing.sales[, c("index.local", "type",
                               "rank", "price", "units.sold", "date")]
names(done.salesB) <- c("index.local", "is.book",
                       "rank", "sales.price", "units.sold", "sales.date")
index <- which(done.salesB$is.book == "book")
done.salesB$is.book <- FALSE
done.salesB$is.book[index] <- TRUE
done.salesB$sales.year <- as.integer(format(done.salesB$sales.date, "%Y"))
done.salesB$sales.month <- as.integer(format(done.salesB$sales.date, "%m"))

# ===== Merging datasets =====
# Extracting matchesB id
matchesB <- matchesB[, c("s.id", "c.id")]
# Merging
comicsB <- merge(x = matchesB, y = done.comicsB,
                by.x = "c.id", by.y = "index.local")
comicsB <- merge(x = comicsB, y = done.salesB,
                by.x = "s.id", by.y = "index.local")
# Structuring the new dataset
comicsB <- comicsB[, c("id", "comic.id", "title",
                     "type", "issue.num", "var", "var.count" , "print",
                     "is.book", "series.num", "vol.num",
                     "genre" , "genre.count", "mature",
                     "cover.date", "sales.date", "sales.year", "sales.month",
                     "mask.date", "year.start", "year.end", "year.span",
                     "cover.price", "sales.price",
                     "price.2011", "price.2012", "price.2013", "price.2014",
                     "rank", "units.sold", "circulation",
                     "publisher", "editor", "copyright",
                     "con.artist", "con.writer",
                     "con.colorist", "con.inker", "con.letterer",
                     "con.cover.artist", "con.cover.inker",
                     "appearances", "appearances.count",
                     "storylines", "storylines.count",
                     "notes", "comments")]

# Merging comics database
comics <- rbind(comics, comicsB)

# Correcting names
names(comics) <- c("id.issue", "id.comic", "title",
                   "type", "issue.num", "var", "var.count" , "print",
                   "is.book", "series.num", "vol.num",
                   "genre" , "genre.count", "mature",
                   "cover.date", "sales.date", "sales.year", "sales.month",
                   "mask.date", "year.start", "year.end", "year.span",
                   "cover.price", "sales.price",
                   "price.2011", "price.2012", "price.2013", "price.2014",
                   "rank", "units.sold", "circulation",
                   "publisher", "editor", "copyright",
                   "con.artist", "con.writer",
                   "con.colorist", "con.inker", "con.letterer",
                   "con.cover.artist", "con.cover.inker",
                   "appearances", "appearances.count",
                   "storylines", "storylines.count",
                   "notes", "comments")


# Removing temporary variables
rm(collapseComics)
rm(index.arrayB)
rm(missing.sales)
rm(missing.comics)
rm(sub.comics)
rm(variations)
rm(index)

# ===== Storing output =====
save(list = c("comparisonB", "matchesB", "efficacyB"),
     file = "Miscellaneous/5_wsMatchingB.RData")
save(list = "matchesB", file = "1_blackbox/4_matchesB.RData")
save(list = "done.comicsB", file = "1_blackbox/5_doneComicsB.RData")
save(list = "done.salesB", file = "1_blackbox/5_doneSalesB.RData")
save(list = "comics", file = "1_blackbox/9_comics.RDAta")
