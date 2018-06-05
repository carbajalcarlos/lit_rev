# ===== Initialization ====
# Loading datasets
load(file = "0_input/1_collapseComics.RData")
load(file = "1_blackbox/2_mediumComics.RData")
load(file = "1_blackbox/2_mediumSales.RData")
load(file = "1_blackbox/3_indexArray.RData")
# Loading libraries
require(stringdist)

# ===== Extracting information =====
comparison <- index.array
names(comparison) <- c("s.id", "c.id")
comparison <- comparison[with(comparison, order(s.id, c.id)), ]
# ----- Extracting data for the comparison matrix
# Title info
comparison$s.title <- medium.sales$title[comparison$s.id]
comparison$c.title <- medium.comics$title[comparison$c.id]
comparison$title.std <- stringdist(a = comparison$s.title,
                                   b = comparison$c.title,
                                   method = "cosine", q = 2)
# Publisher info
comparison$s.publisher <- medium.sales$publisher[comparison$s.id]
comparison$c.publisher <- medium.comics$publisher[comparison$c.id]
comparison$publisher.std <- stringdist(a = comparison$s.publisher, 
                                       b = comparison$c.publisher, 
                                       method = "jw")
# Issue info
comparison$s.issue <- as.numeric(medium.sales$issue.man[comparison$s.id])
comparison$c.issue <- medium.comics$issue.num[comparison$c.id]
comparison$issue.dif <- abs(comparison$s.issue - comparison$c.issue)
# Price info
comparison$s.price <- medium.sales$price[comparison$s.id]
comparison$c.price <- medium.comics$cover.price[comparison$c.id]
comparison$price.dif <- abs(comparison$s.price - comparison$c.price)
# Date info
comparison$s.date <- medium.sales$date[comparison$s.id]
comparison$c.date <- medium.comics$date.man[comparison$c.id]
comparison$date.dif <- as.numeric(abs(comparison$s.date - comparison$c.date)/(60*60*24*(365/12)))  # Difference in months
# Imputation marker
comparison$imp.issue <- is.na(comparison$s.issue)
comparison$imp.date <- is.na(comparison$c.date)

# ===== Evaluation =====
# Removing double imputation pairs()
comparison <- subset(x = comparison, subset = imp.date == FALSE & imp.issue == FALSE)
# Calculating similarity score
comparison$eval <- 0
comparison$eval <- comparison$eval + ifelse(test = comparison$title.std < 0.30, yes = 1, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$publisher.std < 0.1, yes = 1, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$issue.dif <= 0.1, yes = 3, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$price.dif < 0.5, yes = 1, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$date.dif < 4, yes = 1, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$imp.issue, yes = -1, no = 0)
comparison$eval <- comparison$eval + ifelse(test = comparison$imp.date, yes = -1, no = 0)

# ===== Selection =====
# Ordering by priority of the cualificators
comparison <- comparison[with(comparison, order(-eval,
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
comparison$dup <- duplicated(x = comparison$s.id)
index <- which(comparison$dup == FALSE)
matches <- comparison[index,]
# Removing duplicated comics id
matches$dup <- duplicated(x = matches$c.id)
index <- which(matches$dup == FALSE)
matches <- matches[index,]
# Removing unreliable matches
matches <- subset(x = matches, subset = eval >= max(matches$eval)-2)

# Measuring efficacy of the matching process
efficacy <- round(nrow(matches)/nrow(medium.sales)*100, digits = 2)

# ===== Preparing datasets =====
# Comics dataset
done.comics <- medium.comics[, c("id", "comic.id", "index.local", "title",
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
done.comics$var.count <- is.element(done.comics$id, variations$id)
index <- which(done.comics$var.count == TRUE)
sub.comics <- done.comics[index, -41]
sub.comics <- merge(x = sub.comics, y = variations, by = "id")
done.comics <- done.comics[-index, ]
done.comics <- rbind(done.comics, sub.comics)
# Sales dataset
done.sales <- medium.sales[, c("index.local", "type",
                               "rank", "price", "units.sold", "date")]
names(done.sales) <- c("index.local", "is.book",
                       "rank", "sales.price", "units.sold", "sales.date")
index <- which(done.sales$is.book == "book")
done.sales$is.book <- FALSE
done.sales$is.book[index] <- TRUE
done.sales$sales.year <- as.integer(format(done.sales$sales.date, "%Y"))
done.sales$sales.month <- as.integer(format(done.sales$sales.date, "%m"))

# ===== Merging datasets =====
# Extracting matches id
matches <- matches[, c("s.id", "c.id")]
# Merging
comics <- merge(x = matches, y = done.comics,
                by.x = "c.id", by.y = "index.local")
comics <- merge(x = comics, y = done.sales,
                by.x = "s.id", by.y = "index.local")
# Structuring the new dataset
comics <- comics[, c("id", "comic.id", "title",
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

# ===== Storing output =====
save(list = c("comparison", "matches", "efficacy"),
     file = "Miscellaneous/3_wsMatching.RData")
save(list = "matches", file = "1_blackbox/4_matches.RData")
save(list = "done.comics", file = "1_blackbox/5_doneComics.RData")
save(list = "done.sales", file = "1_blackbox/5_doneSales.RData")
save(list = "comics", file = "1_blackbox/8_comics.RData")
