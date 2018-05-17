time <- Sys.time()
# ===== Opening raw data files =====
load(file = "0_input/0_rawComics.RData") # Raw data directory
rare.comics <- rawComics
# Preserving original index
rare.comics$index.global <- as.integer(row.names.data.frame(rare.comics))
# Erasing original raw file
rm(rawComics)
# Loading required libraries
require(stringdist)

# ===== Subsetting entries in the scope (rows) =====
rare.comics <- subset(x = rare.comics, 
                      select = c("ID", "Titles_TitleID", "index.global",
                                 "DisplayTitle", "AlphabetizedTitle",
                                 "ComicType","IssueNum", "Variation", "Printing", "FullIssue",
                                 "Genres", "MatureReadersOnly",
                                 "CoverDate", "StreetDate", "DateMask",
                                 "Cover.Price", "Last.Printed.Price",
                                 "Price.Year.1", "Price.Year.2", "Price.Year.3", "Price.Year.4",
                                 "Artist", "Writer",
                                 "Colorist", "Inker", "Letterer",
                                 "CoverArtist", "CoverInker",
                                 "Publisher", "Editor", "Copyright","Circulation",
                                 "Appearances", "Storylines", "Notes", "NotesText", "Comments",
                                 "CountryCode", "CoverPriceCurrencyCode"))

names(rare.comics) <- c("id", "comic.id", "index.global",
                        "title", "title.alpha",
                        "type","issue.num", "var", "print", "issue.raw",
                        "genre", "mature",
                        "cover.date", "street.date", "mask.date", 
                        "cover.price", "last.price",
                        "price.2011", "price.2012", "price.2013", "price.2014",
                        "con.artist", "con.writer",
                        "con.colorist", "con.inker", "con.letterer",
                        "con.cover.artist", "con.cover.inker",
                        "publisher", "editor", "copyright", "circulation",
                        "appearances", "storylines", "notes", "notes.text", "comments",
                        "country.code", "currency.code")

# Removing variations versions
load(file = "0_input/1_collapseComics.RData") # Reading the index file
index <- is.na(collapseComics$firstVersionIndex) # extraction the NA index from the dataset
index <- grep(pattern = FALSE, x = index) # extracting index of all non-first version issues
index <- collapseComics$index[index] # Extraction the comic index of all non-first version issues
rare.comics <- rare.comics[-index,] # Removing all non-first version issues
rm(collapseComics)


# Removing entries out of the scope, time, region, missing descriptor data
rare.comics$currency.code <- tolower(rare.comics$currency.code) # change to lower case the currency code
rare.comics$country.code <- tolower(rare.comics$country.code) # Change to lower case the country code
rare.comics$cover.date <- as.character(rare.comics$cover.date) # Change to character the cover date variable
rare.comics$cover.date <- gsub(pattern = "(.*)([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2})(.*)", replacement = "\\2",
                               x = rare.comics$cover.date) # Preserves only the year, month and day of the date, erases the time
rare.comics$cover.date <- as.POSIXct(rare.comics$cover.date, "%Y-%m-%d", tz = "CET") # Change the variable type to POSIX
# Subsets the relevant entries
rare.comics <- subset(x = rare.comics, subset = country.code == "us", select = -country.code)
rare.comics <- subset(x = rare.comics, subset = currency.code == "usd", select = -currency.code)

rare.comics <- subset(rare.comics, is.na(cover.date) == TRUE | cover.date >= as.POSIXct("1995-01-01", tz = "CET"))
rare.comics <- subset(rare.comics, is.na(cover.date) == TRUE | cover.date <= as.POSIXct("2015-12-31", tz = "CET"))

# ===== Formatting of variables =====
# --- Change to character variables ---
# Descriptors
rare.comics$title <- tolower(rare.comics$title)
rare.comics$type <- tolower(rare.comics$type)
rare.comics$var <- tolower(rare.comics$var)
rare.comics$genre <- tolower(rare.comics$genre)
# Dates
rare.comics$street.date <- tolower(rare.comics$street.date)
rare.comics$mask.date <- tolower(rare.comics$mask.date)
# Contributors
rare.comics$con.artist <- tolower(rare.comics$con.artist)
rare.comics$con.writer <- tolower(rare.comics$con.writer)
rare.comics$con.colorist <- tolower(rare.comics$con.colorist)
rare.comics$con.inker <- tolower(rare.comics$con.inker)
rare.comics$con.letterer <- tolower(rare.comics$con.letterer)
rare.comics$con.cover.artist <- tolower(rare.comics$con.cover.artist)
rare.comics$con.cover.inker <- tolower(rare.comics$con.cover.inker)
# Publishers
rare.comics$publisher <- tolower(rare.comics$publisher)
rare.comics$editor <- tolower(rare.comics$editor)
rare.comics$copyright <- tolower(rare.comics$copyright)
# Additional info
rare.comics$appearances   <- tolower(rare.comics$appearances)
rare.comics$storylines   <- tolower(rare.comics$storylines)
rare.comics$notes   <- tolower(rare.comics$notes)
rare.comics$notes.text   <- tolower(rare.comics$notes.text)
rare.comics$comments   <- tolower(rare.comics$comments)

# --- Changing enconding ---
enc.in <- "UTF-8"
enc.out <- "ASCII//TRANSLIT"
# Descriptors
rare.comics$title <- iconv(from = enc.in, to = enc.out, x = rare.comics$title)
rare.comics$title.alpha <- iconv(from = enc.in, to = enc.out, x = rare.comics$title.alpha)
rare.comics$type <- iconv(from = enc.in, to = enc.out, x = rare.comics$type)
rare.comics$var <- iconv(from = enc.in, to = enc.out, x = rare.comics$var)
rare.comics$genre <- iconv(from = enc.in, to = enc.out, x = rare.comics$genre)
# Contributors
rare.comics$con.artist <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.artist)
rare.comics$con.writer <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.writer)
rare.comics$con.colorist <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.colorist)
rare.comics$con.inker <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.inker)
rare.comics$con.letterer <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.letterer)
rare.comics$con.cover.artist <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.cover.artist)
rare.comics$con.cover.inker <- iconv(from = enc.in, to = enc.out, x = rare.comics$con.cover.inker)
# Publishers
rare.comics$publisher <- iconv(from = enc.in, to = enc.out, x = rare.comics$publisher)
rare.comics$editor <- iconv(from = enc.in, to = enc.out, x = rare.comics$editor)
rare.comics$copyright <- iconv(from = enc.in, to = enc.out, x = rare.comics$copyright)
# Additional info
rare.comics$appearances   <- iconv(from = enc.in, to = enc.out, x = rare.comics$appearances)
rare.comics$storylines   <- iconv(from = enc.in, to = enc.out, x = rare.comics$storylines)
rare.comics$notes   <- iconv(from = enc.in, to = enc.out, x = rare.comics$notes)
rare.comics$notes.text   <- iconv(from = enc.in, to = enc.out, x = rare.comics$notes.text)
rare.comics$comments   <- iconv(from = enc.in, to = enc.out, x = rare.comics$comments)

# --- Erasing empty spaces ---
rare.comics$type[grep(pattern = "^$", x = rare.comics$type)] <- NA
rare.comics$var[grep(pattern = "^$", x = rare.comics$var)] <- NA
rare.comics$genre[grep(pattern = "^$", x = rare.comics$genre)] <- NA
rare.comics$street.date[grep(pattern = "^$", x = rare.comics$street.date)] <- NA

rare.comics$con.artist[grep(pattern = "^$", x = rare.comics$con.artist)] <- NA
rare.comics$con.writer[grep(pattern = "^$", x = rare.comics$con.writer)] <- NA
rare.comics$con.colorist[grep(pattern = "^$", x = rare.comics$con.colorist)] <- NA
rare.comics$con.inker[grep(pattern = "^$", x = rare.comics$con.inker)] <- NA
rare.comics$con.letterer[grep(pattern = "^$", x = rare.comics$con.letterer)] <- NA
rare.comics$con.cover.artist[grep(pattern = "^$", x = rare.comics$con.cover.artist)] <- NA
rare.comics$con.cover.inker[grep(pattern = "^$", x = rare.comics$con.cover.inker)] <- NA

rare.comics$publisher[grep(pattern = "^$", x = rare.comics$publisher)] <- NA
rare.comics$editor[grep(pattern = "^$", x = rare.comics$editor)] <- NA
rare.comics$copyright[grep(pattern = "^$", x = rare.comics$copyright)] <- NA

rare.comics$circulation[which(rare.comics$circulation == 0)] <- NA

rare.comics$appearances[grep(pattern = "^$", x = rare.comics$appearances)] <- NA
rare.comics$storylines[grep(pattern = "^$", x = rare.comics$storylines)] <- NA
rare.comics$notes[grep(pattern = "^$", x = rare.comics$notes)] <- NA
rare.comics$notes.text[grep(pattern = "^$", x = rare.comics$notes.text)] <- NA
rare.comics$comments[grep(pattern = "^$", x = rare.comics$comments)] <- NA

# --- Restoring factor variables ---
# Descriptors
rare.comics$type <- as.factor(rare.comics$type)
rare.comics$var <- as.factor(rare.comics$var)
rare.comics$genre <- as.factor(rare.comics$genre)
# Dates
rare.comics$mask.date <- as.factor(rare.comics$mask.date)
# Contributors
rare.comics$con.artist <- as.factor(rare.comics$con.artist)
rare.comics$con.writer <- as.factor(rare.comics$con.writer)
rare.comics$con.colorist <- as.factor(rare.comics$con.colorist)
rare.comics$con.inker <- as.factor(rare.comics$con.inker)
rare.comics$con.letterer <- as.factor(rare.comics$con.letterer)
rare.comics$con.cover.artist <- as.factor(rare.comics$con.cover.artist)
rare.comics$con.cover.inker <- as.factor(rare.comics$con.cover.inker)
# Publishers
rare.comics$editor <- as.factor(rare.comics$editor)
rare.comics$copyright <- as.factor(rare.comics$copyright)
# Additional info
rare.comics$appearances   <- as.factor(rare.comics$appearances)
rare.comics$storylines   <- as.factor(rare.comics$storylines)
rare.comics$comments   <- as.factor(rare.comics$comments)
# ----- Restoring binomial variables
rare.comics$mature <- as.logical(rare.comics$mature)

# ===== Formating binary variables =====
# ----- Genre variable
# Standarization of variable size
genres <- levels(rare.comics$genre)
genres <- data.frame(genres, stringsAsFactors = FALSE)
genres$size <- nchar(genres$genres)
# Searching for entries without the proper size
index <- grep(pattern = "28", x = genres$size, invert = TRUE)
# Filling the missing characteres with "0"
for (i in 1:length(index)) {
  size <- nchar(genres$genres[index[i]])
  for (j in (size+1):28) {
    genres$genres[index[i]] <- paste(c(genres$genres[index[i]],"0"), collapse =  "")
  }
}
# Chnaging [space] charactares with 0
genres$genres <- gsub(pattern = " |[[:alpha:]]", replacement = "0", x = genres$genres)
# Restoring the new level names
levels(rare.comics$genre) <- genres$genres
# Cutting to 24 characters
rare.comics$genre <- as.character(rare.comics$genre)
rare.comics$genre <- gsub(pattern = "(^.{24})(.{4}$)", replacement = "\\1", x = rare.comics$genre)
# Erasing entries with no genre defined
index <- grep(pattern = "^0{24}$", rare.comics$genre)
rare.comics$genre[index]<- NA
# Counting the numbers of genres
rare.comics$genre.count <- rare.comics$genre
rare.comics$genre.count <- nchar(gsub(pattern = "0", replacement = "", x = rare.comics$genre.count), keepNA = TRUE)
# Restoring factor variable
rare.comics$genre <- as.factor(rare.comics$genre)
# Removing genres dataset
rm(genres)

# ----- Mask date
dates <- levels(rare.comics$mask.date)
dates <- data.frame(dates, stringsAsFactors = FALSE)
dates$size <- nchar(dates$dates)
# Chnaging [space] charactares with 0
dates$dates <- gsub(pattern = " |[[:alpha:]]", replacement = "0", x = dates$dates)
# Restoring the new level names
levels(rare.comics$mask.date) <- dates$dates
# Erasing entries with no date span defined
rare.comics$mask.date <- as.character(rare.comics$mask.date)
index <- grep(pattern = "^0{170}$", rare.comics$mask.date)
rare.comics$mask.date[index]<- NA
# Inserting year of start
rare.comics$year.start <- rare.comics$mask.date
rare.comics$year.start <- gsub(pattern = "(^0+)(.*)", replacement = "\\1", x = rare.comics$year.start)
rare.comics$year.start <- as.integer(nchar(rare.comics$year.start, keepNA = TRUE) + 1860)
# Inserting year of end
rare.comics$year.end <- rare.comics$mask.date
rare.comics$year.end <- gsub(pattern = "(^.+)1(0+)$", replacement = "\\2", x = rare.comics$year.end)
rare.comics$year.end <- as.integer(2030 - nchar(x = rare.comics$year.end, keepNA = TRUE))
# Restoring factor variable
rare.comics$mask.date <- as.factor(rare.comics$mask.date)
# Removing dates dataset
rm(dates)

# ===== Formating date variables =====
# Street date to POSIX
rare.comics$street.date <- gsub(pattern = "(.*)([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2})(.*)", replacement = "\\2",
                                x = rare.comics$street.date) # Preserves only the year, month and day of the date, erases the time
rare.comics$street.date <- as.POSIXct(rare.comics$street.date, "%Y-%m-%d", tz = "CET") # Change the variable type to POSIX
index <- which(is.na(rare.comics$cover.date) == TRUE)
rare.comics$cover.date[index] <- rare.comics$street.date[index]
# Correcting data span
rare.comics$date.check <- with(rare.comics, format(cover.date, "%Y") >= year.start & format(cover.date, "%Y") <= year.end)
index <- which(rare.comics$date.check == FALSE)
rare.comics$year.start[index] <- as.integer(with(rare.comics, ifelse(test = format(cover.date[index], "%Y") < year.start[index],
                                                                     yes = format(cover.date[index], "%Y"),
                                                                     no = year.start[index])))
rare.comics$year.end[index] <- as.integer(with(rare.comics, ifelse(test = format(cover.date[index], "%Y") > year.end[index],
                                                                   yes = format(cover.date[index], "%Y"),
                                                                   no = year.end[index])))
# Inserting missing years span
rare.comics$date.miss <- is.na(rare.comics$year.start) | is.na(rare.comics$year.end)
index <- which(rare.comics$date.miss == TRUE)
for (i in index) {
  partial <- which(rare.comics$comic.id == rare.comics$comic.id[i])
  # Check if there is any value different to NA
  if (any(!is.na(rare.comics$cover.date[partial])) == FALSE) {
    next
  }
  rare.comics$year.start[i] <- min(format(rare.comics$cover.date[partial], "%Y"), na.rm = TRUE)
  rare.comics$year.end[i] <- max(format(rare.comics$cover.date[partial], "%Y"), na.rm = TRUE)
}
# Correcting variable class
rare.comics$year.start <- as.integer(rare.comics$year.start)
rare.comics$year.end <- as.integer(rare.comics$year.end)
# Inserting duration in years
rare.comics$year.span <- as.integer(rare.comics$year.end - rare.comics$year.start + 1)

# ===== Formating price variables =====
prices <- rare.comics[, c("cover.price", "last.price", "price.2011", "price.2012", "price.2013", "price.2014")]
rare.comics$price.min <- apply(X = prices, MARGIN = 1, FUN = min, na.rm = TRUE)
rare.comics$price.max <- apply(X = prices, MARGIN = 1, FUN = max, na.rm = TRUE)
# Removing temporary variable
rm(prices)

# ===== Extracting information from title =====
rare.comics$title.raw <- rare.comics$title 
rare.comics$series.num <- NA
rare.comics$vol.num <- NA
# ---- Extracting numbers ----
# Extracting series number 
index <- grep(pattern = "\\((.*)([[:digit:]]+)(.*)series\\)", x = rare.comics$title)
rare.comics$series.num[index] <- as.integer(gsub(pattern = "(.*)([[:space:]]|\\()([[:digit:]]+)(.*)series(.*)", replacement = "\\3", x = rare.comics$title[index]))
rare.comics$title[index] <- gsub(pattern = "(.*)([[:space:]]|\\()([[:digit:]]+)(.*)series(.*)", replacement = "\\1\\2\\5", x = rare.comics$title[index])
index <- grep(pattern = "(.*)series(.+)([[:digit:]]+)(.*)", x = rare.comics$title)
rare.comics$series.num[index] <- as.integer(gsub(pattern = "(.*)series(.+)([[:digit:]]+)(.*)", replacement = "\\3", x = rare.comics$title[index]))
rare.comics$title[index] <- gsub(pattern = "(.*)series(.+)([[:digit:]]+)(.*)", replacement = "\\1series\\4", x = rare.comics$title[index])
# Extractring volume number
index <- grep(pattern = "\\((.*)vol(.*)([[:digit:]]+)(.*)\\)", x = rare.comics$title)
rare.comics$vol.num[index] <- as.integer(gsub(pattern = "(.*)(vol)(.*)([[:digit:]]+)(.*)", replacement = "\\4", x = rare.comics$title[index]))
rare.comics$title[index] <- gsub(pattern = "(.*)(vol)(.*)([[:digit:]]+)(.*)", replacement = "\\1\\5", x = rare.comics$title[index])
index <- grep(pattern = "(vol\\.|volum)(.*)([[:digit:]]+)", x = rare.comics$title)
rare.comics$vol.num[index] <- as.integer(gsub(pattern = "(.*)(vol\\.|volum)(.*)([[:digit:]]+)(.*)", replacement = "\\4", x = rare.comics$title[index]))
rare.comics$title[index] <- gsub(pattern = "(.*)(vol\\.|volum)(.*)([[:digit:]]+)(.*)", replacement = "\\1\\5", x = rare.comics$title[index])
# ---- Cleaning in-parenthesis text ----
temporal <- as.data.frame(grep(pattern = "(.*)\\((.+)\\)(.*)", x = rare.comics$title))
names(temporal) <- "index"
# Extracting in-parenthesis text
temporal$parenthesis <- gsub(pattern = "(.*)\\((.+)\\)(.*)", replacement = "\\2", x = rare.comics$title[temporal$index])
temporal$parenthesis <- gsub(pattern = "[&]", replacement = " and ", x = temporal$parenthesis)
temporal$parenthesis <- gsub(pattern = "[+]", replacement = " and ", x = temporal$parenthesis)
temporal$parenthesis <- gsub(pattern = "[[:punct:]]", replacement = " ", x = temporal$parenthesis)
temporal$parenthesis <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temporal$parenthesis)
temporal$parenthesis <- trimws(x = temporal$parenthesis, which = "both")
# Extracting publishers information
temporal$publishers <- rare.comics$publisher[temporal$index]
temporal$publishers <- gsub(pattern = "[&]", replacement = " and ", x = temporal$publishers)
temporal$publishers <- gsub(pattern = "[+]", replacement = " and ", x = temporal$publishers)
temporal$publishers <- gsub(pattern = "[[:punct:]]", replacement = " ", x = temporal$publishers)
temporal$publishers <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temporal$publishers)
temporal$publishers <- trimws(x = temporal$publishers, which = "both")
# Measuring string distance among publishers and in-parenthesis text
temporal$std <- stringdist(a = temporal$parenthesis, b = temporal$publishers, method = "jw")
# Defining a threshold.
temporal$similar <- ifelse(test = temporal$std < 0.23, yes = TRUE, no = FALSE)
# Extracting matching 
index <- temporal$index[grep(pattern = "TRUE", x = temporal$similar)]
# Substracting publishers from title
rare.comics$title[index] <- gsub(pattern = "(.*)\\((.+)\\)(.*)", replacement = "\\1\\3", x = rare.comics$title[index])
rm(temporal)
# ----- Correction of specific symbols ----
index <- grep(pattern = "#[[:space:]]*[[:digit:]]+", x = rare.comics$title)
for (i in index) {
  if (is.na(rare.comics$issue.num[i]) == TRUE) {
    rare.comics$issue.num[i] <- as.numeric(gsub(pattern = "(.+)#[[:space:]]*([[:digit:]]+)(.*)", replacement = "\\2", x = rare.comics$title[i]))
    rare.comics$title[i] <- gsub(pattern = "(.+)#[[:space:]]*([[:digit:]]+)(.*)", replacement = "\\1\\3", x = rare.comics$title[i])
  }
  next
}
index <- grep(pattern = "[&]", x = rare.comics$title)
rare.comics$title[index] <- gsub(pattern = "[&]", replacement = " and ", x = rare.comics$title[index])
index <- grep(pattern = "[+]", x = rare.comics$title)
rare.comics$title[index] <- gsub(pattern = "[+]", replacement = " plus ", x = rare.comics$title[index])
index <- grep(pattern = "(.+),[[:space:]]the[[:space:]]*([[:punct:]]|$)(.*)", x = rare.comics$title)
rare.comics$title[index] <- gsub(pattern = "(.+),[[:space:]]the(.*)", replacement = "the \\1 \\2", x = rare.comics$title[index])
index <- grep(pattern = "(.+),[[:space:]]a[[:space:]]*([[:punct:]]|$)(.*)", x = rare.comics$title)
rare.comics$title[index] <- gsub(pattern = "(.+),[[:space:]]a(.*)", replacement = "a \\1 \\2", x = rare.comics$title[index])
index <- grep(pattern = "[/]", x = rare.comics$title)
rare.comics$title[index] <- gsub(pattern = "[/]", replacement = " ", x = rare.comics$title[index])
# ---- Final format of the title strings ----
# Removing punctuation signs
rare.comics$title <- gsub(pattern = "[[:punct:]]", replacement = "", x = rare.comics$title)
# Removing multiple spaces
rare.comics$title <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.comics$title)
# Removing leading and trailing spaces
rare.comics$title <- trimws(x = rare.comics$title, which = "both")

# ===== Formating Publishers =====
rare.comics$publisher.raw <- rare.comics$publisher
# Removing signs
index <- grep(pattern = "[+&]", x = rare.comics$publisher)
rare.comics$publisher[index] <- gsub(pattern = "[+&]",replacement = " and ", x = rare.comics$publisher[index])
# Note the "/" sign may significate a collaboration between two publishers, " feat " is introduced to mark it
index <- grep(pattern = "[/]", x = rare.comics$publisher)
rare.comics$publisher[index] <- gsub(pattern = "[/]",replacement = " feat ", x = rare.comics$publisher[index])
# final format of the publisher strings
# Removing punctuation signs
rare.comics$publisher <- gsub(pattern = "[[:punct:]]", replacement = "", x = rare.comics$publisher)
# Removing multiple spaces
rare.comics$publisher <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.comics$publisher)
# Removing leading and trailing spaces
rare.comics$publisher <- trimws(x = rare.comics$publisher, which = "both")

# ===== Formating Additional info =====
# Preserving richer notes
index <- which(nchar(rare.comics$notes, keepNA = TRUE) < nchar(rare.comics$notes.text, keepNA = TRUE))
rare.comics$notes[index] <- rare.comics$notes.text[index]
index <- which(is.na(rare.comics$notes) == TRUE)
rare.comics$notes[index] <- rare.comics$notes.text[index]
# Counting the number of apparences
rare.comics$appearances.count <- as.character(rare.comics$appearances)
rare.comics$appearances.count <- gsub(pattern = "[^,]",replacement = "", rare.comics$appearances.count)
rare.comics$appearances.count <- nchar(rare.comics$appearances.count, keepNA = TRUE) + 1
# Counting the number of storylines
rare.comics$storylines.count <- as.character(rare.comics$storylines)
rare.comics$storylines.count <- gsub(pattern = "[^;]",replacement = "", rare.comics$storylines.count)
rare.comics$storylines.count <- nchar(rare.comics$storylines.count, keepNA = TRUE) + 1

# ===== Removing unnecessary factors =====
rare.comics <- subset(rare.comics, subset = is.na(year.start) == FALSE) # 1.79%
rare.comics <- subset(rare.comics, subset = is.na(genre) == FALSE) # 3.67-4.07%

# ==== Storing output file ====

# Reseting the index 
row.names(rare.comics) <- 1:nrow(rare.comics)
rare.comics$index.local <- as.integer(row.names.data.frame(rare.comics))
# Reorganizing the features
rare.comics <- rare.comics[, c("id", "comic.id", "index.global", "index.local",
                               "title", "type", "issue.num", "var", "print",
                               "series.num", "vol.num", 
                               "genre", "genre.count", "mature",
                               "cover.date", "mask.date", "year.start", "year.end", "year.span",
                               "cover.price", "price.min", "price.max",
                               "price.2011", "price.2012", "price.2013", "price.2014",
                               "con.artist", "con.writer",
                               "con.colorist", "con.inker", "con.letterer",
                               "con.cover.artist", "con.cover.inker", 
                               "publisher", "editor", "copyright", "circulation",
                               "appearances", "appearances.count", 
                               "storylines", "storylines.count",
                               "notes", "comments", 
                               "title.raw", "issue.raw", "publisher.raw")]

# ===== Creating summary =====
index <- 1:ncol(rare.comics)
names <- names(rare.comics)
summary.comics <- data.frame(index, names)
summary.comics$class <- NA
summary.comics$range <- NA
summary.comics$missing <- NA

for (i in index) {
  summary.comics$class[i] <- class(x = rare.comics[,i])[1]
  summary.comics$missing[i] <- round(x = sum(is.na(rare.comics[,i]))/nrow(rare.comics)*100, digits = 2)
  if (summary.comics$class[i] == "numeric"| summary.comics$class[i] == "integer") {
    summary.comics$range[i] <- paste(as.integer(range(x = rare.comics[,i], na.rm = TRUE)), collapse = " to ")
  } else if (summary.comics$class[i] == "factor") {
    summary.comics$range[i] <- paste(length(levels(rare.comics[,i])), "levels")
  } else if (summary.comics$class[i] == "POSIXct") {
    summary.comics$range[i] <- paste(range(x = format(rare.comics[,i], "%Y"), na.rm = TRUE), collapse = " to ")
  }
}

Sys.time() - time

# Removing temporary variables
rm(enc.in)
rm(enc.out)
rm(i)
rm(index)
rm(j)
rm(names)
rm(partial)
rm(size)
rm(time)

# Saving the rare.comics subset
save(file = "1_blackbox/0_rareComics.RData", list = "rare.comics")
save(file = "1_blackbox/1_summaryRareComics.RData", list = "summary.comics")