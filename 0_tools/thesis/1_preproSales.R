time <- Sys.time()
# ===== Preparing the working variable =====
load(file = "0_input/0_rawSales.RData") # Raw data directory
rare.sales <- sales_raw
# Preserving original index
rare.sales$index.global <- as.integer(row.names.data.frame(rare.sales))
# Renaming variables
names(rare.sales) <- c("rank", "title", "issue.raw", "price", "publisher", "units.sold", "date", "type", "index.global")
rm(sales_raw) # Erasing original raw file

# ===== General variable formating =====
# ---- Changing the variable type ----
rare.sales$rank <- as.integer(gsub(pattern = "[^[:digit:]]", replacement = "", x = rare.sales$rank)) # changing the rank variable to integers
rare.sales$title <- as.character(rare.sales$title) # changing the title variable to character
rare.sales$price <- as.numeric(gsub(pattern = "[$]", replacement = "", x = rare.sales$price)) # removing the $ sign and changing the price varible to numeric
rare.sales$publisher <- as.character(rare.sales$publisher) # changing the title variable to character
rare.sales$units.sold <- as.integer(gsub(pattern = "[^[:digit:]]", replacement = "", x = rare.sales$units.sold)) # removing . and n/a and changing the units sold variable to integer
rare.sales$date <- as.POSIXct(paste(rare.sales$date,"-01", sep = ""), "%Y-%m-%d", tz = "CET") # Change the variable type to POSIX
rare.sales$type <- as.factor(rare.sales$type)
levels(rare.sales$type) <- c("issue", "book")
# ---- Changing the character codification ----
rare.sales$title <- iconv(x = rare.sales$title, from = "UTF-8", "ASCII//TRANSLIT") # Transliteration of the names
rare.sales$publisher <- iconv(x = rare.sales$publisher, from = "UTF-8", "ASCII//TRANSLIT") # Transliteration of the names
rare.sales$issue.raw <- iconv(x = rare.sales$issue.raw, from = "UTF-8", "ASCII//TRANSLIT") # Transliteration of the names
rare.sales$title <- tolower(rare.sales$title)
rare.sales$publisher <- tolower(rare.sales$publisher)
rare.sales$issue.raw <- tolower(rare.sales$issue.raw)

# ===== Extracting infromation from comic issue =====
rare.sales$issue <- rare.sales$issue.raw # Creating clean issue column
rare.sales$info <- NA
rare.sales$prt <- NA
rare.sales$var <- NA
# ---- Dealing with signs ----
# Searching possible issue/price 
index <- grep(pattern = "\\.", x = rare.sales$issue)
rare.sales$info[index] <- ifelse(test = rare.sales$issue[index] == rare.sales$price[index],
                          yes = "Issue = Price",
                          no = NA)
rare.sales$issue[index] <- NA
# Errasing -1, $, url; type entries
index <- grep(pattern = "^-1|^\\$|^t=", x = rare.sales$issue) 
rare.sales$issue[index] <- NA
# Extracting in parenthesis information
index <- grep(pattern = "\\(.*\\)", x = rare.sales$issue)
rare.sales$info[index] <- paste(rare.sales$info[index], trimws(gsub(pattern = "^.*\\((.*)\\).*$", replacement = "\\1", x = rare.sales$issue[index]), which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "\\(.*\\)", replacement = "", x = rare.sales$issue[index]), which = "both")
# Extracting asterisk entries
index <- grep(pattern = "\\*", x = rare.sales$issue)
rare.sales$info[index] <- paste(rare.sales$info[index], "ast")
rare.sales$issue[index] <- trimws(gsub(pattern = "\\*", replacement = "", x = rare.sales$issue[index]), which = "both")
# Extracting "/publication-year"
index <- grep(pattern = "\\/[[:digit:]]{4}", x = rare.sales$issue)
rare.sales$info[index] <- paste(rare.sales$info[index], trimws(gsub(pattern = "^.*\\/([[:digit:]]{4}).*$", replacement = "issue year: \\1", x = rare.sales$issue[index]), which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "\\/[[:digit:]]{4}", replacement = "", x = rare.sales$issue[index]), which = "both")
# Extracting 1/2, one-half, issue numbers
index <- grep(pattern = "1([[:space:]]*)\\/([[:space:]]*)2|one-half", x = rare.sales$issue)
rare.sales$issue[index] <- trimws(gsub(pattern = "1([[:space:]]*)\\/([[:space:]]*)2|one-half", replacement = ".5", x = rare.sales$issue[index]), which = "both")
# Extracting #-# issues
index <- grep(pattern = "-", x = rare.sales$issue)
rare.sales$info[index] <- paste(rare.sales$info[index], trimws(rare.sales$issue[index], which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "-", replacement = "", x = rare.sales$issue[index]), which = "both")
# Special cases 
rare.sales$issue <- trimws(gsub(pattern = " \\.5", replacement = ".5", x = rare.sales$issue), which = "both")
# ---- Dealing with letters ----
# Moving new issues
index <- grep(pattern = "^new$", x = rare.sales$issue)
rare.sales$info[index] <- paste(rare.sales$info[index], trimws(rare.sales$issue[index], which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "^new$", replacement = "", x = rare.sales$issue[index]), which = "both")
# Moving printing variations
rare.sales$issue <- gsub(pattern = "[[:space:]](ptg|prtg|printing|prt)[[:space:]]*", replacement = " print ", rare.sales$issue)
index <- grep(pattern = " (print|reprint)", x = rare.sales$issue)
rare.sales$prt[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*)( print )(.*)", replacement = "\\2\\3", x = rare.sales$issue[index]), which = "both")
rare.sales$issue[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*)( print )", replacement = "\\1 ", x = rare.sales$issue[index]), which = "both")
# Moving edition variations
rare.sales$issue <- trimws(gsub(pattern = "[[:space:]](ed|edition)([[:space:]]*)", replacement = " edition ", rare.sales$issue), which = "both")
index <- grep(pattern = " edition", x = rare.sales$issue)
rare.sales$var[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*edition)(.*)", replacement = "\\2", rare.sales$issue[index]), which = "both")
rare.sales$issue[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*edition)(.*)", replacement = "\\1\\3", rare.sales$issue[index]), which = "both")
# Moving cover variations
rare.sales$issue <- trimws(gsub(pattern = "(cover|cver|cvrs|cvr)[[:space:]]*", replacement = " cover ", rare.sales$issue), which = "both")
index <- grep(pattern = " cover", x = rare.sales$issue)
rare.sales$var[index] <- paste(rare.sales$var[index], trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*cover)(.*)", replacement = "\\2\\3", rare.sales$issue[index]), which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*cover)(.*)", replacement = "\\1", rare.sales$issue[index]), which = "both")
# Moving general variations
rare.sales$issue <- trimws(gsub(pattern = "[[:space:]](var|variant)[[:space:]]*)", replacement = " variation ", rare.sales$issue), which = "both")
index <- grep(pattern = " variation", x = rare.sales$issue)
rare.sales$var[index] <- paste(rare.sales$var[index], trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*variation)(.*)", replacement = "\\2", rare.sales$issue[index]), which = "both"))
rare.sales$issue[index] <- trimws(gsub(pattern = "(^\\.*[[:digit:]]+)(.*variation)(.*)", replacement = "\\1\\3", rare.sales$issue[index]), which = "both")
# Moving all the remaining string in the issue number
rare.sales$info <- paste(rare.sales$info, gsub(pattern = "(^\\.*[[:digit:]]*)(.*)", replacement = "\\2", x = rare.sales$issue)) 
rare.sales$issue <- gsub(pattern = "([[:digit:]]*\\.*[[:digit:]]+)(.*)", replacement = "\\1", x = rare.sales$issue)
index <- grep(pattern = "[[:alpha:]]", x = rare.sales$issue)
rare.sales$issue[index] <- NA
rare.sales$issue <- gsub(pattern = "[[:alpha:]]", replacement = "", x = rare.sales$issue)
# ---- Last formating ----
# Removing NA added by paste function
rare.sales$info <- gsub(pattern = "NA", replacement = "", x = rare.sales$info)
rare.sales$prt <- gsub(pattern = "NA", replacement = "", x = rare.sales$prt)
rare.sales$var <- gsub(pattern = "NA", replacement = "", x = rare.sales$var)
# Removing double spaces
rare.sales$info <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.sales$info)
rare.sales$prt <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.sales$prt)
rare.sales$var <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.sales$var)
# Removing heading and trailing spaces
rare.sales$info <- trimws(x = rare.sales$info, which = "both")
rare.sales$prt <- trimws(x = rare.sales$prt, which = "both")
rare.sales$var <- trimws(x = rare.sales$var, which = "both")
# Swapping empty spaces and only-signs per NA
index <- grep(pattern = "[[:alnum:]]", invert = TRUE, x = rare.sales$info)
rare.sales$info[index] <- NA
index <- grep(pattern = "[[:alnum:]]", invert = TRUE, x = rare.sales$prt)
rare.sales$prt[index] <- NA
index <- grep(pattern = "[[:alnum:]]", invert = TRUE, x = rare.sales$var)
rare.sales$var[index] <- NA
# Saving issue as a numeric variable
rare.sales$issue <- gsub(pattern = "NA", replacement = "", x = rare.sales$issue)
rare.sales$issue <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.sales$issue)
rare.sales$issue <- trimws(x = rare.sales$issue, which = "both")

index <- grep(pattern = "[[:digit:]]", invert = TRUE, x = rare.sales$issue)
rare.sales$issue[index] <- NA
rare.sales$issue <- as.numeric(rare.sales$issue)
rm(index)

# ===== Extracting infromation from comic title =====
rare.sales$title.raw <- rare.sales$title
rare.sales$vol.num <- NA
rare.sales$series.num <- NA
rare.sales$book.num <- NA
#rare.sales$series.num <- NA
# ---- Removing specific signs ----
index <- grep(pattern = "[.-]", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[.-]", replacement = "", x = rare.sales$title[index])
index <- grep(pattern = "[&]", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[&]", replacement = " and ", x = rare.sales$title[index])
# ---- Extracting volume number ----
index <- grep(pattern = "(.+)(vol|volume)[[:space:]]([[:digit:]]+)(.*)", x = rare.sales$title)
rare.sales$vol.num[index] <- as.integer(gsub(pattern = "(.+)(vol|volume)[[:space:]]([[:digit:]]+)(.*)", replacement = "\\3", x = rare.sales$title[index]))
rare.sales$title[index] <- gsub(pattern = "(.+)(vol|volume)[[:space:]]([[:digit:]]+)(.*)", replacement = "\\1\\4", x = rare.sales$title[index])
index <- grep(pattern = "(.+)(vol|volume)[[:space:]](one|i)([[:space:]]|$)", x = rare.sales$title)
rare.sales$vol.num[index] <- 1
rare.sales$title[index] <- gsub(pattern = "(.+)(vol|volume)[[:space:]](one|i)([[:space:]]|$)(.*)", replacement = "\\1\\5", x = rare.sales$title[index])
index <- grep(pattern = "(.+)(vol|volume)[[:space:]](two|ii)([[:space:]]|$)", x = rare.sales$title)
rare.sales$vol.num[index] <- 2
rare.sales$title[index] <- gsub(pattern = "(.+)(vol|volume)[[:space:]](two|ii)([[:space:]]|$)(.*)", replacement = "\\1\\5", x = rare.sales$title[index])
index <- grep(pattern = "(.+)(vol|volume)[[:space:]](three|iii)([[:space:]]|$)", x = rare.sales$title)
rare.sales$vol.num[index] <- 3
rare.sales$title[index] <- gsub(pattern = "(.+)(vol|volume)[[:space:]](three|iii)([[:space:]]|$)(.*)", replacement = "\\1\\5", x = rare.sales$title[index])
# ---- Extracting book number ----
index <- grep(pattern = "[[:space:]]+book[[:space:]]+[[:digit:]]+", x = rare.sales$title)
rare.sales$book.num[index] <- as.integer(gsub(pattern = "(.+)[[:space:]]book[[:space:]]+([[:digit:]]+)(.*)", replacement = "\\2", x = rare.sales$title[index]))
rare.sales$title[index] <- gsub(pattern = "(.+)[[:space:]]book[[:space:]]+([[:digit:]]+)(.*)", replacement = "\\1 \\3", x = rare.sales$title[index])
# ---- Extracting series number ----
index <- grep(pattern = "series(.*)[[:digit:]]+", x = rare.sales$title)
rare.sales$series.num[index] <- gsub(pattern = "(.+)series(.*)([[:digit:]]+)(.*)",replacement = "\\3", x = rare.sales$title[index])
rare.sales$title[index] <- gsub(pattern = "(.+)series(.*)([[:digit:]]+)(.*)",replacement = "\\1\\4", x = rare.sales$title[index])
# ---- Removing specific acronyms ----
index <- grep(pattern = "(.+)[[:space:]]hc([[:space:]]|$)(.*)", x = rare.sales$title)
rare.sales$var[index] <- "hc"
rare.sales$title[index] <- gsub(pattern = "(.+)[[:space:]]hc([[:space:]]|$)(.*)", replacement = "\\1 \\3", x = rare.sales$title[index])
index <- grep(pattern = "[[:space:]]+ed([[:space:]]+|$)", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[[:space:]]+ed([[:space:]]+|$)", replacement = " edition ", x = rare.sales$title[index])
index <- grep(pattern = "o/a", x = rare.sales$title)
rare.sales$info[index] <- "o/a"
rare.sales$title[index] <- gsub(pattern = "(.+)o/a(.*)", replacement = "\\1\\2", x = rare.sales$title[index])
index <- grep(pattern = "[[:space:]]+1[[:space:]]*/[[:space:]]*2([[:space:]]+|$)", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[[:space:]]+1[[:space:]]*/[[:space:]]*2([[:space:]]+|$)", replacement = " half ", x = rare.sales$title[index])
index <- grep(pattern = "\\((res|resolicitation)\\)", x = rare.sales$title)
rare.sales$info[index] <- "res"
rare.sales$title[index] <- gsub(pattern = "(.+)\\((res|resolicitation)\\)(.*)", replacement = "\\1\\3", x = rare.sales$title[index])
index <- grep(pattern = "\\((net)\\)", x = rare.sales$title)
rare.sales$info[index] <- "net"
rare.sales$title[index] <- gsub(pattern = "(.+)\\((net)\\)(.*)", replacement = "\\1\\3", x = rare.sales$title[index])
index <- grep(pattern = "([[:space:]]|[[:punct:]])(.*)[[:space:]]+ptg", x = rare.sales$title)
rare.sales$prt[index] <- gsub(pattern = "(.+)([[:space:]]|[[:punct:]])(.*)[[:space:]]+ptg(.*)", replacement = "\\3", rare.sales$title[index])
rare.sales$title[index] <- gsub(pattern = "(.+)([[:space:]]|[[:punct:]])(.*)[[:space:]]+ptg(.*)", replacement = "\\1\\2\\4", x = rare.sales$title[index])
# ---- Removing remaining signs ----
index <- grep(pattern = "[/]", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[/]", replacement = " ", x = rare.sales$title[index])
index <- grep(pattern = "#[[:space:]]*[[:digit:]]+", x = rare.sales$title)
for (i in index) {
  if (is.na(rare.sales$issue[i]) == TRUE) {
    rare.sales$issue[i] <- as.numeric(gsub(pattern = "(.+)#[[:space:]]*([[:digit:]]+)(.*)", replacement = "\\2", x = rare.sales$title[i]))
    rare.sales$title[i] <- gsub(pattern = "(.+)#[[:space:]]*([[:digit:]]+)(.*)", replacement = "\\1\\3", x = rare.sales$title[i])
  }
  next
}
rare.sales$title[index] <- gsub(pattern = "(.+)#[[:space:]]*([[:digit:]]+)(.*)", replacement = "", x = rare.sales$title[index])
# ---- Final formation ----
index <- grep(pattern = "[[:punct:]]", x = rare.sales$title)
rare.sales$title[index] <- gsub(pattern = "[[:punct:]]", replacement = "", x = rare.sales$title[index])
rare.sales$title <- gsub(pattern = "[[:space:]]+", replacement = " ", x = rare.sales$title)

# ==== Formating comic publisher ====
rare.sales$publisher.raw <- rare.sales$publisher
# ---- Removing signs ----
index <- grep(pattern = "[&]", x = rare.sales$publisher)
rare.sales$publisher[index] <- gsub(pattern = "[&]", replacement = " and ", x = rare.sales$publisher[index])
index <- grep(pattern = "[/]", x = rare.sales$publisher)
rare.sales$publisher[index] <- gsub(pattern = "[/]", replacement = " feat ", x = rare.sales$publisher[index])
# ---- Final formatting ----
index <- grep(pattern = "[[:punct:]]", x = rare.sales$publisher)
rare.sales$publisher[index] <- gsub(pattern = "[[:punct:]]", replacement = "", x = rare.sales$publisher[index])
rare.sales$publisher <- gsub(pattern = "[[:space:]]+", replacement = " ", rare.sales$publisher)
rare.sales$publisher <- trimws(x = rare.sales$publisher, which = "both")
# ---- Creation of issue number ----
index <- which(is.na(rare.sales$issue) == TRUE) 
rare.sales$issue[index] <- rare.sales$vol.num[index]
index <- which(is.na(rare.sales$issue) == TRUE) 
rare.sales$issue[index] <- rare.sales$book.num[index]
index <- which(is.na(rare.sales$issue) == TRUE) 
rare.sales$issue[index] <- rare.sales$series.num[index]

# ==== Formating variables ====
# Numeric variales
rare.sales$issue <- as.numeric(rare.sales$issue)
rare.sales$series.num <- as.integer(rare.sales$series.num)
# Factor variables
rare.sales$info <-as.factor(rare.sales$info)
rare.sales$var <- as.factor(rare.sales$var)
rare.sales$prt <- as.factor(rare.sales$prt)

# ==== Saving the rare.sales subset ====
# Remmoving missing data observations
rare.sales <- subset(rare.sales, subset = is.na(rank) == FALSE) # 0.01%
rare.sales <- subset(rare.sales, subset = is.na(units.sold) == FALSE) # 0.05%
rare.sales <- subset(rare.sales, subset = is.na(price) == FALSE) # 0.05%

# Reseting the index 
row.names(rare.sales) <- 1:nrow(rare.sales)
rare.sales$index.local <- as.integer(row.names.data.frame(rare.sales))

# Reorganizing the variables
rare.sales <- rare.sales[ , c("index.global", "index.local",
                              "title", "type",
                              "issue", "var", "prt", "info",
                              "vol.num","book.num","series.num",
                              "date", "rank", "price", "units.sold","publisher",
                              "title.raw", "issue.raw", "publisher.raw")]

names(rare.sales) <- c("index.global", "index.local",
                       "title", "type",
                       "issue.num", "var", "print", "issue.info",
                       "vol.num","book.num","series.num",
                       "date", "rank", "price", "units.sold", "publisher",
                       "title.raw", "issue.raw", "publisher.raw")

# ===== Creating summary =====
index <- 1:ncol(rare.sales)
names <- names(rare.sales)
summary.sales <- data.frame(index, names)
summary.sales$class <- NA
summary.sales$range <- NA
summary.sales$missing <- NA

for (i in index) {
  summary.sales$class[i] <- class(x = rare.sales[,i])[1]
  summary.sales$missing[i] <- round(x = sum(is.na(rare.sales[,i]))/nrow(rare.sales)*100, digits = 2)
  if (summary.sales$class[i] == "numeric"| summary.sales$class[i] == "integer") {
    summary.sales$range[i] <- paste(as.integer(range(x = rare.sales[,i], na.rm = TRUE)), collapse = " to ")
  } else if (summary.sales$class[i] == "factor") {
    summary.sales$range[i] <- paste(length(levels(rare.sales[,i])), "levels")
  } else if (summary.sales$class[i] == "POSIXct") {
    summary.sales$range[i] <- paste(range(x = format(rare.sales[,i], "%Y"), na.rm = TRUE), collapse = " to ")
  }
}

Sys.time() - time

# Removing temporary variables
rm(i)
rm(index)
rm(names)
rm(time)

# Storing the data set
save(file = "1_blackbox/0_rareSales.RData", list = "rare.sales")
save(file = "1_blackbox/1_summaryRareSales.RData", list = "summary.sales")
