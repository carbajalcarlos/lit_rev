# Loading source files
load(file = "0_input/0_rawComics.RData")

# Loading libraries requires
require(stringdist)

# ----- Extracting the required attributes -----
publishers <- rawComics[, c("ID","Publisher")]
names(publishers) <- c("id.issue", "publisher.raw")
publishers$publisher.raw <- as.character(publishers$publisher.raw)
rm(rawComics)

# ===== Deduplication by string edition =====
# --- Finding unique names ---
pub.names <- data.frame(table(publishers$publisher.raw))
names(pub.names) <- c("publisher.raw", "issues.count")
pub.names$publisher <- iconv(from = "UTF-8", to = "ASCII//TRANSLIT",
                             x = pub.names$publisher.raw)
pub.names$publisher <- tolower(pub.names$publisher)
pub.names <- pub.names[, c(1,3,2)]

# --- Formating for specific signs ---
pub.names$publisher <- gsub(pattern = "[:]", replacement = "h",
                            x = pub.names$publisher)
pub.names$publisher <- gsub(pattern = "[&]", replacement = " and ",
                            x = pub.names$publisher)
pub.names$publisher <- gsub(pattern = "[+]", replacement = " plus ",
                            x = pub.names$publisher)
pub.names$publisher <- gsub(pattern = "[!]", replacement = " ",
                            x = pub.names$publisher)

# --- Creation of working variables ---
pub.names$pre <- NA
pub.names$pos <- NA
pub.names$pre.count <- NA
pub.names$pos.count <- NA
pub.names$pre.pub <- NA
pub.names$pos.pub <- NA
pub.names$col <- FALSE
pub.names$part <- FALSE
pub.names$col.1 <- NA
pub.names$col.2 <- NA

# --- Looking twice for collaborations ---
for (j in 1:2) {
  # Looking for possible collaboration signs
  index <- grep(pattern = "[/-]", x = pub.names$publisher)
  # Extracting pre-sign/post-sign part
  pub.names$pre[index] <- gsub(pattern = "(.*)(/|-)(.*)", replacement = "\\1",
                               x = pub.names$publisher[index])
  pub.names$pos[index] <- gsub(pattern = "(.*)(/|-)(.*)", replacement = "\\3",
                               x = pub.names$publisher[index])
  # Comparing with other publisher names
  pub.names$pre.pub[index] <- is.element(el = pub.names$pre[index],
                                         set = pub.names$publisher)
  pub.names$pos.pub[index] <- is.element(el = pub.names$pos[index],
                                         set = pub.names$publisher)
  # Establishing collaborations
  pub.names$col[index] <- pub.names$pos.pub[index] & pub.names$pre.pub[index]
  pub.names$part[index] <- xor(pub.names$pos.pub[index], pub.names$pre.pub[index])
  # Splitting the index
  index.col <- which(x = pub.names$col == TRUE)
  index <- index[which(is.element(index, c(index.col)) == FALSE)]
  # Processing collaborations
  pub.names$col.1[index.col] <- pub.names$pre[index.col]
  pub.names$col.2[index.col] <- pub.names$pos[index.col]
  pub.names$publisher[index.col] <- "COLLABORATION"
  
  # Extracting pre-sign/post-sign part
  pub.names$pre[index] <- gsub(pattern = "(^| |.* )(.*)[/-](.*)",
                               replacement = "\\2",
                               x = pub.names$publisher[index])
  pub.names$pos[index] <- gsub(pattern = "(.*)[/|-]([[:alnum:]]+)( .*|$)",
                               replacement = "\\2",
                               x = pub.names$publisher[index])
  # measuring extract parts
  pub.names$pre.count[index] <- nchar(pub.names$pre[index])
  pub.names$pos.count[index] <- nchar(pub.names$pos[index])
  # Processing no collaborations
  for (i in index) {
    if (min(pub.names$pre.count[i],pub.names$pos.count[i]) < 5 ) {
      pub.names$publisher[i] <- gsub(pattern = "[/-]", replacement = "",
                                     x = pub.names$publisher[i])
        paste(pub.names$pre[i], pub.names$pos[i], sep = "") 
    } else {
      pub.names$publisher[i] <- gsub(pattern = "[/-]", replacement = " ",
                                     x = pub.names$publisher[i])
    }
  }
}

# --- Removing signs left ---
pub.names$publisher <- gsub(pattern = "[[:punct:]]", replacement = "",
                            x = pub.names$publisher)
# --- Space-formatting ---
pub.names$publisher <- gsub(pattern = "[[:space:]]+", replacement = " ",
                            x = pub.names$publisher)
pub.names$publisher <- trimws(x = pub.names$publisher, which = "both")

# --- Replacing the names and merging additional information ---
pub.names <- pub.names[, c("publisher.raw", "publisher",
                           "col", "part",
                           "col.1", "col.2")]
names(pub.names) <- c("publisher.raw", "publisher.pre",
                      "ind.col", "ind.col.par",
                      "col.1", "col.2")

# --- Merging the names into the publisher dataset ---
publishers <- merge(x = publishers, y = pub.names, by = "publisher.raw")

# ===== Deduplication by string distance =====
# Extracting unique publisher names
pub.names <- data.frame(table(publishers$publisher.pre))
names(pub.names) <- c("publisher.pre", "frequency")
pub.names$publisher <- as.character(pub.names$publisher.pre)
# Crating comparison attributes
pub.names$m.ind <- NA
pub.names$m.nam <- NA
pub.names$m.std <- NA
pub.names$m.freq <- NA

# Checking every entry for matches
for (i in 1:nrow(pub.names)) {
  temporal <- stringdistmatrix(a = pub.names$publisher[i],
                               b = pub.names$publisher.pre,
                               method = "jw")
  temporal[i] <- 1
  index <- which(temporal < 0.20)
  if (length(index) > 0) {
    temporal <- data.frame(index, pub.names$publisher[index], temporal[index],
                           stringsAsFactors = FALSE)
    temporal <- temporal[with(temporal, order(temporal.index.)), ]
    # Storing the three more similar names
    pub.names$m.ind[i] <- temporal[1,1]
    pub.names$m.nam[i] <- temporal[1,2]
    pub.names$m.std[i] <- temporal[1,3]
    pub.names$m.freq[i] <- pub.names$frequency[temporal[1,1]]
  }
}
pub.names <- pub.names[, c("publisher.pre", "publisher", "frequency", 
                           "m.ind","m.nam", "m.std", "m.freq")]

# --- Selection of similar names ---
# Definition of comparison attributes
pub.names$ratio <- round(pub.names$m.freq/pub.names$frequency, 4)
pub.names$d.char <- with(pub.names,
                         nchar(m.nam, keepNA = T)-nchar(publisher, keepNA = T))
pub.names$com <- with(pub.names,
                      d.char^3/m.std^3)
pub.names$ind <- as.integer(row.names(pub.names))
pub.names$pos <- pub.names$ind-pub.names$m.ind

# Removing possible matches with lower frequencies
index <- which(pub.names$ratio < 1)
pub.names[index, c(4:12)] <- NA
# Removing non-similar names with higher frequencies
index <- which(pub.names$ratio > 1 & pub.names$m.std < 0.1)
pub.names$publisher[index] <- pub.names$m.nam[index]
pub.names[index, c(4:12)] <- NA
# Removing names with equal frequencies but less characters
index <- with(pub.names, 
              which(ratio == 1 & d.char < 0))
pub.names[index, c(4:12)] <- NA
# Removing non-similar names with equal frequencies
index <- with(pub.names, 
              which(ratio == 1 & d.char > 0 & m.std < 0.12))
pub.names$publisher[index] <- pub.names$m.nam[index]
pub.names[index, c(4:12)] <- NA
# Looking for biggest changes
index <- with(pub.names, 
              which(d.char >= 4 & com > 35000))
pub.names$publisher[index] <- pub.names$m.nam[index]
pub.names[index, c(4:12)] <- NA
# Final selection
index <- with(pub.names, 
              which(ratio == 1 & d.char == 0 & pos < 0 & pos > -250))
pub.names$publisher[index] <- pub.names$m.nam[index]
pub.names[index, c(4:12)] <- NA

# --- Final merge --- 
# Selecting attributes  
pub.names <- pub.names[, c("publisher.pre", "publisher")]
publishers <- merge(x = publishers, y = pub.names, by = "publisher.pre")

# Creation of publisher ID
id <- data.frame(sort(unique(publishers$publisher)), stringsAsFactors = FALSE)
names(id) <- "publisher"
id$id.publisher <- 1:nrow(id)
# Merge of the publisher ID
publishers <- merge(x = publishers, y = id, by = "publisher")
# Restructuring the publishers datasert
publishers <- publishers[, c("id.publisher", "publisher", "id.issue",
                             "ind.col", "ind.col.par", "col.1", "col.2",
                             "publisher.raw")]

# ----- Restoring colaboration data -----
# Correcting collaborations 
index <- which(publishers$ind.col == TRUE)
publishers.n <- publishers[-index, ] 
publishers.y <- publishers[index, ]

publishers.y$id.publisher <- NA
publishers.y$publisher <- paste(publishers.y$col.1,
                                publishers.y$col.2,
                                sep = "/")
publishers <- rbind(publishers.y, publishers.n)

# Correcting partial collaborations 
index <- which(publishers$ind.col.par == TRUE)
publishers.n <- publishers[-index, ] 
publishers.y <- publishers[index, ]
# Manual removing of possible collaborations
# Confirmed collaborations
index <- which(publishers.y$publisher == "chaos midnight holdings llc")
publishers.y$publisher[index] <- "chaos/midnight"
publishers.y$col.1[index] <- "chaos"
publishers.y$col.2[index] <- "midnight"
publishers.y$ind.col[index] <- TRUE
publishers.y$id.publisher[index] <- NA
index <- which(publishers.y$publisher == "tugboat teenage dinosaur sparkplug")
publishers.y$publisher[index] <- "tugboat/teenage dinosaur/sparkplug"
publishers.y$col.1[index] <- "sparkplug"
publishers.y$col.2[index] <- "tugboat/teenage dinosaur"
publishers.y$ind.col[index] <- TRUE
publishers.y$id.publisher[index] <- NA
index <- which(publishers.y$publisher == "tugboat sparkplug")
publishers.y$publisher[index] <- "tugboat/teenage dinosaur/sparkplug"
publishers.y$col.1[index] <- "sparkplug"
publishers.y$col.2[index] <- "tugboat/teenage dinosaur"
publishers.y$ind.col[index] <- TRUE
publishers.y$id.publisher[index] <- NA
# Added to other editorials
index <- which(publishers.y$publisher == "semana negra vanguard")
publishers.y$publisher[index] <- "vanguard"
publishers.y$id.publisher[index] <- 4375
index <- which(publishers.y$publisher == "ediciones recreativas editorial novaro")
publishers.y$publisher[index] <- "editorial novaro"
publishers.y$id.publisher[index] <- 1496
# Merging datasets
publishers <- rbind(publishers.y, publishers.n)
publishers$ind.col.par <- NULL
publishers <- publishers[with(publishers, order(id.publisher, id.issue)), ]

# ===== Storing the dataset as RData file =====
# Removing temporary variables
rm(publishers.n)
rm(publishers.y)
rm(names)
rm(index)
# Storing dataset
save(list = "publishers", file = "1_blackbox/11_publishers.RData")
