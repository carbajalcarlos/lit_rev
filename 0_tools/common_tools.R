
save.set <- bg_df


index <- grep(pattern = ";   ", x = bg_df$title, fixed = TRUE)
bg_df$title[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$title[index])


bg_df$title[index]

i <- index[1]

index <- grep(pattern = "^[[:punct:]].*", x = bg_df$title); bg_df$title[index]

# Lookout of specific characters
index <- grep(pattern = ";", x = bg_df$abstract, fixed = TRUE)
temp <- bg_df$abstract[index]; temp

# List of punctuation signs in an object
pat <- bg_df$abstract
pat <- gsub(pattern = "[^[:punct:]]", replacement = "", x = pat)
pat <- trimws(unlist(strsplit(pat, split = "")), which = "both")
pat <- as.data.frame(x = table(pat), stringsAsFactors = FALSE)

# List of words used in an object
wordlist <- bg_df$abstract
wordlist <- trimws(unlist(strsplit(wordlist, split = " ")), which = "both")
wordlist <- as.data.frame(x = table(wordlist), stringsAsFactors = FALSE)

# Extracting patterns in an object
pat <- bg_df$unique.id
pat <- gsub(pattern = "[[:alpha:]]", replacement = "A", x = pat)
pat <- gsub(pattern = "[[:digit:]]", replacement = "D", x = pat)
pat <- trimws(unlist(strsplit(pat, split = "")), which = "both")
pat <- as.data.frame(x = table(pat), stringsAsFactors = FALSE)
