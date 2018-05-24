
save.set <- bg_df


index <- grep(pattern = ";   ", x = bg_df$title, fixed = TRUE)
bg_df$title[index] <- gsub(pattern = ";   ", replacement = " ", x = bg_df$title[index])


bg_df$title[index]

i <- index[1]

index <- grep(pattern = "^[[:punct:]].*", x = bg_df$title); bg_df$title[index]

pat <- gsub(pattern = "[^[:punct:]]", replacement = "", x = bg_df$title)
pat <- trimws(unlist(strsplit(pat, split = "")), which = "both")
pat <- as.data.frame(x = table(pat), stringsAsFactors = FALSE)

index <- grep(pattern = "?", x = bg_df$title, fixed = TRUE)
temp <- bg_df$title[index]; temp

