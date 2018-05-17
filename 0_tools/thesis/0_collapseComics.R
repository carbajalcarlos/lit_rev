# The following script searches for the variations in the comic issues and exports a dataset with the duplicates,
# and their respective first version

### Opening raw data files --------------------------------------------------------------------------------------

# -> Loading raw data from a tsv file
#rawComics <- read.delim(file = "D:/Code/R/0_raw/comic_base.tsv", encoding = "UTF-8")
# -> Loading raw data from RData file
load(file = "0_input/rawComics.RData")

### Extracting the possible duplicated version ------------------------------------------------------------------

# subsetting the relevant columns for the collapse
collapseComics <- subset(x = rawComics, select = c("Issues_Title", "IssueNum", "CoverDate", "Publisher", "ID"))
# This command, preserves the index from the raw version of the data
collapseComics$index <- as.integer(row.names.data.frame(collapseComics))

# Creeating a unique name to compare title + iisue num + date + publisher
collapseComics$unique.title <- paste(collapseComics$Issues_Title,
                                     collapseComics$IssueNum,
                                     collapseComics$CoverDate,
                                     collapseComics$Publisher)
# Resorting the dataframe by the Unique Title first, and then by ID number to provide a hierarchy
collapseComics <- collapseComics[with(collapseComics, order(unique.title, ID)), ]
# Detecting duplicated titles
# NOTE: This command doesn't consider the first apperance of a dupplicated issue as a duplicate itself.
collapseComics$dup.right <- duplicated(x = collapseComics$unique.title)
# By adding the argument fromLast the first apperance starting from bottom-up is not consider a duplicate,
# then the both commands are united to indicate all duplicated-related issues
collapseComics$dup.all <- collapseComics$dup.right | duplicated(x = collapseComics$unique.title, fromLast = TRUE)
# Subsetting all the duplicated-related issues
collapseComics <- subset(x = collapseComics, subset = dup.all == TRUE)
# Creating a first-version index
# NOTE: All the issues set as FALSE from the initial duplicated search are consider as the first version
index <- grep(pattern = FALSE, x = collapseComics$dup.right)
# Remmoving non-relevant columns
collapseComics <- subset(x = collapseComics, select = c("index", "ID", "unique.title"))
# Creation of the new columns for the first version ID and INDEX
collapseComics$firstVersionIndex <- NA
collapseComics$firstVersionID <- NA
# Creation a counter variable to register the issue at hand
counter <- 1

# The following for-loop goes throughout al the entries and perfom two tasks at each case
for (i in 1:length(collapseComics[,1])) {
  # The first task is to state if the issue at hand is original or is a derived version
  # Comparions between the actual index with the index of the current first version
  # NOTE: for this operation the results are stored in the firstVersionID column
  ifelse(test = i == index[counter],
         # If the current index and the current first version index matches, NA is stored
         yes = collapseComics$firstVersionID[i] <- NA,
         # If the indexes doesn't match, the current first verson ID is stored 
         no = collapseComics$firstVersionID[i] <- collapseComics$ID[index[counter]])
  # Under the same logic the first version index is stored in the fistVersionIndex column
  ifelse(test = i == index[counter],
         yes = collapseComics$firstVersionIndex[i] <- NA,
         no = collapseComics$firstVersionIndex[i] <- collapseComics$index[index[counter]])
  # To prevent a error in the last position of the counter vector, this if is implemented
  if (counter < length(index)) {
    # This if compares the next iteration index, with the next first version index
    # if the next iteration corresponds to the first version index of the next issue,
    # the counter is updated to the next issue
    if (i+1 == (index[counter+1])){
      counter <- counter+1
    }
  }
}

# Resort the data frame by ID
collapseComics <- collapseComics[with(collapseComics, order(index)), ]
# The resulting list is stored as a RData file
save(list = "collapseComics", file = "0_output/collapseComics.RData")
# The resulting list is stores as a csv file
#write.csv(x = collapseComics, file = "0_output/collapseComics.csv", fileEncoding = "UTF-8")

# Remove temporary variables
rm(rawComics)
rm(counter)
rm(i)
rm(index)