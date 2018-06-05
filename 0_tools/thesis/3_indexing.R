# ===== Loading =====
# Loading required libraries
require(stringdist)
# Loading datasets
load(file = "1_blackbox/2_mediumComics.RData")
load(file = "1_blackbox/2_mediumSales.RData")

# ==== Preparation of working variables ====
# Subsetting sales sample NOTE: this step is only required 
# to the development of the model
working.set <- medium.sales 
# Defining the working set
#set.seed(69)
#working.set <- medium.sales[sample(x = 1:nrow(medium.sales), size = 500), ]

# ==== Indexing cycle by price levels ====
# Creating required objects
working.set$year <- as.integer(format(working.set$date, "%Y"))
years <- sort(unique(working.set$year))

# Indexing sequence
index.array <- data.frame()
elapsed.time <- as.integer(Sys.time())
for (i in years) {
  # Extracting info from the observation at hand
  sub.sales <- subset(x = working.set, subset = year == i)
  sub.comics <- subset(x = medium.comics, subset = year.start <= i)
  sub.comics <- subset(x = sub.comics, subset = year.end >= i)
  # Spliting the sales subset to a manageble size (if required)
  block.size <- 1000
  division <- ceiling(nrow(sub.sales)/block.size)
  for (j in 1:division) {
    # Defining the block size
    lim.low <- ((j-1)*block.size)+1
    lim.high <- ifelse(test = j == division, 
                       yes = nrow(sub.sales),
                       no = block.size*j)
    par.sales <- sub.sales[lim.low:lim.high, ]
    # Measuring string distance
    sub.index <- stringdistmatrix(a = par.sales$title, b = sub.comics$title,
                                  method = "cosine", q = 3)
    sub.index <- data.frame(which(sub.index < 0.50, arr.ind = TRUE))
    # If no matches were found, go to the next point
    if (length(sub.index[,1])==0) { next }
    # Renaming indexes
    sub.index$row <- par.sales$index.local[sub.index$row] # Sales index
    sub.index$col <- sub.comics$index.local[sub.index$col] # Comic index
    # Storing the iteration results
    index.array <- rbind(index.array, sub.index)
  }
  # Printing progress
  progress <- round(length(unique(index.array$row))/nrow(working.set)*100, 2)
  seconds <- (as.integer(Sys.time()) - elapsed.time)
  print(paste(i," finished, ", max(years) - i, " years to go, ", 
              progress, "% observations found, ",
              round(seconds/(i - min(years) + 1), 2),
              " seconds/year, ",
              round(seconds/length(unique(index.array$row)), 2),
              " seconds/observation", 
              sep = ""), quote = FALSE)
}
# Elapsed time at the end of the process
elapsed.time <- as.numeric(Sys.time()) - elapsed.time
print(ifelse (test = elapsed.time > 60, 
              yes = paste("Execution time:", round(elapsed.time/60, 2), "min"),
              no = paste("Execution time:", round(elapsed.time, 2), "seg")))

# ==== Storing the indexing array results ====
# Removing temporary variables
rm(par.sales)
rm(medium.comics)
rm(medium.sales)
rm(sub.comics)
rm(sub.index)
rm(sub.sales)
rm(block.size)
rm(division)
rm(i)
rm(j)
rm(lim.high)
rm(lim.low)
rm(seconds)
rm(years)
# Storing as RData file
save(list = "index.array", file = "1_blackbox/3_indexArray.RData")
save(list = "index.array", "working.set", "progress", "elapsed.time", file = "Miscellaneous/2_wsIndexing.RData")
