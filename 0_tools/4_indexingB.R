# ===== Loading =====
# Loading required libraries
require(stringdist)
# Loading datasets
load(file = "1_blackbox/2_mediumComics.RData")
load(file = "1_blackbox/2_mediumSales.RData")

# ==== Preparation of working variables ====
# Subsetting sales sample NOTE: this step is only required to the development of the model
working.set <- medium.sales 
#set.seed(69)
#index <- sample(x = 1:nrow(medium.sales), size = 1000)
#working.set <- medium.sales[index, ] # Defining the working set

# ==== Indexing cycle by price levels ====
# Creating required objects
index.array <- data.frame() # Creation of the storage matches array
prices <- sort(unique(working.set$price)) # Extracting prices present in the working set
# Restarting timer
elapsed.time <- as.numeric(Sys.time())
# Indexing sequence
for (i in prices) {
  i <- as.numeric(i) # simplification of functions
  # Subsetting by price levels
  sub.sales <- subset(x = working.set, subset = price == i)
  sub.comics <- subset(x = medium.comics, subset =  price.min >= (i-0.05) & price.max <= (i+0.05))
  # Spliting the sales subset to a manageble size (if required)
  block.size <- 500
  division <- ceiling(nrow(sub.sales)/block.size)
  for (j in 1:division) {
    # Defining the block size
    lim.low <- ((j-1)*block.size)+1
    lim.high <- ifelse(test = j == division, 
                       yes = nrow(sub.sales),
                       no = block.size*j)
    par.sales <- sub.sales[lim.low:lim.high, ]
    # Mesuring string distance
    sub.index <- stringdistmatrix(a = par.sales$title, b = sub.comics$title, method = "cosine", q = 3)
    sub.index <- data.frame(which(sub.index < 0.50, arr.ind = TRUE))
    # If no matches were found, go to the next point
    if (length(sub.index[,1])==0) { next }
    # Renaming indexes
    sub.index$row <- par.sales$index.local[sub.index$row] # Sales index
    sub.index$col <- sub.comics$index.local[sub.index$col] # Comic index
    # Storing the iteration results
    index.array <- rbind(index.array, sub.index)
    # Printing progress
  }
  progress <- round(length(unique(index.array$row))/nrow(working.set)*100, digits = 2)
  print(paste("$", i," - ", progress, "% - ", round(as.numeric(Sys.time()) - elapsed.time, digits = 2), sep = ""))
}
elapsed.time <- as.numeric(Sys.time()) - elapsed.time # Elapsed time at the end of the process
print(elapsed.time)

index.array <- match.array

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
#rm(index)
rm(j)
rm(lim.high)
rm(lim.low)
rm(prices)
# Storing as RData file
save(list = "index.array", file = "1_blackbox/3_indexArrayB.RData")
save(list = "index.array", "working.set", "progress", "elapsed.time", file = "Miscellaneous/2_wsIndexingB.RData")
