# ===== Loading =====
# Loading required libraries
require(stringdist)
# Loading datasets
load(file = "1_blackbox/2_mediumComics.RData")
load(file = "1_blackbox/2_mediumSales.RData")
load(file = "1_blackbox/4_matches.RData")

# ==== Creation of missing datasets ====
# Removing matched observations
medium.comics$matched <- is.element(el = medium.comics$index.local,
                                    set = matches$c.id)
missing.comics <- subset(x = medium.comics, subset = matched == FALSE)
medium.sales$matched <- is.element(el = medium.sales$index.local,
                                    set = matches$s.id)
missing.sales <- subset(x = medium.sales, subset = matched == FALSE)
# Resetting local index
row.names(missing.comics) <- 1:nrow(missing.comics)
missing.comics$index.local <- as.integer(row.names.data.frame(missing.comics))
row.names(missing.sales) <- 1:nrow(missing.sales)
missing.sales$index.local <- as.integer(row.names.data.frame(missing.sales))
# Storing datasets
save(list = c("missing.comics", "missing.sales"),
     file = "1_blackbox/6_missingDatasets.RData")

# ==== Preparation of working variables ====
# Defining the working set
working.setB <- missing.comics
# Subsetting sales sample NOTE: this step is only required to testing
#set.seed(69)
#working.setB <- missing.comics[sample(x = 1:nrow(missing.comics), size = 1000), ]

# Preparing blocking key
borders <- c(min(missing.comics$issue.num, na.rm = TRUE),
            1:10,
            14,20,30,50,100,200,500,
            max(missing.comics$issue.num, na.rm = TRUE))

# ==== Indexing cycle by issue number ====
# Indexing sequence
index.arrayB <- data.frame()
elapsed.timeB <- as.integer(Sys.time())
progressB.pre <- 0
for (i in 1:(length(borders)-1)) {
  # Extracting info from the observation at hand
  sub.comics <- subset(x = working.setB, subset = issue.num >= borders[i])
  sub.comics <- subset(x = sub.comics, subset = issue.num < borders[i+1])
  sub.sales <- subset(x = missing.sales,
                      subset = issue.num >= borders[i] | is.na(issue.num) == TRUE)
  sub.sales <- subset(x = sub.sales,
                      subset = issue.num < borders[i+1] | is.na(issue.num) == TRUE)
  # Spliting the sales subset to a manageble size (if required)
  block.size <- 1000
  division <- ceiling(nrow(sub.comics)/block.size)
  for (j in 1:division) {
    # Defining the block size
    if (division != 1) {
      lim.low <- ((j-1)*block.size)+1
      lim.high <- ifelse(test = j == division, 
                         yes = nrow(sub.comics),
                         no = block.size*j)
      par.comics <- sub.comics[lim.low:lim.high, ]
    } else {
      par.comics <- sub.comics
    }
    # Measuring string distance
    sub.index <- stringdistmatrix(a = par.comics$title, b = sub.sales$title,
                                  method = "cosine", q = 3)
    sub.index <- data.frame(which(sub.index < 0.50, arr.ind = TRUE))
    # If no matches were found, go to the next point
    if (length(sub.index[,1])==0) { next }
    # Renaming indexes
    sub.index$row <- par.comics$index.local[sub.index$row] # Sales index
    sub.index$col <- sub.sales$index.local[sub.index$col] # Comic index
    # Storing the iteration results
    index.arrayB <- rbind(index.arrayB, sub.index)
    progressB <- round(length(unique(index.arrayB$row))/nrow(working.setB)*100, 2)
    # Printing progressB is there is a difference of 2.5%  from the previous print
    if (progressB - progressB.pre >= 2.5) {
      progressB.pre <- progressB
      seconds <- (as.integer(Sys.time()) - elapsed.timeB)
      print(paste("Current issue number: ", i, ", ",
                  progressB, "% observations found, ",
                  round(seconds/length(unique(index.arrayB$row)), 2),
                  " seconds/observation", 
                  sep = ""), quote = FALSE)
    }
  }
}
# Elapsed time at the end of the process
elapsed.timeB <- as.numeric(Sys.time()) - elapsed.timeB
print(ifelse (test = elapsed.timeB > 60, 
              yes = paste("Execution time:", round(elapsed.timeB/60, 2), "min"),
              no = paste("Execution time:", round(elapsed.timeB, 2), "seg")))

# ==== Storing the indexing array results ====
# Removing temporary variables
rm(matches)
rm(medium.comics)
rm(medium.sales)
rm(par.comics)
rm(missing.comics)
rm(missing.sales)
rm(sub.comics)
rm(sub.index)
rm(sub.sales)
rm(block.size)
rm(borders)
rm(division)
rm(i)
rm(j)
rm(lim.high)
rm(lim.low)
rm(progressB.pre)
rm(seconds)

# Storing as RData file
save(list = "index.arrayB", file = "1_blackbox/7_IndexArrayB.RData")
save(list = "index.arrayB", "working.setB", "progressB", "elapsed.timeB", file = "Miscellaneous/4_wsIndexingB.RData")
