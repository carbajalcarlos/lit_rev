# ===== Initialization =====
# Installing required libraries
if (is.element("impute", installed.packages()[,1]) == FALSE) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("impute")
}
if (is.element("missForest", installed.packages()[,1]) == FALSE) { install.packages("missForest") }
# Loading the required libraries
require(impute)
require(missForest)
# Loading required datasets
load(file = "1_blackbox/0_rareComics.RData")
load(file = "1_blackbox/0_rareSales.RData")

# ===== Formation of features datasets =====
# ----- Formation of comics features dataset
features.comics <- rare.comics[, c("issue.num", "cover.date", "cover.price", "print", "index.local", "comic.id", "year.start", "year.end", "year.end", "genre.count")]
# Spliting the date in year and month components
features.comics$year <- as.integer(format(features.comics$cover.date,"%Y"))
features.comics$month <- as.integer(format(features.comics$cover.date,"%m"))
# Removing auxiliary columns
features.comics <- features.comics[, c("issue.num", "cover.price", "print", "index.local", "comic.id", "year", "month", "year.start", "year.end", "year.end", "genre.count")]
# Printing the summary of the comics dataset
print("Comics features subset")
print(summary(features.comics))

# ----- Formation of sales features dataset
features.sales <- rare.sales[, c("index.local", "issue.num", "date", "price", "units.sold", "type", "rank")]
# Creating a logical vector from the type
features.sales$is.book <- ifelse(test = features.sales$type == "book", yes = TRUE, no = FALSE)
# Spliting the date in year and month components
features.sales$year <- as.integer(format(features.sales$date,"%Y"))
features.sales$month <- as.integer(format(features.sales$date,"%m"))
# Removing auxiliary columns
features.sales <- features.sales[, c("index.local", "issue.num", "year", "month", "price", "units.sold", "is.book", "rank")]
# Printing the summary of the comics dataset
print("Sales features subset")
print(summary(features.sales))

# ===== Inputation algorithms =====
# Imputing issues to the sales dataset using several algorithms
index <- unique(x = features.sales$year)
sales.mf <- data.frame()
sales.knn <- data.frame()
# Imputing by years subsets
for (i in index) {
  sub.imp <- subset(x = features.sales, subset = year == i)
  # missForest algorithm
  time <- as.numeric(Sys.time())
  imputation.mf <- missForest(xmis = sub.imp)
  imputation.mf <- as.data.frame(x = imputation.mf$ximp)
  time <- as.numeric(Sys.time() - time)
  # Saving the results and including the processing time
  imputation.mf$time <- time 
  sales.mf <- rbind(sales.mf, imputation.mf)
  # KNN algorithm
  time <- as.numeric(Sys.time())
  imputation.knn <- impute.knn(data = as.matrix(sub.imp))
  imputation.knn <- as.data.frame(imputation.knn$data)
  time <- as.numeric(Sys.time() - time)
  # Saving the results and including the processing time
  imputation.knn$time <- time 
  sales.knn <- rbind(sales.knn, imputation.knn)
  # Keeping track of the progress
  print(paste("Finished year:", i, "Last year:", max(index)))
}
# Calculating procesing time
time.s.mf <- sum(unique(sales.mf$time))/60
sales.mf$time <- NULL
time.s.knn <- sum(unique(sales.knn$time))/60
sales.knn$time <- NULL
# Manual imputation
sales.man <- features.sales
sales.man$issue.num <- ifelse(test = is.na(sales.man$issue.num) == TRUE,
                            yes = 1, no = sales.man$issue.num)

# Imputing dates to the comics dataset using several algorithms
block.size <- 5000
blocks <- ceiling(nrow(features.comics)/block.size)
comics.mf <- data.frame()
comics.knn <- data.frame()
for (i in 1:blocks) {
  # Creating data block to process
  lim.low <- ((i-1)*block.size)+1
  lim.high <- ifelse(test = i == blocks, 
                     yes = nrow(features.comics),
                     no = block.size*i)
  sub.imp <- features.comics[lim.low:lim.high, ]
  # missForest algorithm
  time <- as.numeric(Sys.time())
  imputation.mf <- missForest(xmis = sub.imp)
  imputation.mf <- as.data.frame(x = imputation.mf$ximp)
  time <- as.numeric(Sys.time() - time)
  # Saving the results and including the processing time
  imputation.mf$time <- time 
  comics.mf <- rbind(comics.mf, imputation.mf)
  # KNN algorithm
  time <- as.numeric(Sys.time())
  imputation.knn <- impute.knn(data = as.matrix(sub.imp))
  imputation.knn <- as.data.frame(imputation.knn$data)
  time <- as.numeric(Sys.time() - time)
  # Saving the results and including the processing time
  imputation.knn$time <- time 
  comics.knn <- rbind(comics.knn, imputation.knn)
  # Keeping track of the progress
  print(paste("Finished block:", i,"de", blocks))
}
# Calculating procesing time
time.c.mf <- sum(unique(comics.mf$time))/60
comics.mf$time <- NULL
time.c.knn <- sum(unique(comics.knn$time))/60
comics.knn$time <- NULL
# Manual imputation
comics.man <- features.comics
comics.man$year <- ifelse(test = is.na(comics.man$year) == TRUE,
                          yes = min(comics.man$year, na.rm = TRUE)-1, no = comics.man$year)
comics.man$month <- ifelse(test = is.na(comics.man$month) == TRUE,
                           yes = 1, no = comics.man$month)

# ===== Graphic output =====
# Sales imputation
# Original
png(filename = "3_images/0_salesOriginal.png", width = 1200, height = 900)
hist(features.sales$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(.25,.25,.25,0.50), 
     main = "Original issue distribution", xlab = "Issue number")
legend(x = 80, y = 0.1, legend = "original", lwd = 4, col = rgb(.25,.25,.25,0.50))
box()
dev.off()
# MF output
png(filename = "3_images/1_salesImputationMF.png", width = 1200, height = 900)
hist(features.sales$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(.25,.25,.25,0.50), 
     main = "Output distribution using the missForest imputation algorithm", xlab = "Issue number")
hist(sales.mf$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(1,0,0,0.50), add = TRUE)
legend(x = 80, y = 0.1, legend = c("original", "missForest"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(1,0,0,0.50)))
box()
dev.off()
# KNN output
png(filename = "3_images/2_salesImputationKNN.png", width = 1200, height = 900)
hist(features.sales$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(.25,.25,.25,0.50), 
     main = "Output distribution using the KNN imputation algorithm", xlab = "Issue number")
hist(sales.knn$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(0,1,0,0.50), add = TRUE)
legend(x = 80, y = 0.1, legend = c("original", "knn"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(0,1,0,0.50)))
box()
dev.off()
# Manual output
png(filename = "3_images/3_salesImputationMAN.png", width = 1200, height = 900)
hist(features.sales$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(.25,.25,.25,0.50), 
     main = "Output distribution using the manual imputation algorithm", xlab = "Issue number")
hist(sales.man$issue.num, xlim = c(1, 100), breaks = 200000, freq = FALSE, col = rgb(0,0,1,0.50), add = TRUE)
legend(x = 80, y = 0.1, legend = c("original", "manual"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(0,0,1,0.50)))
box()
dev.off()

# Comics imputation
# Original
png(filename = "3_images/4_datesOriginal.png", width = 1200, height = 900)
hist(as.integer(features.comics$year), breaks = 25, freq = FALSE, col = rgb(.25,.25,.25,0.50), xlim = c(1994,2015), 
     main = "Original dates distribution", xlab = "Publishing year")
legend(x = 2012, y = 0.1, legend = "original", lwd = 4, col = rgb(.25,.25,.25,0.50))
box()
dev.off()
# MF output
png(filename = "3_images/5_datesImputationMF.png", width = 1200, height = 900)
hist(as.integer(features.comics$year), breaks = 25, freq = FALSE, col = rgb(.25,.25,.25,0.50), xlim = c(1994,2015), 
     main = "Output distribution using the missForest imputation algorithm", xlab = "Publishing year")
hist(as.integer(comics.mf$year), breaks = 25, freq = FALSE, col = rgb(1,0,0,0.50), add = TRUE)
legend(x = 2012, y = 0.1, legend = c("original", "missForest"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(1,0,0,0.50)))
box()
dev.off()
# KNN output
png(filename = "3_images/6_datesImputationKNN.png", width = 1200, height = 900)
hist(as.integer(features.comics$year), breaks = 25, freq = FALSE, col = rgb(.25,.25,.25,0.50), xlim = c(1994,2015), 
     main = "Output distribution using the KNN imputation algorithm", xlab = "Publishing year")
hist(as.integer(comics.knn$year), breaks = 25, freq = FALSE, col = rgb(0,1,0,0.50), add = TRUE)
legend(x = 2012, y = 0.1, legend = c("original", "KNN"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(0,1,0,0.50)))
box()
dev.off()
# Manual output
png(filename = "3_images/7_datesImputationMAN.png", width = 1200, height = 900)
hist(as.integer(features.comics$year), breaks = 25, freq = FALSE, col = rgb(.25,.25,.25,0.50), xlim = c(1994,2015), 
     main = "Output distribution using the manual imputation algorithm", xlab = "Publishing year")
hist(as.integer(comics.man$year), breaks = 25, freq = FALSE, col = rgb(0,0,1,0.50), add = TRUE)
legend(x = 2012, y = 0.1, legend = c("original", "MAN"), lwd = 4, col = c(rgb(.25,.25,.25,0.50), rgb(0,0,1,0.50)))
box()
dev.off()

# ==== Inseration of the values generated ====
# New dataframe with missing dates generated
medium.comics <- rare.comics
index <- which(is.na(medium.comics$cover.date) == TRUE)
medium.comics$date.mf <- medium.comics$cover.date
medium.comics$date.knn <- medium.comics$cover.date
medium.comics$date.man <- medium.comics$cover.date
medium.comics$date.mf[index] <- as.POSIXct(paste(round(comics.mf$year[index],0),"-", round(comics.mf$month[index],0),"-01", sep = ""), tz = "CET")
medium.comics$date.knn[index] <- as.POSIXct(paste(round(comics.knn$year[index],0),"-", round(comics.knn$month[index],0),"-01", sep = ""), tz = "CET")
medium.comics$date.man[index] <- as.POSIXct(paste(round(comics.man$year[index],0),"-", round(comics.man$month[index],0),"-01", sep = ""), tz = "CET")
# New dataframe with missing issue generated
medium.sales <- rare.sales
index <- which(is.na(medium.sales$issue.num) == TRUE)
medium.sales$issue.mf <- medium.sales$issue.num
medium.sales$issue.knn <- medium.sales$issue.num
medium.sales$issue.man <- medium.sales$issue.num
medium.sales$issue.mf[index] <- round(sales.mf$issue.num[index])
medium.sales$issue.knn[index] <- round(sales.knn$issue.num[index])
medium.sales$issue.man[index] <- round(sales.man$issue.num[index])
# Calculating the total processing time
total.time <- round(time.c.knn + time.c.mf + time.s.knn + time.s.mf, 2)

# ===== Storing output ======
# Removing temporary variables
#rm(comics.knn)
#rm(comics.man)
#rm(comics.mf)
rm(features.comics)
rm(features.sales)
rm(imputation.knn)
rm(imputation.mf)
rm(rare.comics)
rm(rare.sales)
#rm(sales.knn)
#rm(sales.man)
#rm(sales.mf)
rm(sub.imp)
rm(block.size)
rm(blocks)
rm(i)
rm(index)
rm(lim.high)
rm(lim.low)
rm(time)
# Storing as RData file
save(list = "medium.sales", file = "1_blackbox/2_mediumSales.RData")
save(list = "medium.comics", file = "1_blackbox/2_mediumComics.RData")
  save.image(file = "Miscellaneous/1_wsImputation.RData")
