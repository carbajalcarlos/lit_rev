# Loading requited data sources
load(file = "2_output/comics.RData")
load(file = "1_blackbox/10_issues.RData")

#loading required libraries
require(reshape2)
require(Hmisc)

# Use of creative teams id
cdb <- merge(x = cdb,
             y = issi[, c("id.issue", "id.team.creative")],
             by = "id.issue")
cdb <- cdb[, c(1:4, 74, 5:73)]
participations_ <- merge(x = participations,
                         y = comics[, c("sales.year", "id.issue", "sales.price", "units.sold", "price.2014")],
                        by = "id.issue")
# ===== Creation of a analysis dataset =====
# Including identificators
analysis <- cdb[, c(1:6)]
# Introducing dependent variable 
analysis$revenue <- cdb$units.sold*cdb$price
analysis$price <- cdb$price.avg
# Using units sold as proxy for the circulation
analysis$units.sold <- cdb$units.sold
# Including number of genres the issue is classified
analysis$no.genres <- cdb$genre.count
# Including number of creative contributors
analysis$no.creators <- with(data = cdb,
                             expr = ceiling((count.artists+count.writers)*contributors.count))
# Including single creator indicator
analysis$single.creator <- analysis$no.creators == 1
# Adding reference year 
analysis$year <- cdb$year
# Adding additional information 
analysis.y <- analysis[which(is.na(analysis$id.team.creative) == FALSE), ]
analysis.n <- analysis[which(is.na(analysis$id.team.creative) == TRUE),]

analysis.n <- merge(x = analysis.n, 
                  y = contributors[, c("id.contributor", "start")], 
                  by = "id.contributor")
names(analysis.n)[which(names(analysis.n) == "start")] <- "min.year" 
test <- recast(data = participations_,
               formula = id.team.creative + id.contributor ~ .,
               id.var = c("id.team.creative", "id.contributor"),
               measure.var = "sales.year", min, na.rm = TRUE)
names(test)[3] <- "min.year"
test <- recast(data = participations_,
               formula = id.team.creative ~ .,
               id.var = c("id.team.creative", "id.contributor"),
               measure.var = "sales.year", min, na.rm = TRUE)
names(test)[2] <- "min.year"
analysis.y <- merge(x = analysis.y, y = test, by = "id.team.creative")

analysis <- rbind(analysis.y, analysis.n)
# Including tenure
analysis$tenure <- analysis$year-analysis$min.year +1
# Correcting for missing information
analysis$tenure[which(analysis$tenure < 1)] <- 1

# Removing temporary variables
rm(analysis.y)
rm(analysis.n)
rm(test)

# ===== Aggregating information required by focal time ===== 
# Using a years blocking key
years <- sort(unique(analysis$year))
# Creating necesary attributes
analysis$dev.rev <- NA
analysis$dev.pri <- NA
analysis$hig <- NA
analysis$low <- NA
analysis$gc <- NA
analysis$il <- NA
analysis$ic <- NA
analysis$it <- NA

analysis$ip <- NA
analysis$us <- NA
analysis$cr <- NA

# Creating dataset to store output
analysis.out <- data.frame()
sub.par.con <- subset(x = participations, subset = year == years[1]-1)
for (i in years) {
  # Single contributors datasets
  sub.ana.con <- subset(x = analysis,
                      subset = year == i & is.na(id.team.creative) == TRUE)
  # Teams datasets
  sub.ana.team <- subset(x = analysis,
                         subset = year == i & is.na(id.team.creative) == FALSE)
  # Participations dataset
  sub.par <- subset(x = participations_, subset = sales.year <= i)
  # Publishers datasets
  sub.pub <- subset(x = publishers, subset = year == i)
  sub.com <- subset(x = comics, subset = sales.year == i)
  sub.par.pub <- subset(x = participations_, subset = sales.year == i)
  
  # Reading missing information
  for (j in 1:nrow(sub.ana.con)) {
    sample <- sub.par[which(sub.par$id.contributor == sub.ana.con$id.contributor[j]), ]
    sub.ana.con$dev.rev[j] <- sd(sample$sales.price*sample$units.sold)
    sub.ana.team$dev.pri[j] <- sd(sample$price.avg)
    sub.ana.con$hig[j] <- max(sample$Price)
    sub.ana.con$low[j] <- min(sample$Price)
    sub.ana.con$gc[j] <- sum(apply(sample[49:72], 2, any))
    sub.ana.con$il[j] <- length(which(sub.par.con$id.contributor == sub.ana.con$id.contributor[j]))
    sub.ana.con$ic[j] <- length(which(sub.par.pub$id.contributor == sub.ana.con$id.contributor[j]))
    sub.ana.con$it[j] <- nrow(sample)
    # Publisher information
    
    sub.ana.con$ip[j] <- length(which(sub.pub$id.publisher == sub.ana.con$id.publisher[j]))
    sub.ana.con$us[j] <- sum(sub.com$units.sold[which(sub.com$id.publisher == sub.ana.con$id.publisher[j])])
    sub.ana.con$cr[j]  <- length(unique(sub.par.pub$id.contributor[which(sub.par.pub$id.publisher == sub.ana.con$id.publisher[j])]))
  }
  for (j in 1:nrow(sub.ana.team)) {
    sample <- sub.par[which(sub.par$id.team.creative == sub.ana.team$id.team.creative[j]), ]
    sub.ana.team$dev.rev[j] <- sd(sample$sales.price*sample$units.sold)
    sub.ana.team$dev.pri[j] <- sd(sample$price.avg)
    sub.ana.team$hig[j] <- max(sample$Price)
    sub.ana.team$low[j] <- min(sample$Price)
    sub.ana.team$gc[j] <- sum(apply(sample[49:72], 2, any))
    sub.ana.team$il[j] <- length(which(sub.par.con$id.team.creative == sub.ana.team$id.team.creative[j]))
    sub.ana.team$ic[j] <- length(which(sub.par.pub$id.team.creative == sub.ana.team$id.team.creative[j]))
    sub.ana.team$it[j] <- nrow(sample)
    
    sub.ana.team$ip[j] <- length(which(sub.pub$id.publisher == sub.ana.team$id.publisher[j]))
    sub.ana.team$us[j] <- sum(sub.com$units.sold[which(sub.com$id.publisher == sub.ana.team$id.publisher[j])])
    sub.ana.team$cr[j]  <- length(unique(sub.par.pub$id.contributor[which(sub.par.pub$id.publisher == sub.ana.team$id.publisher[j])]))
  }
  analysis.out <- rbind(analysis.out, sub.ana.con, sub.ana.team)
  sub.par.con <- subset(x = participations_, subset = sales.year == i)
}

# Correcting problems with missing data
analysis.out$il[which(analysis.out$il == 0)] <- 1
analysis.out$ip[which(analysis.out$ip == 0)] <- 1

# Analysis values from the output dataset
analysis.st <- analysis.out[, c("price", "dev.pri", "year")]
names(analysis.st) <- c("st.price", "st.price.dev", "year")
analysis.st$st.circulation <- analysis.out$units.sold
analysis.st$st.genres <- analysis.out$no.genres
analysis.st$workload.inc <- analysis.out$ic/analysis.out$il > 5
analysis.st$st.lowest <- analysis.out$low
analysis.st$single.creator <- analysis.out$single.creator
analysis.st$st.creators <- analysis.out$no.creators
analysis.st$st.genre.exp <- analysis.out$gc
analysis.st$st.team.ext <- analysis.out$it
analysis.st$st.highest <- analysis.out$hig
analysis.st$st.workload <- analysis.out$ic
analysis.st$st.tenure <- analysis.out$tenure
analysis.st$st.tenure2 <- (analysis.out$tenure^2)/100
analysis.st$st.pub.size <- with(analysis.out, ip*us*cr)
analysis.st$y97.99 <- NA
analysis.st$y00.02 <- NA
analysis.st$y03.05 <- NA
analysis.st$y06.08 <- NA
analysis.st$y09.11 <- NA
analysis.st$y12.14 <- NA
# Whitin a year standatization
years <- sort(unique(analysis.st$year))
for (i in years) {
  index <- which(analysis.st$year == i)
  analysis.st$st.price[index] <- scale(analysis.st$st.price[index])
  analysis.st$st.price.dev[index] <- scale(analysis.st$st.price.dev[index])
  analysis.st$st.circulation[index] <- scale(analysis.st$st.circulation[index])
  analysis.st$st.genres[index] <- scale(analysis.st$st.genres[index])
  analysis.st$st.lowest[index] <- scale(analysis.st$st.lowest[index])
  analysis.st$st.creators[index] <- scale(analysis.st$st.creators[index])
  analysis.st$st.genre.exp[index] <- scale(analysis.st$st.genre.exp[index])
  analysis.st$st.team.ext[index] <- scale(analysis.st$st.team.ext[index])
  analysis.st$st.highest[index] <- scale(analysis.st$st.highest[index])
  analysis.st$st.workload[index] <- scale(analysis.st$st.workload[index])
  analysis.st$st.tenure[index] <- scale(analysis.st$st.tenure[index])
  analysis.st$st.tenure2[index] <- scale(analysis.st$st.tenure2[index])
  analysis.st$st.pub.size[index] <- scale(analysis.st$st.pub.size[index])
  analysis.st$y97.99[index] <- i >= 1997 & i <= 1999
  analysis.st$y00.02[index] <- i >= 2000 & i <= 2002
  analysis.st$y03.05[index] <- i >= 2003 & i <= 2005
  analysis.st$y06.08[index] <- i >= 2006 & i <= 2008
  analysis.st$y09.11[index] <- i >= 2009 & i <= 2011
  analysis.st$y12.14[index] <- i >= 2012 & i <= 2014
}

analysis.st$year <- NULL

sd(analysis.st)

rcorr(x = as.matrix(analysis.st), type="pearson")

# Test of linear regresion
tg.model <- lm(st.price ~ st.circulation + st.genres + workload.inc + st.highest + single.creator + st.creators + st.genre.exp + st.team.ext + st.workload + st.tenure + st.tenure2 + st.pub.size,
               data = analysis.st)
summary(tg.model)
# Test of linear regresion
tg.model <- lm(st.price ~ st.circulation + st.genres + workload.inc + st.highest + single.creator + st.creators + st.genre.exp + st.genre.exp*single.creator + st.team.ext + st.workload + st.tenure + st.tenure2 + st.pub.size,
               data = analysis.st)
summary(tg.model)

save(list = c("analysis.out", "analysis.st"), file = "2_output/tgAnalysis.RData")

