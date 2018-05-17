# Loading data sources
load(file = "2_output/comics.RData")
load(file = "2_output/participations.RData")
load(file = "2_output/publishers.RData")
load(file = "1_blackbox/9_comics.RData")
# Loading required libraries
require(reshape2)

# ===== Adding information on debut by team =====
teams.cont <- recast(data = participations, 
                     id.var = c("id.team", "id.contributor"),
                     measure.var = "id.issue",
                     formula = id.team + id.contributor ~ ., 
                     length)
names(teams.cont)[3] <- "issue.count"

debut <- recast(data = participations, 
                     id.var = c("id.contributor"),
                     measure.var = "year",
                     formula = id.contributor ~ ., 
                     min)
names(debut)[2] <- "debut.min"

teams.cont <- merge(x = teams.cont, y = debut, by = "id.contributor")
teams.cont <- subset(teams.cont, subset = is.na(id.team) == FALSE)
teams.cont$debut <- as.integer(teams.cont$debut)

# Extracting minimun and mean debut for teams
team.min <- recast(data = teams.cont, 
                   id.var = c("id.team"),
                   measure.var = "debut.min",
                   formula = id.team ~ ., 
                   min)
names(team.min)[2] <- "debut.min"

team.mean <- recast(data = teams.cont, 
                    id.var = c("id.team"),
                    measure.var = "debut.min",
                    formula = id.team ~ ., 
                    mean)
names(team.mean)[2] <- "debut.mean"
# Adding debut data to teams in the cdb
cdb$debut.min <- cdb$debut.mean <- NULL
cdb.y <- merge(x = cdb, y = team.min, by = "id.team")
cdb.y <- merge(x = cdb.y, y = team.mean, by = "id.team")
cdb.y$team.first <- cdb.y$team.last <- NULL

# Adding debut data to single contributors in the cdb
cdb.n <- cdb[which(!is.element(cdb$id.issue, cdb.y$id.issue)), ]
cdb.n <- merge(x = cdb.n, y = debut, by = "id.contributor")
cdb.n$debut.mean <- cdb.n$debut.min
cdb.n$team.first <- cdb.n$team.last <- NULL

# Merging edited dataset 
cdb <- rbind(cdb.y, cdb.n)

# Removing temporary variables
rm(cdb.y)
rm(cdb.n)
rm(debut)
rm(team.mean)
rm(team.min)
rm(teams.cont)

# ===== Calculation of additional attributes =====
# Inserting cover data
names(cdb)[which(names(cdb) == "date")] <- "sales.date"
names(cdb)[which(names(cdb) == "year")] <- "sales.year"
names(cdb)[which(names(cdb) == "month")] <- "sales.month"
cdb <- merge(x = cdb,
             y = participations[duplicated(participations$id.issue) == FALSE,
                                c("id.issue", "year")],
             by = "id.issue")
names(cdb)[which(names(cdb) == "year")] <- "cover.year"
# Correcting minumun years
cdb$cover.year[which(cdb$cover.year < 1997)] <- 1997
cdb$cover.year[which(cdb$cover.year > 2014)] <- 2014
# Changing variable to integer
cdb$debut.min <- as.integer(cdb$debut.min)
cdb$debut.mean <- as.integer(cdb$debut.mean)
# Adding tenure attributes
cdb$tenure.mean <- cdb$sales.year-cdb$debut.mean+1
cdb$tenure.min <- cdb$sales.year-cdb$debut.min+1
# Correcting initial year by the sales data miss
index <- which(names(cdb) == "sales.year")
index <- c(index, which(names(cdb) == "cover.year"))
index <- c(index, which(names(cdb) == "comic.start"))
cdb$comic.start <- apply(cdb[, index], 1, min)
# Calculating the age of the comic line
cdb$comic.duration <- cdb$sales.year - cdb$comic.start

# Calculating team proportions
# Editorial team proportion
cdb$prop.editorial <- cdb$count.editors
# Creative team proportion
index <- which(names(cdb) == "count.writers")
index <- c(index, which(names(cdb) == "count.artists"))
index <- c(index, which(names(cdb) == "count.cover.artists"))
cdb$prop.creative <- apply(cdb[, index], 1, sum)
# Technical team proportion
index <- which(names(cdb) == "count.inkers")
index <- c(index, which(names(cdb) == "count.letterers"))
index <- c(index, which(names(cdb) == "count.colorists"))
index <- c(index, which(names(cdb) == "count.cover.inkers"))
index <- c(index, which(names(cdb) == "count.cover.colorists"))
cdb$prop.technical <- apply(cdb[, index], 1, sum)
# calculation of revenue
cdb$revenue <- cdb$price * cdb$units.sold 

# ===== Adding focal year information ====
# Removing non-focal attributes
cdb <- cdb[, # Identificators
           c("id.issue", "id.comic", "id.publisher",
             "id.team", "id.contributor",
             # Issue description
             "publisher", "title", "issue.num",  
             "is.book", "is.cover.special",
             "var.count", "genre.count",
             "appearances.count", "storylines.count",
             # Dates
             "sales.date", "sales.year", "cover.year",
             "comic.start", "comic.duration",
             # Sales data
             "price", "price.avg", "rank", "units.sold", "revenue",
             # Ownership
             "is.creator.owned", "is.owner.in",
             # Contributors count
             "contributors.count", "is.single.contributor",
             # editors
             "count.editors",
             # creatives
             "count.writers", "count.artists", "count.cover.artists",
             # technical
             "count.inkers", "count.letterers", "count.colorists", 
             "count.cover.inkers", "count.cover.colorists",
             # Team distribution
             "prop.editorial", "prop.creative", "prop.technical",
             # Team experience
             "debut.min", "debut.mean",
             "tenure.mean", "tenure.min")]

# Variables dropped
#             "month", "comic.end",
#             "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4",
#             "exp.gen.5", "exp.gen.6" ,"exp.gen.7", "exp.gen.8",
#             "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
#             "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16",
#             "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
#             "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24"

# Adding focal attributes
cdb$pre.issues <- NA
cdb$pre.comics <- NA
cdb$pre.publisher <- NA
cdb$pre.creator.issues <- NA
cdb$pre.issues.owned <- NA
cdb$pre.creator.comics <- NA
cdb$pre.comics.owned <- NA
cdb$pre.higher.issue <- NA
cdb$date.higher <- NA
cdb$pre.lower.issue <- NA
cdb$date.lower <- NA
cdb$genre.exp <- NA
cdb$genre.mix.exp <- NA
cdb$workload <- NA
cdb$workload.pre <- NA
cdb$pub.issues <- NA
cdb$pub.sales <- NA
cdb$pub.cont <- NA
# Using a years blocking key
years <- sort(unique(cdb$cover.year))
# testing subset

for (i in years) {
  # Subsetting information by year
  sub.par <- participations[which(participations$year <= i), ]
  sub.con <- participations[which(participations$year == i), ]
  sub.com <- comics[which(comics$sales.year == i), ]
  sub.pub <- publishers[which(publishers$year == i), ]
  
  # Adding focal information for single contributors
  index <- which(cdb$cover.year == i & cdb$is.single.contributor == TRUE)
  for (j in index) {
    k <- which(sub.par$id.contributor == cdb$id.contributor[j])
    if (length(k) > 0) {
      cdb$pre.issues[j] <- length(unique(sub.par$id.issue[k]))
      cdb$pre.comics[j] <- length(unique(sub.par$id.comic[k]))
      cdb$pre.publisher[j] <- length(unique(sub.par$id.publisher[k]))
      cdb$pre.creator.issues[j] <- sum(sub.par$creator.owned[k])
      cdb$pre.issues.owned[j] <- sum(sub.par$is.owner[k])
      cdb$pre.creator.comics[j] <- 0
      cdb$pre.comics.owned[j] <- 0
      for (l in unique(sub.par$id.comic[k])) {
        cdb$pre.creator.comics[j] <- cdb$pre.creator.comics[j] + any(sub.par$creator.owned[which(sub.par$id.comic == l & sub.par$id.contributor == cdb$id.contributor[j])])
        cdb$pre.comics.owned[j]  <- cdb$pre.comics.owned[j] + any(sub.par$creator.owned[which(sub.par$id.comic == l & sub.par$id.contributor == cdb$id.contributor[j])])
      }
      cdb$pre.higher.issue[j] <- max(sub.par$price.avg[k])
      cdb$date.higher[j] <- max(sub.par$year[k][which(sub.par$price.avg[k] == cdb$pre.higher.issue[j])])
      cdb$pre.lower.issue[j] <- min(sub.par$price.avg[k])
      cdb$date.lower[j] <- max(sub.par$year[k][which(sub.par$price.avg[k] == cdb$pre.lower.issue[j])])
      cdb$genre.exp[j] <- sum(apply(sub.par[k,c(48:71)], 2, any))
      cdb$genre.mix.exp[j] <- length(unique(sub.par$genre[k]))
      cdb$workload[j] <- length(unique(sub.par$id.issue[k][which(sub.par$year[k] == i)]))
      cdb$workload.pre[j] <- length(unique(sub.par$id.issue[k][which(sub.par$year[k] == i-1)]))
      
      cdb$pub.issues[j] <- with(sub.pub, length(id.issue[which(id.publisher == cdb$id.publisher[j])]))
      cdb$pub.sales[j] <- with(sub.com, sum(units.sold[which(id.publisher == cdb$id.publisher[j])]))
      cdb$pub.cont[j] <- with(sub.con, length(unique(id.contributor[which(id.publisher == cdb$id.publisher[j])])))
    }
  }
  # Adding focal information for teams
  index <- which(cdb$cover.year == i & cdb$is.single.contributor == FALSE)
  for (j in index) {
    k <- which(sub.par$id.team == cdb$id.team[j])
    if (length(k) > 0) {
      cdb$pre.issues[j] <- length(unique(sub.par$id.issue[k]))
      cdb$pre.comics[j] <- length(unique(sub.par$id.comic[k]))
      cdb$pre.publisher[j] <- length(unique(sub.par$id.publisher[k]))
      cdb$pre.creator.issues[j] <- 0
      cdb$pre.issues.owned[j] <- 0
      for (l in unique(sub.par$id.issue[k])) {
        cdb$pre.creator.issues[j] <- cdb$pre.creator.issues[j] + any(sub.par$creator.owned[which(sub.par$id.issue == l & sub.par$id.team == cdb$id.team[j])])
        cdb$pre.issues.owned[j] <- cdb$pre.issues.owned[j] + any(sub.par$creator.owned[which(sub.par$id.issue == l & sub.par$id.team == cdb$id.team[j])])
      }
      cdb$pre.creator.comics[j] <- 0
      cdb$pre.comics.owned[j] <- 0
      for (l in unique(sub.par$id.comic[k])) {
        cdb$pre.creator.comics[j] <- cdb$pre.creator.comics[j] + any(sub.par$creator.owned[which(sub.par$id.comic == l & sub.par$id.team == cdb$id.team[j])])
        cdb$pre.comics.owned[j]  <- cdb$pre.comics.owned[j] + any(sub.par$creator.owned[which(sub.par$id.comic == l & sub.par$id.team == cdb$id.team[j])])
      }
      cdb$pre.higher.issue[j] <- max(sub.par$price.avg[k])
      cdb$date.higher[j] <- max(sub.par$year[k][which(sub.par$price.avg[k] == cdb$pre.higher.issue[j])])
      cdb$pre.lower.issue[j] <- min(sub.par$price.avg[k])
      cdb$date.lower[j] <- max(sub.par$year[k][which(sub.par$price.avg[k] == cdb$pre.lower.issue[j])])
      cdb$genre.exp[j] <- sum(apply(sub.par[k,c(48:71)], 2, any))
      cdb$genre.mix.exp[j] <- length(unique(sub.par$genre[k]))
      cdb$workload[j] <- length(unique(sub.par$id.issue[k][which(sub.par$year[k] == i)]))
      cdb$workload.pre[j] <- length(unique(sub.par$id.issue[k][which(sub.par$year[k] == i-1)]))
      
      cdb$pub.issues[j] <- with(sub.pub, length(id.issue[which(id.publisher == cdb$id.publisher[j])]))
      cdb$pub.sales[j] <- with(sub.com, sum(units.sold[which(id.publisher == cdb$id.publisher[j])]))
      cdb$pub.cont[j] <- with(sub.con, length(unique(id.contributor[which(id.publisher == cdb$id.publisher[j])])))
    }
  }
}

# Completing missing information by asumptions
index <- which(is.na(cdb$appearances.count) == TRUE)
cdb$appearances.count[index] <- 0
index <- which(is.na(cdb$storylines.count) == TRUE)
cdb$storylines.count[index] <- 1
index <- which(is.na(cdb$genre.exp) == TRUE)
cdb$genre.exp[index] <- cdb$genre.count[index]
index <- which(cdb$workload == 0)
cdb$workload[index] <- 1
index <- which(cdb$workload.pre == 0)
cdb$workload.pre[index] <- 1
# Correcting publisher data missing
index <- which(cdb$pub.issues == 0)
cdb$pub.issues[index] <- 1
index <- which(cdb$pub.sales == 0)
cdb$pub.sales[index] <- cdb$units.sold[index]
index <- which(cdb$pub.cont == 0)
cdb$pub.cont[index] <- cdb$contributors.count[index]

# Calculating additional values
cdb$deviation <- sd(cdb$revenue)
cdb$since.higher <- cdb$cover.year - cdb$date.higher
cdb$since.lower <- cdb$cover.year - cdb$date.lower
cdb$workload.inc <- cdb$workload - cdb$workload.pre >= 5
# Adding dummy variable for 6 years span
cdb$g.years <- "97-99"
cdb$g.years[which(cdb$sales.year >= 2000 & cdb$sales.year <= 2002)] <- "00-02"
cdb$g.years[which(cdb$sales.year >= 2003 & cdb$sales.year <= 2005)] <- "03-05"
cdb$g.years[which(cdb$sales.year >= 2006 & cdb$sales.year <= 2008)] <- "06-08"
cdb$g.years[which(cdb$sales.year >= 2009 & cdb$sales.year <= 2011)] <- "09-11"
cdb$g.years[which(cdb$sales.year >= 2012 & cdb$sales.year <= 2014)] <- "12-14"
cdb$year <- as.integer(as.character(cdb$year))

# Recoding ranking value
cdb$deviation <- NA
ranking <- data.frame()
for (i in sort(unique(cdb$sales.year))) {
  sub.rank <- subset(x = cdb,
                     subset = sales.year == i,
                     select = c("id.issue", "revenue","deviation", "units.sold"))
  sub.rank <- sub.rank[with(sub.rank, order(revenue, units.sold)), ]
  sub.rank$revenue.rank <- 1:nrow(sub.rank)
  sub.rank$revenue.rank <- sub.rank$revenue.rank/nrow(sub.rank)
  sub.rank$deviation <- sub.rank$revenue - mean(sub.rank$revenue)
  ranking <- rbind(ranking, sub.rank)
}
cdb$deviation <- NULL
cdb$revenue.rank <- NULL
cdb <- merge(x = cdb, y = ranking[, c(1,3,5)], by = "id.issue")


# Remove temporary variables
rm(ranking)
rm(sub.rank)
rm(sub.year)
rm(i)
rm(index)
rm(j)

# ===== Storing dataset =====
save(list = "cdb", file = "2_output/comicsFocal.RData")
