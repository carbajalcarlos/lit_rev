# ====== Initialization ======
# Loading participations dataset
load(file = "1_blackbox/10_participations.RData")
load(file = "1_blackbox/10_issues.RData")
load(file = "1_blackbox/10_teams.RData")

# Loading the required libraries
require(reshape2)

# ===== Aggregation by teams =====
# Number of issues
teams.temp <- recast(data = issues,
                     formula =  id.team ~ .,
                     id.var = c("id.team"),
                     measure.var = "id.issue", length)
names(teams.temp)[2] <- "count.issues"
teams <- merge(x = teams, y = teams.temp, by = "id.team")
# Start of activity
teams.temp <- recast(data = issues,
                     formula =  id.team ~ .,
                     id.var = c("id.team"),
                     measure.var = "year", min, na.rm = TRUE)
names(teams.temp)[2] <- "start"
teams <- merge(x = teams, y = teams.temp, by = "id.team")
# Last participations activity
teams.temp <- recast(data = issues,
                     formula =  id.team ~ .,
                     id.var = c("id.team"),
                     measure.var = "year", max, na.rm = TRUE)
names(teams.temp)[2] <- "last"
teams <- merge(x = teams, y = teams.temp, by = "id.team")

# Type of projects worked on
teams.temp <- recast(data = issues,
                     formula =  id.team ~ creator.owned,
                     id.var = c("id.team", "creator.owned"),
                     measure.var = "id.issue", length)
names(teams.temp) <- c("id.team", "corporate.owned", "creator.owned")
teams <- merge(x = teams, y = teams.temp, by = "id.team")
# ownership rights
teams.temp <- recast(data = issues,
                     formula =  id.team ~ owner.in,
                     id.var = c("id.team", "owner.in"),
                     measure.var = "id.issue", length)
names(teams.temp) <- c("id.team", "owner.out", "owner.in")
teams <- merge(x = teams, y = teams.temp, by = "id.team")

# Defining profile
teams.temp <- recast(data = issues,
                    formula =  id.team ~ .,
                    id.var = c("id.team"),
                    measure.var = "count.editorials", mean)
names(teams.temp)[2] <- "prop.editorial"
teams <- merge(x = teams, y = teams.temp, by = "id.team")
teams.temp <- recast(data = issues,
                     formula =  id.team ~ .,
                     id.var = c("id.team"),
                     measure.var = "count.creatives", mean)
names(teams.temp)[2] <- "prop.creatives"
teams <- merge(x = teams, y = teams.temp, by = "id.team")
teams.temp <- recast(data = issues,
                     formula =  id.team ~ .,
                     id.var = c("id.team"),
                     measure.var = "count.technicals", mean)
names(teams.temp)[2] <- "prop.technicals"
teams <- merge(x = teams, y = teams.temp, by = "id.team")

# Participation count attributes
teams.temp <- data.frame(teams[,1])
names(teams.temp) <- "id.team"
# Members
teams.temp$members <- NA
teams.temp$members.edit <- NA
teams.temp$members.crea <- NA
teams.temp$members.tech <- NA
# Experience
teams.temp$count.comic <- NA
teams.temp$count.publi <- NA
teams.temp$genre.mix <- NA
# Previous performance
teams.temp$higher.issue <- NA
teams.temp$lower.issue <- NA
# Publishers
teams.temp$fav.pub.1 <- NA
teams.temp$part.pub.1 <- NA
teams.temp$fav.pub.2 <- NA
teams.temp$part.pub.2 <- NA
teams.temp$fav.pub.3 <- NA
teams.temp$part.pub.3 <- NA
# Diversity
teams.temp$gen.1 <- NA
teams.temp$gen.2 <- NA
teams.temp$gen.3 <- NA
teams.temp$gen.4 <- NA
teams.temp$gen.5 <- NA
teams.temp$gen.6 <- NA
teams.temp$gen.7 <- NA
teams.temp$gen.8 <- NA
teams.temp$gen.9 <- NA
teams.temp$gen.10 <- NA
teams.temp$gen.11 <- NA
teams.temp$gen.12 <- NA
teams.temp$gen.13 <- NA
teams.temp$gen.14 <- NA
teams.temp$gen.15 <- NA
teams.temp$gen.16 <- NA
teams.temp$gen.17 <- NA
teams.temp$gen.18 <- NA
teams.temp$gen.19 <- NA
teams.temp$gen.20 <- NA
teams.temp$gen.21 <- NA
teams.temp$gen.22 <- NA
teams.temp$gen.23 <- NA
teams.temp$gen.24 <- NA

# Counting cicle
for (i in 1:nrow(teams.temp)) {
  index <- which(issues$id.team == teams.temp$id.team[i])
  sample <- issues[index, ]
  # Members
  teams.temp$members[i] <- mean(sample$count.contributors, na.rm = TRUE)
  teams.temp$members.edit[i] <- mean(sample$count.editorials, na.rm = TRUE)
  teams.temp$members.crea[i] <- mean(sample$count.creatives, na.rm = TRUE)
  teams.temp$members.tech[i] <- mean(sample$count.technicals, na.rm = TRUE)
  # Experience
  teams.temp$count.comic[i] <- length(unique(na.omit(sample$id.comic)))
  teams.temp$count.publi[i] <- length(unique(na.omit(sample$publisher)))
  teams.temp$genre.mix[i] <- length(unique(na.omit(sample$genre)))
  # Collaborations
  temp <- sort(table(as.character(sample$publisher)), decreasing = TRUE)
  if (length(temp) >= 1) {
    teams.temp$part.pub.1[i] <- temp[1]
    teams.temp$part.pub.2[i] <- temp[2]
    teams.temp$part.pub.3[i] <- temp[3]
    temp <- names(temp)
    teams.temp$fav.pub.1[i] <- temp[1]
    teams.temp$fav.pub.2[i] <- temp[2]
    teams.temp$fav.pub.3[i] <- temp[3]
  }
  # Previous performance
  teams.temp$higher.issue[i] <- max(sample$price.avg, na.rm = TRUE)
  teams.temp$lower.issue[i] <- min(sample$price.avg, na.rm = TRUE)
  
  # Diversity                                             
  teams.temp$gen.1[i] <- sum(sample$gen.1, na.rm = TRUE)
  teams.temp$gen.2[i] <- sum(sample$gen.2, na.rm = TRUE)
  teams.temp$gen.3[i] <- sum(sample$gen.3, na.rm = TRUE)
  teams.temp$gen.4[i] <- sum(sample$gen.4, na.rm = TRUE)
  teams.temp$gen.5[i] <- sum(sample$gen.5, na.rm = TRUE)
  teams.temp$gen.6[i] <- sum(sample$gen.6, na.rm = TRUE)
  teams.temp$gen.7[i] <- sum(sample$gen.7, na.rm = TRUE)
  teams.temp$gen.8[i] <- sum(sample$gen.8, na.rm = TRUE)
  teams.temp$gen.9[i] <- sum(sample$gen.9, na.rm = TRUE)
  teams.temp$gen.10[i] <- sum(sample$gen.10, na.rm = TRUE)
  teams.temp$gen.11[i] <- sum(sample$gen.11, na.rm = TRUE)
  teams.temp$gen.12[i] <- sum(sample$gen.12, na.rm = TRUE)
  teams.temp$gen.13[i] <- sum(sample$gen.13, na.rm = TRUE)
  teams.temp$gen.14[i] <- sum(sample$gen.14, na.rm = TRUE)
  teams.temp$gen.15[i] <- sum(sample$gen.15, na.rm = TRUE)
  teams.temp$gen.16[i] <- sum(sample$gen.16, na.rm = TRUE)
  teams.temp$gen.17[i] <- sum(sample$gen.17, na.rm = TRUE)
  teams.temp$gen.18[i] <- sum(sample$gen.18, na.rm = TRUE)
  teams.temp$gen.19[i] <- sum(sample$gen.19, na.rm = TRUE)
  teams.temp$gen.20[i] <- sum(sample$gen.20, na.rm = TRUE)
  teams.temp$gen.21[i] <- sum(sample$gen.21, na.rm = TRUE)
  teams.temp$gen.22[i] <- sum(sample$gen.22, na.rm = TRUE)
  teams.temp$gen.23[i] <- sum(sample$gen.23, na.rm = TRUE)
  teams.temp$gen.24[i] <- sum(sample$gen.24, na.rm = TRUE)
}
teams <- merge(x = teams, y = teams.temp, by = "id.team")

# Removing temporary variables
rm(teams.temp)
rm(sample)
