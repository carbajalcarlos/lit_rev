# ===== Initialization =====
# Loading participations dataset
load(file = "0_input/2_creator_issue_dyads_final.RData")
load(file = "0_input/0_rawComics.RData")

# Loading the required libraries
require(reshape2)

# ===== Generating participation dataset =====
# Extracting attributes required
participations <- c_i[, # Identificators numbers 
                      c("creator_id", "title_id", "issue_id",
                        # Information
                        "creator", "title", "publisher", "year",
                        # Editorial participation
                        "editor",
                        # Creative participations
                        "writer", "artist", "cover_artist", 
                        # Technical participations
                        "inker", "letterer", "colorist",
                        "cover_inker", "cover_colorist",
                        # Ownership
                        "creator_owner", "creator_owned")]
# Remanaming the variables
names(participations) <- c("id.contributor",  "id.comic", "id.issue",
                           "contributor", "title", "publisher", "year",
                           "is.editor",
                           "is.writer", "is.artist", "is.cover.artist",
                           "is.inker", "is.letterer", "is.colorist",
                           "is.cover.inker", "is.cover.colorist",
                           "is.owner", "creator.owned")
# Tasks count
participations$task.count <- apply(X = participations[,c(8:16)],
                                   MARGIN = 1,
                                   FUN = sum)

# Role attributes
participations$role.editorial <- participations$is.editor
participations$role.creative <- apply(X = participations[, c("is.writer",
                                                             "is.artist",
                                                             "is.cover.artist")],
                                      MARGIN = 1, FUN = any)
participations$role.technical <- apply(X = participations[, c("is.inker",
                                                            "is.letterer",
                                                            "is.colorist",
                                                            "is.cover.inker",
                                                            "is.cover.colorist")],
                                     MARGIN = 1, FUN = any)
# Roles count 
participations$role.count <- apply(X = participations[,c(20:22)],
                                   MARGIN = 1,
                                   FUN = sum)
# Proportional participation
participations$prop.editorial <- with(data = participations,
                                      expr = 1/role.count*role.editorial)
participations$prop.creative <- with(data = participations,
                                      expr = 1/role.count*role.creative)
participations$prop.technical <- with(data = participations,
                                      expr = 1/role.count*role.technical)

# --- Adding publisher id ---
# Generation publisher id
publishers <- sort(unique(participations$publisher))
publishers <- data.frame(publishers, stringsAsFactors = FALSE)
names(publishers) <- "publisher"
publishers$id.publisher <- 1:nrow(publishers)
# Merging publisher id
participations <- merge(x = participations, y = publishers, by = "publisher")

# Removing temporary vatiables
rm(c_i)

# ===== Extracting additional comics data ====
# --- Extracting additional comics data ---
c.additional <- rawComics[, c("ID", "Price.Year.1", "Price.Year.2",
                              "Price.Year.3", "Price.Year.4", "Genres")]
names(c.additional) <- c("id.issue", "price.2011", "price.2012",
                         "price.2013", "price.2014", "genre")

# --- Processing genres ---
# Obtaining the average price on the last 4 years
c.additional$price.avg <- apply(X = c.additional[2:5], MARGIN = 1,
                                FUN = mean, rm.na = TRUE)
c.additional$price.avg <- round(x = c.additional$price.avg, digits = 2)
# Removing unneeded attributes 
c.additional <- c.additional[, c("id.issue", "price.avg", "genre")]

# --- Processing genres ---
genres <- levels(c.additional$genre)
genres <- data.frame(genres, stringsAsFactors = FALSE)
genres$size <- nchar(genres$genres)
# Searching for entries without the proper size
index <- grep(pattern = "28", x = genres$size, invert = TRUE)
# Filling the missing characteres with "0"
for (i in 1:length(index)) {
  size <- nchar(genres$genres[index[i]])
  for (j in (size+1):28) {
    genres$genres[index[i]] <- paste(c(genres$genres[index[i]],"0"),
                                     collapse =  "")
  }
}
# Chnaging [space] charactares with 0
genres$genres <- gsub(pattern = "[^1]", replacement = "0", x = genres$genres)
# Restoring the new level names
levels(c.additional$genre) <- genres$genres
# Trimming to 24 characters
c.additional$genre <- as.character(c.additional$genre)
c.additional$genre <- gsub(pattern = "(^.{24})(.{4}$)",
                           replacement = "\\1",
                           x = c.additional$genre)
# Erasing entries with no genre defined
index <- grep(pattern = "1", c.additional$genre, invert = TRUE)
c.additional$genre[index]<- NA
# Generating a genre dummy array
index <- which(is.na(c.additional$genre) == FALSE)
genres <- unlist(strsplit(c.additional$genre[index], ""))
genres <- as.logical(as.integer(genres))
genres <- data.frame(matrix(genres, ncol = 24, byrow = TRUE))
genres <- cbind(c.additional$id.issue[index], genres)
names(genres) <- c("id.issue",paste("gen",1:24, sep = "."))
# Including dummy genre array into the information available
c.additional.y <- merge(x = c.additional, y = genres, by = "id.issue")
index <- which(!is.element(el = c.additional$id.issue,
                           set = c.additional.y$id.issue))
c.additional.n <- c.additional[index, ] 
genres <- data.frame(matrix(nrow = length(index), ncol = 24))
names(genres) <- paste("gen",1:24, sep = ".")
c.additional.n <- cbind(c.additional.n, genres)
c.additional <- rbind(c.additional.y, c.additional.n)

# --- Removing temporary dataset ---
rm(c.additional.n)
rm(c.additional.y)
rm(genres)
rm(rawComics)

# ===== Aggregation by issue =====
index <- which(duplicated(participations$id.issue) == FALSE)
issues <- participations[index ,c("id.issue", "id.comic",
                                  "year", "publisher")]
names(issues) <- c("id.issue", "id.comic", "year")
issues <- issues[with(issues, order(id.issue)), ]

# Number of contributors
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "id.contributor", length)
names(issue.tasks)[2] <- "count.contributors"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")

# Ownership
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.owner", any, na.rm = TRUE)
names(issue.tasks)[2] <- "owner.in"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "creator.owned", any, na.rm = TRUE)
names(issue.tasks)[2] <- "creator.owned"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")

# Proportional tasks
temporal <- participations[,c(3,8:16,19)]
for (i in 2:10) {
  temporal[,i] = temporal[,i]*(1/temporal[,11])
}

# Sum of tasks
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.editor", sum)
names(issue.tasks)[2] <- "count.editors"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.writer", sum)
names(issue.tasks)[2] <- "count.writers"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.artist", sum)
names(issue.tasks)[2] <- "count.artists"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.inker", sum)
names(issue.tasks)[2] <- "count.inkers"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.letterer", sum)
names(issue.tasks)[2] <- "count.letterers"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.colorist", sum)
names(issue.tasks)[2] <- "count.colorists"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.cover.artist", sum)
names(issue.tasks)[2] <- "count.cover.artists"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.cover.inker", sum)
names(issue.tasks)[2] <- "count.cover.inkers"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = temporal,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "is.cover.colorist", sum)
names(issue.tasks)[2] <- "count.cover.colorists"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")


# Sum of roles
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "prop.editorial", sum)
names(issue.tasks)[2] <- "count.editorials"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "prop.creative", sum)
names(issue.tasks)[2] <- "count.creatives"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "prop.technical", sum)
names(issue.tasks)[2] <- "count.technicals"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
# Ownership
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")
issue.tasks <- recast(data = participations,
                      formula =  id.issue ~ .,
                      id.var = c("id.issue"),
                      measure.var = "prop.technical", sum)
names(issue.tasks)[2] <- "count.technicals"
issues <- merge(x = issues, y = issue.tasks, by = "id.issue")



# Proportional values for the counters
for (i in 3:14) {
  issues[,i] = issues[,i]/issues[,2]
}

# --- Special cases ---
# Single contributor
issues$single.contributor <- FALSE
index <- with(issues, which(count.contributors == 1))
issues$single.contributor[index] <- TRUE
# Cover work
issues$cover.work <- FALSE
index <- with(issues, which((count.cover.inkers + count.cover.artists + count.cover.colorists) > 0))
issues$cover.work[index] <- TRUE
# Only editorial work
issues$only.editorial <- FALSE
index <- with(issues, which(count.creatives == 0 & count.technicals == 0))
issues$only.editorial[index] <- TRUE
# Only technical work
issues$only.technical <- FALSE
index <- with(issues, which(count.editorials == 0 & count.creatives == 0))
issues$only.technical[index] <- TRUE

# --- Core teams ---
issues$members <- NA
# Forming core teams
index <- with(data = issues,
              expr = which(!only.editorial & !only.technical & !single.contributor)) 
for (i in index) {
  members <- which(participations$id.issue == issues$id.issue[i])
  members <- participations[members,]
  members <- subset(x = members, subset = role.editorial | role.creative)
  issues$members[i] <- paste(members$id.contributor, collapse = "+")
}
# Forming only editorial teams 
index <- with(data = issues, expr = which(only.editorial & !single.contributor))
for (i in index) {
  members <- which(participations$id.issue == issues$id.issue[i])
  members <- participations[members,]
  if (nrow(members) <= 1) {
    next
  }
  issues$members[i] <- paste(members$id.contributor, collapse = "+")
}
# Forming only technical teams 
index <- with(data = issues, expr = which(only.technical & !single.contributor))
for (i in index) {
  members <- which(participations$id.issue == issues$id.issue[i])
  members <- participations[members,]
  if (nrow(members) <= 1) {
    next
  }
  issues$members[i] <- paste(members$id.contributor, collapse = "+")
}

# Creating team id
teams <- levels(as.factor(issues$members))
teams <- data.frame(teams, stringsAsFactors = FALSE)
names(teams) <- "members"
teams$char <- nchar(teams$members)
teams <- teams[with(teams, order(char, members)), ]
teams$id.team <- 1:nrow(teams)
teams <- teams[, c(1,3)]
# Merging  core team id
issues.y <- merge(x = issues, y = teams, by = "members")
issues.y <- issues.y[, c(2:19,1,20)]
index <- which(!is.element(el = issues$id.issue, set = issues.y$id.issue))
issues.n <- issues[index,]        
issues.n$id.team <- NA 
issues <- rbind(issues.y, issues.n)

# --- Forming creative teams --- 
issues$members.creatives <- NA
# Forming core teams
index <- with(data = issues,
              expr = which(count.creatives > 0 & !single.contributor)) 
for (i in index) {
  members <- which(participations$id.issue == issues$id.issue[i])
  members <- participations[members,]
  members <- subset(x = members, subset = role.creative)
  if (nrow(members) <= 1) {
    next
  }
  issues$members.creatives[i] <- paste(members$id.contributor, collapse = "+")
}

# Creating team id
creatives <- levels(as.factor(issues$members.creatives))
creatives <- data.frame(creatives, stringsAsFactors = FALSE)
names(creatives) <- "members.creatives"
creatives$char <- nchar(creatives$members.creatives)
creatives <- creatives[with(creatives, order(char, members.creatives)), ]
creatives$id.team.creative <- 1:nrow(creatives)
creatives <- creatives[, c(1,3)]
# Merging creative team id
issues.y <- merge(x = issues, y = creatives, by = "members.creatives")
issues.y <- issues.y[, c(2:21,1,22)]
index <- which(!is.element(el = issues$id.issue, set = issues.y$id.issue))
issues.n <- issues[index,]        
issues.n$id.team.creative <- NA 
issues <- rbind(issues.y, issues.n)

# --- Merge with additional comics data ---
issues <- merge(x = issues, y = c.additional, by = "id.issue")

# Removing temporary variables
rm(c.additional)
rm(issue.tasks)
rm(issues.n)
rm(issues.y)
rm(members)
rm(temporal)

# ===== Aggregation by contributors =====
# Merge of participation and issue datasets
# --- Merge with participations ---
participations <- merge(x = participations, y = issues, by = "id.issue")

# Generation of contributor dataset
index <- which(duplicated(participations$id.contributor) == FALSE)
contributors <- participations[index, c("id.contributor", "contributor")]
contributors <- contributors[with(contributors, order(id.contributor)), ]

# Number of issues
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "id.issue", length)
names(cont.temp)[2] <- "count.issues"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
# Start of activity
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "year", min, na.rm = TRUE)
names(cont.temp)[2] <- "start"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
# Last participations activity
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "year", max, na.rm = TRUE)
names(cont.temp)[2] <- "last"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
# Number of roles in the same issue
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ role.count,
                    id.var = c("id.contributor", "role.count"),
                    measure.var = "id.issue", length)
names(cont.temp) <- c("id.contributor", "1.role", "2.rol", "3.rol")
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
# Type of projects worked on
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ creator.owned,
                    id.var = c("id.contributor", "creator.owned"),
                    measure.var = "id.issue", length)
names(cont.temp) <- c("id.contributor", "proj.corporate", "proj.creator")
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
# ownership rights
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ is.owner,
                    id.var = c("id.contributor", "is.owner"),
                    measure.var = "id.issue", length)
names(cont.temp) <- c("id.contributor", "issue.no.owned", "issues.owned")
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")

# Defining profile
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "prop.editorial", mean)
names(cont.temp)[2] <- "prop.editorial"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "prop.creative", mean)
names(cont.temp)[2] <- "prop.creative"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")
cont.temp <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "prop.technical", mean)
names(cont.temp)[2] <- "prop.technical"
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")

# Participation count attributes
cont.temp <- data.frame(sort(unique(participations$id.contributor)))
names(cont.temp) <- "id.contributor"
# Experience
cont.temp$count.comic <- NA
cont.temp$count.teams <- NA
cont.temp$count.teams.creative <- NA
cont.temp$count.publi <- NA
cont.temp$count.colaborations <- NA
cont.temp$count.colaborations.creative <- NA
cont.temp$genre.mix <- NA
# Previous performance
cont.temp$higher.issue <- NA
cont.temp$lower.issue <- NA
# Collaborations
cont.temp$fav.team.1 <- NA
cont.temp$part.team.1 <- NA
cont.temp$fav.team.2 <- NA
cont.temp$part.team.2 <- NA
cont.temp$fav.team.3 <- NA
cont.temp$part.team.3 <- NA
# Collaborations
cont.temp$fav.team.crea.1 <- NA
cont.temp$part.team.crea.1 <- NA
cont.temp$fav.team.crea.2 <- NA
cont.temp$part.team.crea.2 <- NA
cont.temp$fav.team.crea.3 <- NA
cont.temp$part.team.crea.3 <- NA
# Publishers
cont.temp$fav.pub.1 <- NA
cont.temp$part.pub.1 <- NA
cont.temp$fav.pub.2 <- NA
cont.temp$part.pub.2 <- NA
cont.temp$fav.pub.3 <- NA
cont.temp$part.pub.3 <- NA
# Diversity
cont.temp$gen.1 <- NA
cont.temp$gen.2 <- NA
cont.temp$gen.3 <- NA
cont.temp$gen.4 <- NA
cont.temp$gen.5 <- NA
cont.temp$gen.6 <- NA
cont.temp$gen.7 <- NA
cont.temp$gen.8 <- NA
cont.temp$gen.9 <- NA
cont.temp$gen.10 <- NA
cont.temp$gen.11 <- NA
cont.temp$gen.12 <- NA
cont.temp$gen.13 <- NA
cont.temp$gen.14 <- NA
cont.temp$gen.15 <- NA
cont.temp$gen.16 <- NA
cont.temp$gen.17 <- NA
cont.temp$gen.18 <- NA
cont.temp$gen.19 <- NA
cont.temp$gen.20 <- NA
cont.temp$gen.21 <- NA
cont.temp$gen.22 <- NA
cont.temp$gen.23 <- NA
cont.temp$gen.24 <- NA

# Counting cicle
for (i in 1:nrow(cont.temp)) {
  index <- which(participations$id.contributor == cont.temp$id.contributor[i])
  sample <- participations[index, ]
  # Experience
  cont.temp$count.comic[i] <- length(unique(na.omit(sample$id.comic)))
  cont.temp$count.publi[i] <- length(unique(na.omit(sample$id.publisher)))
  cont.temp$count.teams[i] <- length(unique(na.omit(sample$id.team)))
  cont.temp$count.colaborations[i] <- length(na.omit(sample$id.team))
  cont.temp$count.teams.creative[i] <- length(unique(na.omit(sample$id.team.creative)))
  cont.temp$count.colaborations.creative[i] <- length(na.omit(sample$id.team.creative))
  cont.temp$genre.mix[i] <- length(unique(na.omit(sample$genre)))
  # Collaborations
  temp <- sort(table(sample$id.team), decreasing = TRUE)
  if (length(temp) >= 1) {
    cont.temp$part.team.1[i] <- temp[1]
    cont.temp$part.team.2[i] <- temp[2]
    cont.temp$part.team.3[i] <- temp[3]
    temp <- names(temp)
    cont.temp$fav.team.1[i] <- temp[1]
    cont.temp$fav.team.2[i] <- temp[2]
    cont.temp$fav.team.3[i] <- temp[3]
  }
  temp <- sort(table(sample$id.team.creative), decreasing = TRUE)
  if (length(temp) >= 1) {
    cont.temp$part.team.crea.1[i] <- temp[1]
    cont.temp$part.team.crea.2[i] <- temp[2]
    cont.temp$part.team.crea.3[i] <- temp[3]
    temp <- names(temp)
    cont.temp$fav.team.crea.1[i] <- temp[1]
    cont.temp$fav.team.crea.2[i] <- temp[2]
    cont.temp$fav.team.crea.3[i] <- temp[3]
  }
  temp <- sort(table(sample$publisher), decreasing = TRUE)
  if (length(temp) >= 1) {
    cont.temp$part.pub.1[i] <- temp[1]
    cont.temp$part.pub.2[i] <- temp[2]
    cont.temp$part.pub.3[i] <- temp[3]
    temp <- names(temp)
    cont.temp$fav.pub.1[i] <- temp[1]
    cont.temp$fav.pub.2[i] <- temp[2]
    cont.temp$fav.pub.3[i] <- temp[3]
  }
  # Diversity                                             
  cont.temp$gen.1[i] <- sum(sample$gen.1, na.rm = TRUE)
  cont.temp$gen.2[i] <- sum(sample$gen.2, na.rm = TRUE)
  cont.temp$gen.3[i] <- sum(sample$gen.3, na.rm = TRUE)
  cont.temp$gen.4[i] <- sum(sample$gen.4, na.rm = TRUE)
  cont.temp$gen.5[i] <- sum(sample$gen.5, na.rm = TRUE)
  cont.temp$gen.6[i] <- sum(sample$gen.6, na.rm = TRUE)
  cont.temp$gen.7[i] <- sum(sample$gen.7, na.rm = TRUE)
  cont.temp$gen.8[i] <- sum(sample$gen.8, na.rm = TRUE)
  cont.temp$gen.9[i] <- sum(sample$gen.9, na.rm = TRUE)
  cont.temp$gen.10[i] <- sum(sample$gen.10, na.rm = TRUE)
  cont.temp$gen.11[i] <- sum(sample$gen.11, na.rm = TRUE)
  cont.temp$gen.12[i] <- sum(sample$gen.12, na.rm = TRUE)
  cont.temp$gen.13[i] <- sum(sample$gen.13, na.rm = TRUE)
  cont.temp$gen.14[i] <- sum(sample$gen.14, na.rm = TRUE)
  cont.temp$gen.15[i] <- sum(sample$gen.15, na.rm = TRUE)
  cont.temp$gen.16[i] <- sum(sample$gen.16, na.rm = TRUE)
  cont.temp$gen.17[i] <- sum(sample$gen.17, na.rm = TRUE)
  cont.temp$gen.18[i] <- sum(sample$gen.18, na.rm = TRUE)
  cont.temp$gen.19[i] <- sum(sample$gen.19, na.rm = TRUE)
  cont.temp$gen.20[i] <- sum(sample$gen.20, na.rm = TRUE)
  cont.temp$gen.21[i] <- sum(sample$gen.21, na.rm = TRUE)
  cont.temp$gen.22[i] <- sum(sample$gen.22, na.rm = TRUE)
  cont.temp$gen.23[i] <- sum(sample$gen.23, na.rm = TRUE)
  cont.temp$gen.24[i] <- sum(sample$gen.24, na.rm = TRUE)
  # Previous performance
  cont.temp$higher.issue[i] <- max(sample$price.avg, na.rm = TRUE)
  cont.temp$lower.issue[i] <- min(sample$price.avg, na.rm = TRUE)
}
contributors <- merge(x = contributors, y = cont.temp, by = "id.contributor")

# Removing temporary variables
rm(cont.temp)
rm(sample)

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

# ===== Cleaning datasets =====
# Participations dataset
participations <- participations[, # Identificators
                                 c("id.publisher", "id.comic",
                                   "id.team", "id.team.creative",
                                   "id.issue", "id.contributor",
                                   # Names
                                   "publisher", "title", "contributor", "year",
                                   # === Data per participation ===
                                   # Dummy tasks identificators
                                   "is.editor", 
                                   "is.writer", "is.artist", "is.cover.artist",
                                   "is.inker", "is.letterer", "is.colorist",
                                   "is.cover.inker", "is.cover.colorist",
                                   # Count of tasks per contributor per issue
                                   "task.count",
                                   # Ownership indicators
                                   "is.owner", "creator.owned",
                                   # Special cover works
                                   "cover.work",
                                   # Dummy roles identificatiors 
                                   "role.editorial", "role.creative", "role.technical",
                                   # Count of roles per contributor per issue
                                   "role.count",
                                   # Proportional contribution per role
                                   "prop.editorial", "prop.creative", "prop.technical",
                                   # === Data per issue ===
                                   # Contributors per issue, single identificator
                                   "count.contributors", "single.contributor",
                                   # Count of contributors per issue by tasks
                                   "count.editors", 
                                   "count.writers", "count.artists",
                                   "count.cover.artists",
                                   "count.inkers", "count.letterers", 
                                   "count.colorists","count.cover.inkers",
                                   "count.cover.colorists",
                                   # Count of contributors per issue by roles
                                   "count.editorials", "count.creatives",
                                   "count.technicals",
                                   # Special team cases
                                   "only.editorial", "only.technical",
                                   # Concatenation of team members
                                   "members", "members.creatives",
                                   # Price and unique genre combination per issue
                                   "price.avg", "genre",
                                   # Dummy genre identificators
                                   "gen.1", "gen.2", "gen.3", "gen.4", "gen.5",
                                   "gen.6", "gen.7", "gen.8", "gen.9", "gen.10", 
                                   "gen.11", "gen.12", "gen.13", "gen.14",
                                   "gen.15", "gen.16", "gen.17", "gen.18",
                                   "gen.19", "gen.20", "gen.21", "gen.22",
                                   "gen.23", "gen.24")]

# Issues dataset
# Adding publisher id
issues <- merge(x = issues, y = publishers, by = "publisher")
# Restructuring variables
issues <- issues[, # Identificators
                 c("id.issue", "id.comic", "id.publisher",
                   "id.team", "id.team.creative",
                   # issue information
                   "publisher", "year",
                   # Ownership
                   "owner.in", "creator.owned",
                   # Special cover work
                   "cover.work",
                   # Contributors per issue, single identificator
                   "count.contributors", "single.contributor",
                   # Count of contributors per issue by tasks
                   "count.editors", 
                   "count.writers", "count.artists", "count.cover.artists",
                   "count.inkers", "count.letterers",  "count.colorists",
                   "count.cover.inkers", "count.cover.colorists",
                   # Count of contributors per issue by roles
                   "count.editorials", "count.creatives", "count.technicals",
                   # Special team cases
                   "only.editorial", "only.technical",
                   # Concatenation of team members
                   "members", "members.creatives",
                   # Price and unique genre combination per issue
                   "price.avg", "genre",
                   # Dummy genre identificators
                   "gen.1", "gen.2", "gen.3", "gen.4", "gen.5", "gen.6",
                   "gen.7", "gen.8", "gen.9", "gen.10", "gen.11", "gen.12",
                   "gen.13", "gen.14", "gen.15", "gen.16", "gen.17", "gen.18",
                   "gen.19", "gen.20", "gen.21", "gen.22", "gen.23", "gen.24")]

# Contributors
contributors <- contributors[, # Identificators
                             c("id.contributor", "contributor",
                               # Activity period
                               "start", "last",
                               # Participations counts
                               "count.issues", "count.comic", "count.publi",
                               "count.teams", "count.colaborations",
                               "count.teams.creative", "count.colaborations.creative",
                               # Unique genre combinations
                               "genre.mix",
                               # Ownership
                               "proj.creator", "issues.owned", # NOTE: Count comics owned
                               # Counter of how many roles have performed
                               "1.role", "2.rol", "3.rol",
                               # Profile
                               "prop.editorial", "prop.creative", "prop.technical",
                               # Previous performance
                               "higher.issue", "lower.issue", # NOTE: Include the year of each occurence
                               # Publishers collaborations
                               "fav.pub.1", "part.pub.1", # NOTE: Change to publisher id
                               "fav.pub.2", "part.pub.2",
                               "fav.pub.3", "part.pub.3",
                               # Teams collaborations
                               "fav.team.1", "part.team.1",
                               "fav.team.2", "part.team.2", 
                               "fav.team.3", "part.team.3", 
                               "fav.team.crea.1", "part.team.crea.1", 
                               "fav.team.crea.2", "part.team.crea.2",
                               "fav.team.crea.3", "part.team.crea.3",
                               # Genre identificators counter
                               "gen.1", "gen.2", "gen.3", "gen.4", "gen.5",
                               "gen.6", "gen.7", "gen.8", "gen.9", "gen.10", 
                               "gen.11", "gen.12", "gen.13", "gen.14",
                               "gen.15", "gen.16", "gen.17", "gen.18",
                               "gen.19", "gen.20", "gen.21", "gen.22",
                               "gen.23", "gen.24")]
# Teams
teams <- teams[, # Identificators
               c("id.team", "members.x",
                 # Members count
                 "members.y",
                 # Activity period
                 "start", "last",
                 # Participations counts
                 "count.issues", "count.comic", "count.publi",
                 # Unique genre combination
                 "genre.mix",
                 # Ownership
                 "creator.owned", "owner.in",
                 # Profile
                 "prop.editorial", "prop.creatives", "prop.technicals",
                 # Previous performance
                 "higher.issue", "lower.issue", # NOTE: Include the year of each occurence
                 # Publishers collaborations
                 "fav.pub.1", "part.pub.1", # NOTE: Change to publisher id
                 "fav.pub.2", "part.pub.2",
                 "fav.pub.3", "part.pub.3",
                 # Genre identificators counter
                 "gen.1", "gen.2", "gen.3", "gen.4", "gen.5",
                 "gen.6", "gen.7", "gen.8", "gen.9", "gen.10", 
                 "gen.11", "gen.12", "gen.13", "gen.14",
                 "gen.15", "gen.16", "gen.17", "gen.18",
                 "gen.19", "gen.20", "gen.21", "gen.22",
                 "gen.23", "gen.24")]

# ====== Storing participation data =====
# Removing temporary variables
rm(index)
rm(i)
rm(j)
rm(size)
rm(temp)

save(list = "participations", file = "1_blackbox/10_participations.RData")
save(list = "issues", file = "1_blackbox/10_issues.RData")
save(list = "teams", file = "1_blackbox/10_teams.RData")
save(list = "contributors", file = "1_blackbox/10_contributors.RData")
save(list = "publishers", file = "1_blackbox/10_publishers.RData")
save(list = "creatives", file = "1_blackbox/10_creatives.RData")
