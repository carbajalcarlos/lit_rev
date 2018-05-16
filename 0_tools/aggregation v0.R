# ===== Initialization =====
# Loading participations information
load(file = "0_input/2_creator_issue_dyads_final.RData")
load(file = "0_input/0_rawComics.RData")
# Loading the required libraries
require(reshape2)

# ====== Extracting information from seccundary sources ====
sec.comics <- rawComics[, c("ID", "Price.Year.4", "Genres")]
names(sec.comics) <- c("id.issue", "current.price", "genre")
# Preprocesing attributes
genres <- levels(sec.comics$genre)
genres <- data.frame(genres, stringsAsFactors = FALSE)
genres$size <- nchar(genres$genres)
# Searching for entries without the proper size
index <- grep(pattern = "28", x = genres$size, invert = TRUE)
# Filling the missing characteres with "0"
for (i in 1:length(index)) {
  size <- nchar(genres$genres[index[i]])
  for (j in (size+1):28) {
    genres$genres[index[i]] <- paste(c(genres$genres[index[i]],"0"), collapse =  "")
  }
}

# Chnaging [space] charactares with 0
genres$genres <- gsub(pattern = " |[[:alpha:]]", replacement = "0", x = genres$genres)
# Restoring the new level names
levels(sec.comics$genre) <- genres$genres
# Cutting to 24 characters
sec.comics$genre <- as.character(sec.comics$genre)
sec.comics$genre <- gsub(pattern = "(^.{24})(.{4}$)", replacement = "\\1", x = sec.comics$genre)
# Erasing entries with no genre defined
index <- grep(pattern = "^0{24}$", sec.comics$genre)
sec.comics$genre[index]<- NA
# Generating a genre dummy array
index <- which(is.na(sec.comics$genre) == FALSE)
data <- as.logical(as.integer(unlist(strsplit(sec.comics$genre[index], ""))))
issue.genres <- as.data.frame(matrix(data, ncol = 24, byrow = TRUE))
issue.genres <- cbind(sec.comics[index,1], issue.genres)
names(issue.genres) <- c("id.issue",paste("g",1:24, sep = "."))

# Removing genres dataset
rm(data)
rm(rawComics)
rm(genres)
rm(j)

# ====== Extracting attributes of interest ======
# Identificators
participations <- c_i [, c("creator_id", "creator",
                         # Issue info
                         "title_id", "issue_id", "title", "year", 
                         # Corporation info
                         "publisher", "copyright",
                         # Main participations
                         "writer", "artist",
                         # Secundaty participations
                         "inker", "letterer", "colorist",
                         # Special participations
                         "cover_artist", "cover_inker", "cover_colorist",
                         # Roles
                         "editor", "supporter",
                         # Ownership
                         "creator_owner", "creator_owned")]
# Remanaming the variables
names(participations) <- c("id.contributor", "name",
                         "id.comic", "id.issue", "title", "year",
                         "publisher.con", "copyright.con",
                         "is.writer", "is.artist",
                         "is.inker", "is.letterer", "is.colorist",
                         "is.cover.artist", "is.cover.inker", "is.cover.colorist",
                         "role.editor", "role.support",
                         "owner", "own.by.creator")

names(participations)[13] <- "creator.project" 

# Completing roles
participations$role.creative <- with(data = participations, 
                                   expr = is.writer | is.artist)
participations$role.support <- with(data = participations, 
                                  expr = is.inker | is.letterer | is.colorist)
participations$role.special <- with(data = participations, 
                                  expr = is.cover.artist | is.cover.inker | is.cover.colorist)
# Finding multiple roles 
names <- names(participations)
index <- c(which(names == "role.creative"),
           which(names == "role.editor"),
           which(names == "role.support"),
           which(names == "role.special"))
participations$multiple <- apply(participations[,index], 1, sum, na.rm = TRUE)
# Reordering the dataset by contributor id
participations <- participations[with(participations, order(id.contributor,
                                                      year,
                                                      id.comic,
                                                      id.issue)), ]

# Substracting attributes of interest
participations <- participations[, c("id.contributor", "name",
                                 "id.comic", "id.issue", "year",
                                 "role.creative", "role.editor",
                                 "role.support", "role.special", "multiple",
                                 "owner.creator", "owner.project",
                                 "is.writer", "is.artist",
                                 "is.inker", "is.letterer", "is.colorist",
                                 "is.cover.artist", "is.cover.inker",
                                 "is.cover.colorist",
                                 "publisher.con", "copyright.con")]

# Removing original dataset
rm(c_i)
rm(names)

# ===== Team composition data =====
# Creation of the team dataset by issue
issue.teams <- data.frame(sort(unique(participations$id.issue)))
names(issue.teams) <- "id.issue"
# Adding necessary attributes
issue.teams$size <- NA
issue.teams$members <- NA
# Filling the attributes for every issue
for (i in 1:nrow(issue.teams)) {
  index <- which(participations$id.issue == issue.teams$id.issue[i])
  issue.teams$size[i] <- length(index)
  issue.teams$members[i] <- paste(participations$id.contributor[index], collapse = "+")
}
# Team id generation
team.names <- data.frame(sort(unique(issue.teams$members[which(issue.teams$size > 1)])))
names(team.names) <- "members"
team.names$length <- nchar(as.character(team.names$members))
team.names <- team.names[with(team.names, order(length, members)), ]
team.names$id.team <- 1:nrow(team.names)
# Inserting team id
issue.teams <- merge(x = issue.teams, y = team.names[, c(1,3)], by = "members")
# Restructuring the dataset
issue.teams <- issue.teams[, c("id.issue", "id.team", "size", "members")]
issue.teams <- issue.teams[with(issue.teams, order(id.issue)), ]

# Creating team long table
team.composition <- merge(x = participations[, c("id.issue","id.comic", "id.contributor", "year")],
               y = issue.teams,
               by = "id.issue")
# Addition of individuals
index <- which(!is.element(participations$id.issue, issue.teams$id.issue))
individual <- data.frame(participations$id.issue[index],
                         participations$id.comic[index],
                         participations$id.contributor[index],
                         participations$year[index])
names(individual) <- c("id.issue", "id.comic", "id.contributor", "year")
individual$id.team <- NA
individual$size <- 1
individual$members <- individual$id.contributor
# Binding rows
issue.teams <- rbind(issue.teams,
                     individual[, c("id.issue", "id.team", "size", "members")])
team.composition <- rbind(team.composition, individual)
# Inclusion in the participations dataset
participations$key <- with(participations, paste(id.contributor, id.issue))
team.composition$key <- with(team.composition, paste(id.contributor, id.issue))
# Restructure
team.composition <- team.composition[, c("key",
                                         "id.team", "size", "members")]
participations <- merge(x = participations, y = team.composition, by = "key")


# Removing temporary variables
rm(team.composition)
rm(individual)
rm(team.names)

# ===== Aggregating data by issue =====
# Aggregating roles by division of labor
issue.labor <- participations[, c("id.issue", "role.creative", "role.editor",
                                "role.support", "role.special", "multiple")]
# Conversion to numeric variable
index <- which(issue.labor$multiple >1)
issue.labor$multiple[index] <- 1/issue.labor$multiple[index]
issue.labor$role.creative.dec <- with(issue.labor, role.creative * multiple)
issue.labor$role.editor.dec <- with(issue.labor, role.editor * multiple)
issue.labor$role.support.dec <- with(issue.labor, role.support * multiple)
issue.labor$role.special.dec <- with(issue.labor, role.special * multiple)

# Roles by issue
issue.roles.creative <- recast(data = issue.labor,
                               formula =  id.issue ~ role.creative,
                               id.var = c("id.issue", "role.creative"),
                               measure.var = "role.creative.dec", sum)
issue.roles.editor <- recast(data = issue.labor,
                             formula =  id.issue ~ role.editor,
                             id.var = c("id.issue", "role.editor"),
                             measure.var = "role.editor.dec", sum)
issue.roles.support <- recast(data = issue.labor,
                              formula =  id.issue ~ role.support,
                              id.var = c("id.issue", "role.support"),
                              measure.var = "role.support.dec", sum)
issue.roles.special <- recast(data = issue.labor,
                              formula =  id.issue ~ role.special,
                              id.var = c("id.issue", "role.special"),
                              measure.var = "role.special.dec", sum)
# Renaming attributes
names(issue.roles.creative)[3] <- c("count.creative")
names(issue.roles.editor)[3] <- c("count.editor")
names(issue.roles.support)[3] <- c("count.support")
names(issue.roles.special)[3] <- c("count.special")
# Meargin aggregated data
aggre.issues <- merge(x = issue.teams, 
                      y = issue.roles.creative[, c(1,3)],
                      by = "id.issue")
aggre.issues <- merge(x = aggre.issues, 
                      y = issue.roles.editor[, c(1,3)],
                      by = "id.issue")
aggre.issues <- merge(x = aggre.issues, 
                      y = issue.roles.support[, c(1,3)],
                      by = "id.issue")
aggre.issues <- merge(x = aggre.issues, 
                      y = issue.roles.special[, c(1,3)],
                      by = "id.issue")

# Composition of attributes
aggre.issues$prop.creative <- with(data = aggre.issues,
                                   expr = count.creative/(size-count.editor))
aggre.issues$prop.support <- with(data = aggre.issues,
                                   expr = (count.support+count.special)/(size-count.editor))
aggre.issues$ind.single <- aggre.issues$size == 1
aggre.issues$ind.editor <- aggre.issues$count.editor > 0

# Removing temporary variables
rm(issue.labor)
rm(issue.roles.creative)
rm(issue.roles.editor)
rm(issue.roles.support)
rm(issue.roles.special)
rm(issue.teams)


# ===== Aggregation by contributors =====
# Start of activity
cont.start <- recast(data = participations,
                     formula =  id.contributor ~ .,
                     id.var = c("id.contributor"),
                     measure.var = "year", min, na.rm = TRUE)
names(cont.start) <- c("id.contributor", "start")
# Last participations activity
cont.last <- recast(data = participations,
                    formula =  id.contributor ~ .,
                    id.var = c("id.contributor"),
                    measure.var = "year", max, na.rm = TRUE)
names(cont.last) <- c("id.contributor", "last")
# Number of roles in the same issue
cont.roles <- recast(data = participations,
                     formula =  id.contributor ~ role.count,
                     id.var = c("id.contributor", "role.count"),
                     measure.var = "id.issue", length)
names(cont.roles) <- c("id.contributor", "1.role", "2.rol", "3.rol")
# Type of projects worked on
cont.project.type <- recast(data = participations,
                            formula =  id.contributor ~ creator.owned,
                            id.var = c("id.contributor", "creator.owned"),
                            measure.var = "id.issue", length)
names(cont.project.type) <- c("id.contributor", "proj.corporate", "proj.creator")
# ownership rights
cont.ownership <- recast(data = participations,
                         formula =  id.contributor ~ is.owner,
                         id.var = c("id.contributor", "is.owner"),
                         measure.var = "id.issue", length)
names(cont.ownership) <- c("id.contributor", "issue.no.owned", "issues.owned")

# Defining profile
cont.prop.editorial <- recast(data = participations,
                              formula =  id.contributor ~ .,
                              id.var = c("id.contributor"),
                              measure.var = "prop.editorial", mean)
names(cont.prop.editorial)[2] <- "prop.editorial"
cont.prop.creative <- recast(data = participations,
                             formula =  id.contributor ~ .,
                             id.var = c("id.contributor"),
                             measure.var = "prop.creative", mean)
names(cont.prop.creative)[2] <- "prop.creative"
cont.prop.technical <- recast(data = participations,
                              formula =  id.contributor ~ .,
                              id.var = c("id.contributor"),
                              measure.var = "prop.technical", mean)
names(cont.prop.technical)[2] <- "prop.technical"

# Participation count attributes
cont.counter <- data.frame(sort(unique(participations$id.contributor)))
names(cont.counter) <- "id.contributor"
# Experience
cont.counter$count.issue <- NA
cont.counter$count.publi <- NA
cont.counter$count.comic <- NA
#cont.counter$count.teams <- NA
cont.counter$count.colaborations <- NA
cont.counter$genre.mix <- NA
# Previous performance
cont.counter$higher.issue <- NA
cont.counter$lower.issue <- NA
# Collaborations
cont.counter$fav.team.1 <- NA
cont.counter$part.team.1 <- NA
cont.counter$fav.team.2 <- NA
cont.counter$part.team.2 <- NA
cont.counter$fav.team.3 <- NA
cont.counter$part.team.3 <- NA
cont.counter$fav.pub.1 <- NA
cont.counter$part.pub.1 <- NA
cont.counter$fav.pub.2 <- NA
cont.counter$part.pub.2 <- NA
cont.counter$fav.pub.3 <- NA
cont.counter$part.pub.3 <- NA
# Diversity
cont.counter$g.1 <- NA
cont.counter$g.2 <- NA
cont.counter$g.3 <- NA
cont.counter$g.4 <- NA
cont.counter$g.5 <- NA
cont.counter$g.6 <- NA
cont.counter$g.7 <- NA
cont.counter$g.8 <- NA
cont.counter$g.9 <- NA
cont.counter$g.10 <- NA
cont.counter$g.11 <- NA
cont.counter$g.12 <- NA
cont.counter$g.13 <- NA
cont.counter$g.14 <- NA
cont.counter$g.15 <- NA
cont.counter$g.16 <- NA
cont.counter$g.17 <- NA
cont.counter$g.18 <- NA
cont.counter$g.19 <- NA
cont.counter$g.20 <- NA
cont.counter$g.21 <- NA
cont.counter$g.22 <- NA
cont.counter$g.23 <- NA
cont.counter$g.24 <- NA

# Counting cicle
for (i in nrow(cont.counter)) {
  index <- which(participations$id.contributor == cont.counter$id.contributor[i])
  sample <- participations[index, ]
  # Experience
  cont.counter$count.publi[i] <- length(unique(sample$publisher.con))
  cont.counter$count.comic[i] <- length(unique(sample$id.comic))
  cont.counter$count.issue[i] <- length(unique(sample$id.issue))
  cont.counter$count.teams[i] <- length(unique(sample$id.team))
  cont.counter$count.colaborations[i] <- sum(!is.na(sample$id.team))
  cont.counter$genre.mix[i] <- length(unique(sample$genre))
  # Collaborations
  temp <- sort(table(sample$id.team), decreasing = TRUE)
  cont.counter$part.team.1[i] <- temp[1]
  cont.counter$part.team.2[i] <- temp[2]
  cont.counter$part.team.3[i] <- temp[3]
  temp <- names(temp)
  cont.counter$fav.team.1[i] <- temp[1]
  cont.counter$fav.team.2[i] <- temp[2]
  cont.counter$fav.team.3[i] <- temp[3]
  temp <- sort(table(sample$publisher.con), decreasing = TRUE)
  cont.counter$part.pub.1[i] <- temp[1]
  cont.counter$part.pub.2[i] <- temp[2]
  cont.counter$part.pub.3[i] <- temp[3]
  temp <- names(temp)
  cont.counter$fav.pub.1[i] <- temp[1]
  cont.counter$fav.pub.2[i] <- temp[2]
  cont.counter$fav.pub.3[i] <- temp[3]
  # Diversity                                             
  cont.counter$g.1[i] <- sum(sample$g.1, na.rm = TRUE)
  cont.counter$g.2[i] <- sum(sample$g.2, na.rm = TRUE)
  cont.counter$g.3[i] <- sum(sample$g.3, na.rm = TRUE)
  cont.counter$g.4[i] <- sum(sample$g.4, na.rm = TRUE)
  cont.counter$g.5[i] <- sum(sample$g.5, na.rm = TRUE)
  cont.counter$g.6[i] <- sum(sample$g.6, na.rm = TRUE)
  cont.counter$g.7[i] <- sum(sample$g.7, na.rm = TRUE)
  cont.counter$g.8[i] <- sum(sample$g.8, na.rm = TRUE)
  cont.counter$g.9[i] <- sum(sample$g.9, na.rm = TRUE)
  cont.counter$g.10[i] <- sum(sample$g.10, na.rm = TRUE)
  cont.counter$g.11[i] <- sum(sample$g.11, na.rm = TRUE)
  cont.counter$g.12[i] <- sum(sample$g.12, na.rm = TRUE)
  cont.counter$g.13[i] <- sum(sample$g.13, na.rm = TRUE)
  cont.counter$g.14[i] <- sum(sample$g.14, na.rm = TRUE)
  cont.counter$g.15[i] <- sum(sample$g.15, na.rm = TRUE)
  cont.counter$g.16[i] <- sum(sample$g.16, na.rm = TRUE)
  cont.counter$g.17[i] <- sum(sample$g.17, na.rm = TRUE)
  cont.counter$g.18[i] <- sum(sample$g.18, na.rm = TRUE)
  cont.counter$g.19[i] <- sum(sample$g.19, na.rm = TRUE)
  cont.counter$g.20[i] <- sum(sample$g.20, na.rm = TRUE)
  cont.counter$g.21[i] <- sum(sample$g.21, na.rm = TRUE)
  cont.counter$g.22[i] <- sum(sample$g.22, na.rm = TRUE)
  cont.counter$g.23[i] <- sum(sample$g.23, na.rm = TRUE)
  cont.counter$g.24[i] <- sum(sample$g.24, na.rm = TRUE)
  # Previous performance
  cont.counter$higher.issue[i] <- max(sample$current.price, na.rm = TRUE)
  cont.counter$lower.issue[i] <- min(sample$current.price, na.rm = TRUE)
}

# Merging aggregated data
aggre.contributors <- participations[,c("id.contributor", "name")]
aggre.contributors <- aggre.contributors[with(aggre.contributors,
                                              order(id.contributor)), ]
aggre.contributors$dup <- duplicated(aggre.contributors$id.contributor)
aggre.contributors <- subset(x = aggre.contributors,
                             subset = dup == FALSE,
                             select = -dup)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.counter)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.start)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.last)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.roles)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.creative)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.editor)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.support)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.special)

# Special aggregation for genre
aggre.issues_ <- merge(x = aggre.issues, y = issue.genres, by = "id.issue")
index <- which(!is.element(el = aggre.issues$id.issue,
                           set = aggre.issues_$id.issue))
aggre.issues. <- aggre.issues[index, ]
data <- as.data.frame(matrix(nrow = length(index), ncol = 24))
names(data) <- paste("g", 1:24, sep = ".")
aggre.issues. <- cbind(aggre.issues., data)
aggre.issues <- rbind(aggre.issues_, aggre.issues.)

# Removing temporary variables
rm(aggre.issues_)
rm(aggre.issues.)
rm(cont.counter)
rm(cont.count.creative)
rm(cont.count.editor)
rm(cont.count.special)
rm(cont.count.support)
rm(cont.last)
rm(cont.roles)
rm(cont.start)
rm(data)
rm(issue.genres)
rm(participations_)
rm(participations.no)
rm(sample)
rm(sec.comics)
rm(temp)



# ===== Aggregating data by contributor previo =====
# Start of activity
cont.start <- recast(data = participations,
                     formula =  id.contributor ~ .,
                     id.var = c("id.contributor"),
                     measure.var = "year", min, na.rm = TRUE)
names(cont.start) <- c("id.contributor", "start")
# Last participations activity
cont.last <- recast(data = participations,
                       formula =  id.contributor ~ .,
                       id.var = c("id.contributor"),
                       measure.var = "year", max, na.rm = TRUE)
names(cont.last) <- c("id.contributor", "last")
# Number of roles in the same issue
cont.roles <- recast(data = participations,
                     formula =  id.contributor ~ multiple,
                     id.var = c("multiple", "id.contributor"),
                     measure.var = "id.issue", length)
names(cont.roles) <- c("id.contributor", "1.role", "2.rol", "3.rol", "4.rol")

# Number of roles in the same issue
cont.project.type <- recast(data = participations,
                         formula =  id.contributor ~ creator.project,
                         id.var = c("id.contributor", "creator.project"),
                         measure.var = "id.issue", length)
names(cont.project.type) <- c("id.contributor", "own.corporate", "own.creator")

cont.ownership <- recast(data = participations,
                         formula =  id.contributor ~ is.owner,
                         id.var = c("id.contributor", "is.owner"),
                         measure.var = "id.issue", length)
names(cont.ownership) <- c("id.contributor", "issue.no.owned", "issues.owned")

test <- merge(x = cont.project.type, y = cont.ownership[, c(1,3)],
              by = "id.contributor")

# Aggregating profile data
participations$partial.factor <- 1
index <- which(participations$multiple > 1)
participations$partial.factor[index] <- 1/participations$multiple[index]
# Calculating relative participation
participations$partial.creative <- with(data = participations,
                                        expr = role.creative*partial.factor)
participations$partial.editor <- with(data = participations,
                                      expr = role.editor*partial.factor)
participations$partial.support <- with(data = participations,
                                       expr = role.support*partial.factor)
participations$partial.special <- with(data = participations,
                                       expr = role.special*partial.factor)



# Defining profile
cont.count.creative <- recast(data = participations,
                              formula =  id.contributor ~ .,
                              id.var = c("id.contributor"),
                              measure.var = "partial.creative", sum)
names(cont.count.creative)[2] <- "count.creative"
cont.count.editor <- recast(data = participations,
                            formula =  id.contributor ~ .,
                            id.var = c("id.contributor"),
                            measure.var = "partial.editor", sum)
names(cont.count.editor)[2] <- "count.editor"
cont.count.support <- recast(data = participations,
                              formula =  id.contributor ~ .,
                              id.var = c("id.contributor"),
                              measure.var = "partial.support", sum)
names(cont.count.support)[2] <- "count.support"
cont.count.special <- recast(data = participations,
                              formula =  id.contributor ~ .,
                              id.var = c("id.contributor"),
                              measure.var = "partial.special", sum)
names(cont.count.special)[2] <- "count.special"

# Including genre analysis
participations_ <- merge(x = participations, y = issue.genres, by = "id.issue")
index <- which(!is.element(participations$key, participations_$key))
data <- data.frame(matrix(nrow = length(index), ncol = 24))
names(data) <- paste("g", 1:24, sep = ".")
participations.no <- cbind(participations[index,], data)
participations <- rbind(participations_, participations.no)
participations <- merge(x = participations, y = sec.comics[, c(1,3)], by = "id.issue")
# Including price analysis
participations <- merge(x = participations, y = sec.comics[, c(1,2)], by = "id.issue")

# participations counting attributes
cont.counter <- data.frame(sort(unique(participations$id.contributor)))
names(cont.counter) <- "id.contributor"
# Experience
cont.counter$count.publi <- NA
cont.counter$count.comic <- NA
cont.counter$count.issue <- NA
cont.counter$count.teams <- NA
cont.counter$count.colaborations <- NA
cont.counter$genre.mix <- NA
# Previous performance
cont.counter$higher.issue <- NA
cont.counter$lower.issue <- NA
# Collaborations
cont.counter$fav.team.1 <- NA
cont.counter$part.team.1 <- NA
cont.counter$fav.team.2 <- NA
cont.counter$part.team.2 <- NA
cont.counter$fav.team.3 <- NA
cont.counter$part.team.3 <- NA
cont.counter$fav.pub.1 <- NA
cont.counter$part.pub.1 <- NA
cont.counter$fav.pub.2 <- NA
cont.counter$part.pub.2 <- NA
cont.counter$fav.pub.3 <- NA
cont.counter$part.pub.3 <- NA
# Diversity
cont.counter$g.1 <- NA
cont.counter$g.2 <- NA
cont.counter$g.3 <- NA
cont.counter$g.4 <- NA
cont.counter$g.5 <- NA
cont.counter$g.6 <- NA
cont.counter$g.7 <- NA
cont.counter$g.8 <- NA
cont.counter$g.9 <- NA
cont.counter$g.10 <- NA
cont.counter$g.11 <- NA
cont.counter$g.12 <- NA
cont.counter$g.13 <- NA
cont.counter$g.14 <- NA
cont.counter$g.15 <- NA
cont.counter$g.16 <- NA
cont.counter$g.17 <- NA
cont.counter$g.18 <- NA
cont.counter$g.19 <- NA
cont.counter$g.20 <- NA
cont.counter$g.21 <- NA
cont.counter$g.22 <- NA
cont.counter$g.23 <- NA
cont.counter$g.24 <- NA

# Counting cicle
for (i in nrow(cont.counter)) {
  index <- which(participations$id.contributor == cont.counter$id.contributor[i])
  sample <- participations[index, ]
  # Experience
  cont.counter$count.publi[i] <- length(unique(sample$publisher.con))
  cont.counter$count.comic[i] <- length(unique(sample$id.comic))
  cont.counter$count.issue[i] <- length(unique(sample$id.issue))
  cont.counter$count.teams[i] <- length(unique(sample$id.team))
  cont.counter$count.colaborations[i] <- sum(!is.na(sample$id.team))
  cont.counter$genre.mix[i] <- length(unique(sample$genre))
  # Collaborations
  temp <- sort(table(sample$id.team), decreasing = TRUE)
  cont.counter$part.team.1[i] <- temp[1]
  cont.counter$part.team.2[i] <- temp[2]
  cont.counter$part.team.3[i] <- temp[3]
  temp <- names(temp)
  cont.counter$fav.team.1[i] <- temp[1]
  cont.counter$fav.team.2[i] <- temp[2]
  cont.counter$fav.team.3[i] <- temp[3]
  temp <- sort(table(sample$publisher.con), decreasing = TRUE)
  cont.counter$part.pub.1[i] <- temp[1]
  cont.counter$part.pub.2[i] <- temp[2]
  cont.counter$part.pub.3[i] <- temp[3]
  temp <- names(temp)
  cont.counter$fav.pub.1[i] <- temp[1]
  cont.counter$fav.pub.2[i] <- temp[2]
  cont.counter$fav.pub.3[i] <- temp[3]
  # Diversity                                             
  cont.counter$g.1[i] <- sum(sample$g.1, na.rm = TRUE)
  cont.counter$g.2[i] <- sum(sample$g.2, na.rm = TRUE)
  cont.counter$g.3[i] <- sum(sample$g.3, na.rm = TRUE)
  cont.counter$g.4[i] <- sum(sample$g.4, na.rm = TRUE)
  cont.counter$g.5[i] <- sum(sample$g.5, na.rm = TRUE)
  cont.counter$g.6[i] <- sum(sample$g.6, na.rm = TRUE)
  cont.counter$g.7[i] <- sum(sample$g.7, na.rm = TRUE)
  cont.counter$g.8[i] <- sum(sample$g.8, na.rm = TRUE)
  cont.counter$g.9[i] <- sum(sample$g.9, na.rm = TRUE)
  cont.counter$g.10[i] <- sum(sample$g.10, na.rm = TRUE)
  cont.counter$g.11[i] <- sum(sample$g.11, na.rm = TRUE)
  cont.counter$g.12[i] <- sum(sample$g.12, na.rm = TRUE)
  cont.counter$g.13[i] <- sum(sample$g.13, na.rm = TRUE)
  cont.counter$g.14[i] <- sum(sample$g.14, na.rm = TRUE)
  cont.counter$g.15[i] <- sum(sample$g.15, na.rm = TRUE)
  cont.counter$g.16[i] <- sum(sample$g.16, na.rm = TRUE)
  cont.counter$g.17[i] <- sum(sample$g.17, na.rm = TRUE)
  cont.counter$g.18[i] <- sum(sample$g.18, na.rm = TRUE)
  cont.counter$g.19[i] <- sum(sample$g.19, na.rm = TRUE)
  cont.counter$g.20[i] <- sum(sample$g.20, na.rm = TRUE)
  cont.counter$g.21[i] <- sum(sample$g.21, na.rm = TRUE)
  cont.counter$g.22[i] <- sum(sample$g.22, na.rm = TRUE)
  cont.counter$g.23[i] <- sum(sample$g.23, na.rm = TRUE)
  cont.counter$g.24[i] <- sum(sample$g.24, na.rm = TRUE)
  # Previous performance
  cont.counter$higher.issue[i] <- max(sample$current.price, na.rm = TRUE)
  cont.counter$lower.issue[i] <- min(sample$current.price, na.rm = TRUE)
}

# Merging aggregated data
aggre.contributors <- participations[,c("id.contributor", "name")]
aggre.contributors <- aggre.contributors[with(aggre.contributors,
                                              order(id.contributor)), ]
aggre.contributors$dup <- duplicated(aggre.contributors$id.contributor)
aggre.contributors <- subset(x = aggre.contributors,
                             subset = dup == FALSE,
                             select = -dup)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.counter)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.start)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.last)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.roles)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.creative)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.editor)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.support)
aggre.contributors <- merge(x = aggre.contributors, by = "id.contributor",
                            y = cont.count.special)

# Special aggregation for genre
aggre.issues_ <- merge(x = aggre.issues, y = issue.genres, by = "id.issue")
index <- which(!is.element(el = aggre.issues$id.issue,
                           set = aggre.issues_$id.issue))
aggre.issues. <- aggre.issues[index, ]
data <- as.data.frame(matrix(nrow = length(index), ncol = 24))
names(data) <- paste("g", 1:24, sep = ".")
aggre.issues. <- cbind(aggre.issues., data)
aggre.issues <- rbind(aggre.issues_, aggre.issues.)

# Removing temporary variables
rm(aggre.issues_)
rm(aggre.issues.)
rm(cont.counter)
rm(cont.count.creative)
rm(cont.count.editor)
rm(cont.count.special)
rm(cont.count.support)
rm(cont.last)
rm(cont.roles)
rm(cont.start)
rm(data)
rm(issue.genres)
rm(participations_)
rm(participations.no)
rm(sample)
rm(sec.comics)
rm(temp)

# ===== Aggregating data by team ====
#
team.issues <- recast(data = aggre.issues,
                    formula =  id.team ~ .,
                    id.var = c("id.team"),
                    measure.var = "id.issue", length)
names(team.issues)[2] <- c("t")

# Merging team data
team.id <- sort(unique(participations$id.team))
aggre.team <- data.frame(team.id)

# ===== Aggregating activity span =====
# Aggregating data per time
cont.activity <- recast(data = participations,
                        formula =  id.contributor ~ year,
                        id.var = c("id.contributor", "year"),
                        measure.var = "id.issue", length)

team.activity <- recast(data = teams,
                        formula =  id.team ~ year,
                        id.var = c("id.team", "year"),
                        measure.var = "id.issue", length)

#i test
i <- which(cont.counter$id.contributor == 30110)

# ===== Storing datasets as RData files =====
# Removing temporary variables
rm(i)
rm(index)

# Storing the dataset as a RData file
save(list = "participations", file = "1_blackbox/9_contributorIssue.RData")
