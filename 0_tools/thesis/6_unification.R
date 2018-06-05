# Loading source files
load(file = "1_blackbox/9_comics.RData")
load(file = "1_blackbox/10_issues.RData")
load(file = "1_blackbox/10_teams.RData")
load(file = "1_blackbox/10_contributors.RData")
load(file = "1_blackbox/11_publishers.RData")
load(file = "1_blackbox/10_participations.RData")

# ===== Unification of databases =====
# Changing issues names to avoid overlaps
names(issues) <- paste("i", names(issues), sep = ".")
names(teams) <- paste("t", names(teams), sep = ".")
names(contributors) <- paste("c", names(contributors), sep = ".")

# Adding publisher id
cdb <- merge(x = comics[, -32], # Removing publisher
             y = publishers[, -7], # Removing publisher.raw
             by = "id.issue")

# Adding issue information
cdb <- merge(x = cdb, by.x = "id.issue",
             y = issues, by.y = "i.id.issue")

# Splitting teams and invidual issues
index <- which(cdb$i.single.contributor == TRUE)
cdb.s <- cdb[index, ]
cdb.t <- cdb[-index, ]

# Adding team information
cdb.t <- merge(x = cdb.t, by.x = "i.id.team", 
               y = teams, by.y = "t.id.team")

# Adding contributors id
index <- cdb.s$id.issue
index <- with(participations,
              which(is.element(el = id.issue, set = index)))
cdb.s <- merge(x = cdb.s,
               y = participations[index, c(5,6)], # Extracting id.contributor
               by = "id.issue")
# Adding contributors information
cdb.s <- merge(x = cdb.s, by.x = "id.contributor",
               y = contributors, by.y = "c.id.contributor")

# ===== Pairing attributes ===== 
# --- Editting teams dataset ---
# Adding missing attributes
cdb.t$id.contributor <- NA
# Removing attributes
remove <- c(which(names(cdb.t) == "t.members.y"),
            which(names(cdb.t) == "t.fav.pub.1"),
            which(names(cdb.t) == "t.fav.pub.2"),
            which(names(cdb.t) == "t.fav.pub.3"),
            which(names(cdb.t) == "t.part.pub.1"),
            which(names(cdb.t) == "t.part.pub.2"),
            which(names(cdb.t) == "t.part.pub.3"))*-1
if (length(remove) != 0) { cdb.t <- cdb.t[, remove] }

# Reorganizing ID attributes
move <- c(which(names(cdb.t) == "id.issue"),
          which(names(cdb.t) == "id.comic"),
          which(names(cdb.t) == "id.publisher"),
          which(names(cdb.t) == "i.id.team"),
          which(names(cdb.t) == "id.contributor"))
move <- c(move, (1:ncol(cdb.t))[-move])
cdb.t <- cdb.t[, move]

# Ranaming attributes
names(cdb.t)[which(names(cdb.t) == "i.id.team")] <- c("id.team")
names(cdb.t)[which(names(cdb.t) == "t.members.x")] <- c("members")
names(cdb.t)[which(names(cdb.t) == "t.start")] <- c("act.start")
names(cdb.t)[which(names(cdb.t) == "t.last")] <- c("act.end")
names(cdb.t)[which(names(cdb.t) == "t.count.issues")] <- c("count.issues")
names(cdb.t)[which(names(cdb.t) == "t.count.comic")] <- c("count.comic")
names(cdb.t)[which(names(cdb.t) == "t.count.publi")] <- c("count.publi")
names(cdb.t)[which(names(cdb.t) == "t.genre.mix")] <- c("genre.mix")
names(cdb.t)[which(names(cdb.t) == "t.creator.owned")] <- c("ind.projects")
names(cdb.t)[which(names(cdb.t) == "t.owner.in")] <- c("issues.owned")
names(cdb.t)[which(names(cdb.t) == "t.prop.editorial")] <- c("prop.editorial")
names(cdb.t)[which(names(cdb.t) == "t.prop.creatives")] <- c("prop.creative")
names(cdb.t)[which(names(cdb.t) == "t.prop.technicals")] <- c("prop.technical")
names(cdb.t)[which(names(cdb.t) == "t.higher.issue")] <- c("higher.issue")
names(cdb.t)[which(names(cdb.t) == "t.lower.issue")] <- c("lower.issue")
names(cdb.t)[which(names(cdb.t) == "t.gen.1")] <- c("exp.gen.1")
names(cdb.t)[which(names(cdb.t) == "t.gen.2")] <- c("exp.gen.2")
names(cdb.t)[which(names(cdb.t) == "t.gen.3")] <- c("exp.gen.3")
names(cdb.t)[which(names(cdb.t) == "t.gen.4")] <- c("exp.gen.4")
names(cdb.t)[which(names(cdb.t) == "t.gen.5")] <- c("exp.gen.5")
names(cdb.t)[which(names(cdb.t) == "t.gen.6")] <- c("exp.gen.6")
names(cdb.t)[which(names(cdb.t) == "t.gen.7")] <- c("exp.gen.7")
names(cdb.t)[which(names(cdb.t) == "t.gen.8")] <- c("exp.gen.8")
names(cdb.t)[which(names(cdb.t) == "t.gen.9")] <- c("exp.gen.9")
names(cdb.t)[which(names(cdb.t) == "t.gen.10")] <- c("exp.gen.10")
names(cdb.t)[which(names(cdb.t) == "t.gen.11")] <- c("exp.gen.11")
names(cdb.t)[which(names(cdb.t) == "t.gen.12")] <- c("exp.gen.12")
names(cdb.t)[which(names(cdb.t) == "t.gen.13")] <- c("exp.gen.13")
names(cdb.t)[which(names(cdb.t) == "t.gen.14")] <- c("exp.gen.14")
names(cdb.t)[which(names(cdb.t) == "t.gen.15")] <- c("exp.gen.15")
names(cdb.t)[which(names(cdb.t) == "t.gen.16")] <- c("exp.gen.16")
names(cdb.t)[which(names(cdb.t) == "t.gen.17")] <- c("exp.gen.17")
names(cdb.t)[which(names(cdb.t) == "t.gen.18")] <- c("exp.gen.18")
names(cdb.t)[which(names(cdb.t) == "t.gen.19")] <- c("exp.gen.19")
names(cdb.t)[which(names(cdb.t) == "t.gen.20")] <- c("exp.gen.20")
names(cdb.t)[which(names(cdb.t) == "t.gen.21")] <- c("exp.gen.21")
names(cdb.t)[which(names(cdb.t) == "t.gen.22")] <- c("exp.gen.22")
names(cdb.t)[which(names(cdb.t) == "t.gen.23")] <- c("exp.gen.23")
names(cdb.t)[which(names(cdb.t) == "t.gen.24")] <- c("exp.gen.24")

# Selection of attributes
cdb.t <- cdb.t[, c("id.issue", "id.comic", "id.publisher",
                   "id.team", "id.contributor",
                   "title", "type", "issue.num", "var", "var.count", "print",
                   "is.book", "series.num", "vol.num",
                   "genre", "genre.count", "mature",
                   "cover.date", "sales.date", "sales.year", "sales.month",
                   "mask.date", "year.start", "year.end", "year.span",
                   "cover.price", "sales.price",
                   "price.2011", "price.2012", "price.2013", "price.2014",
                   "rank", "units.sold", "circulation",
                   "copyright", "publisher", "ind.col", "col.1", "col.2",
                   "appearances", "appearances.count",
                   "storylines", "storylines.count",
                   "notes", "comments", "i.owner.in", "i.creator.owned",
                   "i.cover.work", "i.count.contributors", "i.single.contributor",
                   "i.count.editors",
                   "i.count.writers", "i.count.artists", "i.count.cover.artists",
                   "i.count.inkers", "i.count.letterers", "i.count.colorists",
                   "i.count.cover.inkers", "i.count.cover.colorists", 
                   "i.only.editorial", "i.only.technical", "i.price.avg",
                   "members", "act.start", "act.end", "count.issues",
                   "count.comic", "count.publi", "genre.mix", "ind.projects",
                   "issues.owned", "prop.editorial", "prop.creative",
                   "prop.technical", "higher.issue", "lower.issue",
                   "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4", 
                   "exp.gen.5", "exp.gen.6", "exp.gen.7", "exp.gen.8",
                   "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
                   "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16", 
                   "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
                   "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24")]

# --- Editting teams dataset ---
# Removing attributes
remove <- c(which(names(cdb.s) == "c.count.teams"),
            which(names(cdb.s) == "c.count.colaborations"),
            which(names(cdb.s) == "c.count.teams.creative"),
            which(names(cdb.s) == "c.count.colaborations.creative"),
            which(names(cdb.s) == "c.1.role"),
            which(names(cdb.s) == "c.2.rol"),
            which(names(cdb.s) == "c.3.rol"),
            which(names(cdb.s) == "c.fav.pub.1"),
            which(names(cdb.s) == "c.part.pub.1"),
            which(names(cdb.s) == "c.fav.pub.2"),
            which(names(cdb.s) == "c.part.pub.2"),
            which(names(cdb.s) == "c.fav.pub.3"),
            which(names(cdb.s) == "c.part.pub.3"),
            which(names(cdb.s) == "c.fav.team.1"),
            which(names(cdb.s) == "c.part.team.1"),
            which(names(cdb.s) == "c.fav.team.2"),
            which(names(cdb.s) == "c.part.team.2"),
            which(names(cdb.s) == "c.fav.team.3"),
            which(names(cdb.s) == "c.part.team.3"),
            which(names(cdb.s) == "c.fav.team.crea.1"),
            which(names(cdb.s) == "c.part.team.crea.1"),
            which(names(cdb.s) == "c.fav.team.crea.2"),
            which(names(cdb.s) == "c.part.team.crea.2"),
            which(names(cdb.s) == "c.fav.team.crea.3"),
            which(names(cdb.s) == "c.part.team.crea.3"))*-1
if (length(remove) != 0) { cdb.s <- cdb.s[, remove] }

# Reorganizing ID attributes
move <- c(which(names(cdb.s) == "id.issue"),
          which(names(cdb.s) == "id.comic"),
          which(names(cdb.s) == "id.publisher"),
          which(names(cdb.s) == "i.id.team"),
          which(names(cdb.s) == "id.contributor"))
move <- c(move, (1:ncol(cdb.s))[-move])

cdb.s <- cdb.s[, move]
# Ranaming attributes
names(cdb.s)[which(names(cdb.s) == "i.id.team")] <- c("id.team")
names(cdb.s)[which(names(cdb.s) == "c.contributor")] <- c("members")
names(cdb.s)[which(names(cdb.s) == "c.start")] <- c("act.start")
names(cdb.s)[which(names(cdb.s) == "c.last")] <- c("act.end")
names(cdb.s)[which(names(cdb.s) == "c.count.issues")] <- c("count.issues")
names(cdb.s)[which(names(cdb.s) == "c.count.comic")] <- c("count.comic")
names(cdb.s)[which(names(cdb.s) == "c.count.publi")] <- c("count.publi")
names(cdb.s)[which(names(cdb.s) == "c.genre.mix")] <- c("genre.mix")
names(cdb.s)[which(names(cdb.s) == "c.proj.creator")] <- c("ind.projects")
names(cdb.s)[which(names(cdb.s) == "c.issues.owned")] <- c("issues.owned")
names(cdb.s)[which(names(cdb.s) == "c.prop.editorial")] <- c("prop.editorial")
names(cdb.s)[which(names(cdb.s) == "c.prop.creative")] <- c("prop.creative")
names(cdb.s)[which(names(cdb.s) == "c.prop.technical")] <- c("prop.technical")
names(cdb.s)[which(names(cdb.s) == "c.higher.issue")] <- c("higher.issue")
names(cdb.s)[which(names(cdb.s) == "c.lower.issue")] <- c("lower.issue")
names(cdb.s)[which(names(cdb.s) == "c.gen.1")] <- c("exp.gen.1")
names(cdb.s)[which(names(cdb.s) == "c.gen.2")] <- c("exp.gen.2")
names(cdb.s)[which(names(cdb.s) == "c.gen.3")] <- c("exp.gen.3")
names(cdb.s)[which(names(cdb.s) == "c.gen.4")] <- c("exp.gen.4")
names(cdb.s)[which(names(cdb.s) == "c.gen.5")] <- c("exp.gen.5")
names(cdb.s)[which(names(cdb.s) == "c.gen.6")] <- c("exp.gen.6")
names(cdb.s)[which(names(cdb.s) == "c.gen.7")] <- c("exp.gen.7")
names(cdb.s)[which(names(cdb.s) == "c.gen.8")] <- c("exp.gen.8")
names(cdb.s)[which(names(cdb.s) == "c.gen.9")] <- c("exp.gen.9")
names(cdb.s)[which(names(cdb.s) == "c.gen.10")] <- c("exp.gen.10")
names(cdb.s)[which(names(cdb.s) == "c.gen.11")] <- c("exp.gen.11")
names(cdb.s)[which(names(cdb.s) == "c.gen.12")] <- c("exp.gen.12")
names(cdb.s)[which(names(cdb.s) == "c.gen.13")] <- c("exp.gen.13")
names(cdb.s)[which(names(cdb.s) == "c.gen.14")] <- c("exp.gen.14")
names(cdb.s)[which(names(cdb.s) == "c.gen.15")] <- c("exp.gen.15")
names(cdb.s)[which(names(cdb.s) == "c.gen.16")] <- c("exp.gen.16")
names(cdb.s)[which(names(cdb.s) == "c.gen.17")] <- c("exp.gen.17")
names(cdb.s)[which(names(cdb.s) == "c.gen.18")] <- c("exp.gen.18")
names(cdb.s)[which(names(cdb.s) == "c.gen.19")] <- c("exp.gen.19")
names(cdb.s)[which(names(cdb.s) == "c.gen.20")] <- c("exp.gen.20")
names(cdb.s)[which(names(cdb.s) == "c.gen.21")] <- c("exp.gen.21")
names(cdb.s)[which(names(cdb.s) == "c.gen.22")] <- c("exp.gen.22")
names(cdb.s)[which(names(cdb.s) == "c.gen.23")] <- c("exp.gen.23")
names(cdb.s)[which(names(cdb.s) == "c.gen.24")] <- c("exp.gen.24")

# Selection of attributes
cdb.s <- cdb.s[, c("id.issue", "id.comic", "id.publisher",
                   "id.team", "id.contributor",
                   "title", "type", "issue.num", "var", "var.count", "print",
                   "is.book", "series.num", "vol.num",
                   "genre", "genre.count", "mature",
                   "cover.date", "sales.date", "sales.year", "sales.month",
                   "mask.date", "year.start", "year.end", "year.span",
                   "cover.price", "sales.price",
                   "price.2011", "price.2012", "price.2013", "price.2014",
                   "rank", "units.sold", "circulation",
                   "copyright", "publisher", "ind.col", "col.1", "col.2",
                   "appearances", "appearances.count",
                   "storylines", "storylines.count",
                   "notes", "comments", "i.owner.in", "i.creator.owned",
                   "i.cover.work", "i.count.contributors", "i.single.contributor",
                   "i.count.editors",
                   "i.count.writers", "i.count.artists", "i.count.cover.artists",
                   "i.count.inkers", "i.count.letterers", "i.count.colorists",
                   "i.count.cover.inkers", "i.count.cover.colorists", 
                   "i.only.editorial", "i.only.technical", "i.price.avg",
                   "members", "act.start", "act.end", "count.issues",
                   "count.comic", "count.publi", "genre.mix", "ind.projects",
                   "issues.owned", "prop.editorial", "prop.creative",
                   "prop.technical", "higher.issue", "lower.issue",
                   "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4", 
                   "exp.gen.5", "exp.gen.6", "exp.gen.7", "exp.gen.8",
                   "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
                   "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16", 
                   "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
                   "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24")]

# Merging datasets
cdb <- rbind(cdb.s, cdb.t)


# ===== Editing unified dataset =====
# Renaming structuring full version
cdb.full <- cdb[, # Identificators
                c("id.issue", "id.comic", "id.publisher",
                  "id.team", "id.contributor",
                  # Descriptors
                  "title", "type", "issue.num", "var", "print",
                  "var.count", "series.num", "vol.num",
                  "is.book", "i.cover.work", 
                  # Genre
                  "genre", "genre.count", "mature",
                  # Content 
                  "appearances", "appearances.count",
                  "storylines", "storylines.count",
                  # Dates
                  "cover.date", "sales.date", "sales.year", "sales.month",
                  "mask.date", "year.start", "year.end", "year.span",
                  # Price
                  "cover.price", "sales.price", "i.price.avg",
                  "price.2011", "price.2012", "price.2013", "price.2014",
                  # Distribution figures
                  "rank", "units.sold", "circulation",
                  # Publisher
                  "copyright", "publisher", "ind.col", "col.1", "col.2",
                  # Additional information
                  "notes", "comments",
                  # Ownership
                  "i.owner.in", "i.creator.owned",
                  # Contributors count
                  "i.count.contributors", "i.single.contributor",
                  # Roles count
                  "i.count.editors",
                  "i.count.writers", "i.count.artists", "i.count.cover.artists",
                  "i.count.inkers", "i.count.letterers", "i.count.colorists",
                  "i.count.cover.inkers", "i.count.cover.colorists",
                  # Special teams considerations
                  "i.only.editorial", "i.only.technical",
                  # Team descriptors
                  "members",
                  # Activity period
                  "act.start", "act.end",
                  # Experience counters
                  "count.issues", "count.comic", "count.publi",
                  # Previous achievementst until date at hand
                  "higher.issue", "lower.issue",
                  # Projects own until date at hand
                  "ind.projects", "issues.owned",
                  # Role experience distribution
                  "prop.editorial", "prop.creative", "prop.technical",
                  # Unique genres combinations
                  "genre.mix",
                  # Record by genre
                  "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4", 
                  "exp.gen.5", "exp.gen.6", "exp.gen.7", "exp.gen.8",
                  "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
                  "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16", 
                  "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
                  "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24")]

names(cdb.full) <- c("id.issue", "id.comic", "id.publisher",
                     "id.team", "id.contributor",
                     # Descriptors
                     "title", "type", "issue.num", "var", "print",
                     "var.count", "series.num", "vol.num",
                     "is.book", "is.cover.special", 
                     # Genre
                     "genre", "genre.count", "mature",
                     # Content 
                     "appearances", "appearances.count",
                     "storylines", "storylines.count",
                     # Dates
                     "cover.date", "date", "year", "month",
                     "mask.date", "comic.start", "comic.end", "comic.duration",
                     # Price
                     "price.cover", "price", "price.avg",
                     "price.2011", "price.2012", "price.2013", "price.2014",
                     # Distribution figures
                     "rank", "units.sold", "circulation",
                     # Publisher
                     "copyright", "publisher", "is.collaboration",
                     "colaborator.1", "colaborator.2",
                     # Additional information
                     "notes", "comments",
                     # Ownership
                     "is.owner.in", "is.creator.owned",
                     # Contributors count
                     "contributors.count", "is.single.contributor",
                     # Roles count
                     "count.editors",
                     "count.writers", "count.artists", "count.cover.artists",
                     "count.inkers", "count.letterers", "count.colorists",
                     "count.cover.inkers", "count.cover.colorists",
                     # Special teams considerations
                     "is.only.editorial", "is.only.technical",
                     # Team descriptors
                     "contributors",
                     # Activity period
                     "team.first", "team.last",
                     # Experience counters
                     "count.issues", "count.comic", "count.publishers",
                     "count.creator.issues", "count.issues.owned",
                     # Previous achievementst until date at hand
                     "higher.issue", "lower.issue",
                     # Role experience distribution
                     "prop.editorial", "prop.creative", "prop.technical",
                     # Unique genres combinations
                     "genre.mix",
                     # Record by genre
                     "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4", 
                     "exp.gen.5", "exp.gen.6", "exp.gen.7", "exp.gen.8",
                     "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
                     "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16", 
                     "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
                     "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24")

# Ordering the observations
cdb.full <- cdb.full[with(cdb.full, order(year, month, 
                                          id.team, id.contributor,
                                          id.comic, id.issue)), ]
# Resetting index
row.names(cdb.full) <- 1:nrow(cdb.full)

# ===== Preparing simplified version =====

cdb <- cdb.full[, c("id.issue", "id.comic", "id.publisher",
                    "id.team", "id.contributor",
                    # Descriptors
                    "title","issue.num", "var.count",
                    "is.book", "is.cover.special", 
                    # Content count
                    "genre.count",
                    "appearances.count",
                    "storylines.count",
                    # Dates
                    "date", "year", "month",
                    "comic.start", "comic.end", "comic.duration",
                    # Price
                    "price", "price.avg",
                    # Distribution figures
                    "rank", "units.sold", # Correlation of 0.89 with circulation
                    # Publisher
                    "publisher", "is.collaboration",
                    # Ownership
                    "is.creator.owned",
                    # Contributors count
                    "contributors.count", "is.single.contributor",
                    # Roles count
                    # Editors
                    "count.editors",
                    # Creative team
                    "count.writers", "count.artists", "count.cover.artists",
                    # Technical team
                    "count.inkers", "count.letterers", "count.colorists",
                    "count.cover.inkers", "count.cover.colorists",
                    # Special teams considerations
                    "is.only.editorial", "is.only.technical",
                    # Activity period
                    "team.first", "team.last",
                    # Experience counters
                    "count.issues", "count.comic", "count.publishers",
                    "count.creator.issues", "count.issues.owned",
                    # Previous achievementst until date at hand
                    "higher.issue", "lower.issue",
                    # Role experience distribution
                    "prop.editorial", "prop.creative", "prop.technical",
                    # Unique genres combinations
                    "genre.mix",
                    # Record by genre
                    "exp.gen.1", "exp.gen.2", "exp.gen.3", "exp.gen.4", 
                    "exp.gen.5", "exp.gen.6", "exp.gen.7", "exp.gen.8",
                    "exp.gen.9", "exp.gen.10", "exp.gen.11", "exp.gen.12",
                    "exp.gen.13", "exp.gen.14", "exp.gen.15", "exp.gen.16", 
                    "exp.gen.17", "exp.gen.18", "exp.gen.19", "exp.gen.20",
                    "exp.gen.21", "exp.gen.22", "exp.gen.23", "exp.gen.24")]

# Removing unrepresentative observations
cdb <- subset(x = cdb, # -4 observations
              subset = is.collaboration == FALSE,
              select = -is.collaboration)
cdb <- subset(x = cdb, # -6 observations
              subset = is.only.editorial == FALSE,
              select = -is.only.editorial)
cdb <- subset(x = cdb, # -2 observations
              subset = is.only.technical == FALSE,
              select = -is.only.technical)
cdb <- subset(x = cdb, # -2 observations
              subset = count.writers != 0 & count.artists != 0)

# ===== Storing full dataset =====
save(list = "cdb.full", file = "2_output/comicsFull.RData")
save(list = "cdb", file = "2_output/comics.RData")
save(list = "contributors", file = "2_output/contributors.RData")
save(list = "participations", file = "2_output/participations.RData")
save(list = "publishers", file = "2_output/publishers.RData")
save(list = "teams", file = "2_output/teams.RData")

# Removing temporary variables
rm(comics)
rm(cdb.s)
rm(cdb.t)
rm(cdb.full)
rm(contributors)
rm(issues)
rm(participations)
rm(publishers)
rm(teams)

rm(index)
rm(move)
rm(remove)
