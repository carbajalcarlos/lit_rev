# ----- Initialization of the project -----
# Loading required libraries
#require(bibliometrix, quietly = TRUE)
#require(stringdist, quietly = TRUE)
#require(tm, quietly = TRUE)
#require(countrycode, quietly = TRUE)

# Loading required data files
load(file = "2_blackbox/ws_cleaing.Rdata")

# Forming required structure <-
bm_ds <- subset(x = bg_df, select = c("author", "title", "journal", "journal.iso",
                                      "volume", "number", "pages", "entry.type", "type",
                                      "keywords", "keywords.plus", "abstract",
                                      "affiliation", "cited.references", "times.cited",
                                      "year", "research.areas", "unique.id", "doi"))
