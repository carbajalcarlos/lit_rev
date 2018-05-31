# ----- Initialization of the project -----
# Installation and loading of required libraries
require(bibliometrix, quietly = TRUE)

# Importing data from WoS-export
load(file = "2_blackbox/1_bm.Rdata")

# ----- Blibliometric analysis -----
bm_anl <- biblioAnalysis(bm, sep = "; ")
# Printing bibliometric summary
bm_smm <- summary(object = bm_anl, k = 12, pause = FALSE)
# Printing production graphs
plot(x = bm_anl, k = 12, pause = FALSE)
# Obtaining most cited references by article cited and by author cited
# bm$CR[3] # Used to define the appropriated separatord fiels
bm_cit_art <- citations(M = bm, field = "article", sep = "; ")
head(bm_cit_art$Cited, n = 10)
bm_cit_aut <- citations(M = bm, field = "author", sep = "; ")
head(bm_cit_aut$Cited, n = 10)
