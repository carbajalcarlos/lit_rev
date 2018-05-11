# ----- Initialization of the project -----
# Installation and loading of required libraries
require("bibliometrix")

# Importing data from WoS-export
bm <- readFiles("0_data/wos-digital_innovation-2000-2018.bib")
bm <- convert2df(bm, dbsource = "isi", format = "bibtex")

# ----- Data cleaning -----
for (i in 1:length(bm)) {
  if (class(bm[ ,i]) == "character") {
    tolower(bm[ ,i])
  }
}



# Blibliometric analysis
bm_analysis <- biblioAnalysis(bm, sep = ";")
bm_summary <- summary(object = bm_analysis, k = 10, pause = FALSE)
plot(x = bm_analysis, k = 10, pause = TRUE)

# References analysis
