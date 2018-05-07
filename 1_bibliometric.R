# Installation and loading of required libraries
require("bibliometrix")

# Importing data from WoS-export
bm <- readFiles("0_data/wos-digital_innovation-2000-2018.bib")
bm <- convert2df(bm, dbsource = "isi", format = "bibtex")

# Blibliometric analysis
bm_analysis <- biblioAnalysis(bm, sep = ";")
bm_summary <- summary(object = bm_analysis, k = 10, pause = FALSE)
plot(x = bm_analysis, k = 10, pause = TRUE)

# References analysis
