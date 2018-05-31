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

# Obtaining most cited references by article and author 
# bm$CR[3] # Used to define the appropriated separatord fiels
bm_cit_art <- citations(M = bm, field = "article", sep = "; ")
head(bm_cit_art$Cited, n = 12)
bm_cit_aut <- citations(M = bm, field = "author", sep = "; ")
head(bm_cit_aut$Cited, n = 12)

# Obtaining the most cited local references
bm_cit_loc <- localCitations(M = bm,  sep = "; ")
# By Article
head(bm_cit_loc$Papers, n = 12)
# By Author
head(bm_cit_loc$Authors, n = 12)

# Calculating authors dominance factor
bm_dom <- dominance(results = bm_anl, k = 12)
bm_dom

# Calculating the authors h-index
# One author: Yoo Youngjin
bm_hin_yoo <- Hindex(M = bm, authors = "YOO Y", sep = ";", years = 10)
bm_hin_yoo$H
bm_hin_yoo$CitationList
# One author: Yoo Youngjin
bm_hin_nam <- Hindex(M = bm, authors = "NAMBISAN S", sep = ";", years = 10)
bm_hin_nam$H
bm_hin_nam$CitationList
# The 12 most productive authors
authors <- gsub(pattern = ",", replacement = " ", x = names(bm_anl$Authors)[1:12]) # Extracting the 12 most productive authors
bm_hin <- Hindex(M = bm, authors = authors, sep = ";", years = 10)
bm_hin$H
bm_hin$CitationList

# Estimation of the Lotka's Law coefficient
bm_ltk <- lotka(bm_anl)
bm_ltk$AuthorProd
# Estimation coefficients
bm_ltk$Beta # beta
bm_ltk$C # constant
bm_ltk$R2 # Goodness of fit
bm_ltk$p.value # P-value of K-S two sample test
# Distribution comparison plot
bm_ltk_obs <- bm_ltk$AuthorProd[,3] # Observed distribution
bm_ltk_the <- 10^(log10(bm_ltk$C)-2*log10(bm_ltk$AuthorProd[,1])) # Theoretical distribution
plot(x = bm_ltk$AuthorProd[,1], y = bm_ltk_the, col = "red",
     type = "l", ylim = c(0,1), 
     main = "Scientific Productivity",
     xlab = "Articles", ylab = "Frequency of authors") 
lines(x = bm_ltk$AuthorProd[,1], y = bm_ltk_obs, col = "blue")
legend(x = "topright",  lty = c(1,1,1), cex = 1, bty = "n",
       c("Theoretical (B=2)", "Observed"), col = c("red", "blue"))
