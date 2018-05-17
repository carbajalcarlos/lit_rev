# Loading required data sources
load(file = "2_output/analysis.RData")
load(file = "2_output/comicsFocal.RData")
load(file = "2_output/comicsFull.RData")

# Loading required libraries
if (FALSE) { install.packages("corrgram") }
require(corrgram)
require(Hmisc)
require(psych)


da <- 
  ka <- sd(log(cdb.full$price*cdb.full$units.sold))
la <- mean(log(cdb.full$price*cdb.full$units.sold))
se <- mean(log(cdb.full$price*cdb.full$units.sold))
se$`log(cdb.full$price * cdb.full$units.sold)` <- scale(se$`log(cdb.full$price * cdb.full$units.sold)` )
mean(da)
sd(da)
hist(da)

1/2.2E-16
1/4.545455e+15

exp(log(100))
exp((-0.05*ka)+la)

mean()

# Iterations tracker
it <- 1

# Sampling the analysis dataset to create a correlogram image
cdb.sam <- sample(1:nrow(cdb), 10000)



cdb.var <- cdb.std[cdb.sam, c("genre.count", "contributors.count",
                              "genre.exp", "team.exp.2", "tenure.min",
                              "tech.crea", "pub.size",
                              "is.creator.owned", "is.edited", "is.book")]
names(cdb.var) <- c("genre.count", "contributors.count",
                    "genre.experience", "team.experience", "team.tenure",
                    "tech.crea", "publisher.size",
                    "is.creator.owned", "is.edited", "is.book")

cdb.var$is.book <- cdb.var$is.book*1 
cdb.var$is.edited <- cdb.var$is.edited*1 
cdb.var$is.creator.owned <- cdb.var$is.creator.owned*1 


if (TRUE) {
  index <- with(cdb.var, 
                which(genre.count > quantile(genre.count, 0.01) &
                        genre.count < quantile(genre.count, 0.99) & 
                        contributors.count > quantile(contributors.count, 0.01) &
                        contributors.count < quantile(contributors.count, 0.99) &
                        genre.experience > quantile(genre.experience, 0.05) &
                        genre.experience < quantile(genre.experience, 0.95) &
                        team.experience > quantile(team.experience, 0.05) &
                        team.experience < quantile(team.experience, 0.95) &
                        publisher.size > quantile(publisher.size, 0.05) &
                        publisher.size < quantile(publisher.size, 0.95)))
  cdb.var <- cdb.var[index, ] 
}

# Creating the correlogram
png(filename = "4_descriptors/4_var_corrG.png",
    width = 1200, height = 1200)
par(oma = c(2,2,2,2),
    mar =  c(5.1, 4.1, 4.1, 2.1),)
corrgram(cdb.var,
         main = "Correlogram of the predictive variables",
         lower.panel = panel.ellipse,
         upper.panel = panel.pie,
         diag.panel = panel.density,
         text.panel =  panel.txt,
         labels = c(1:ncol(cdb.var)),
         gap = 1,
         lwd = 2.5,
         #cex.cor = 3,
         cex = 2.5,  # Control the dots size in the scatterplot
         cex.labels = 6, # Control the variable names in the diagonal
         cex.main = 3) # Cntrols the main title
it <- it*-1
dev.off()

# Exporting the correlation matrix
cdb.des <- cdb[, c("genre.count", "contributors.count",
                   "genre.exp", "pre.issues", "tenure.min", 
                   "tech.crea", "pub.size", "is.creator.owned", 
                   "is.editor", "is.book")]
cdb.des$pub.size <- with(cdb, 
                         scale(pub.sales)+scale(pub.cont)+scale(pub.issues))


names(cdb.des) <- c("genre.count", "contributors.count",
                    "genre.experience", "team.experience", "team.tenure",
                    "tech.crea", "publisher.size",
                    "is.creator.owned", "is.edited", "is.book")

cdb.des$is.book <- cdb.des$is.book*1 
cdb.des$is.edited <- cdb.des$is.edited*1 
cdb.des$is.creator.owned <- cdb.des$is.creator.owned*1 


sink(file = "4_descriptors/4_predictorsDescr.txt")
cat("=============================\n\n")
cat("Description of the predictable variables\n")
cat("=============================\n")
psych::describe(x = cdb.des)
sink()

# Exporting the correlation matrix
sink(file = "4_descriptors/4_predictorsCorr.txt")
cat("=============================\n\n")
cat("Correlation matix of the predictable variables\n")
cat("=============================\n")
corr.test(x = cdb.des)
sink()

# Exporting the correlation matrix standardized
cdb.st <- cdb.std[, c("genre.count", "contributors.count",
                   "genre.exp","team.exp.2", "tenure.min", 
                   "tech.crea", "pub.size", "is.creator.owned",
                   "is.edited", "is.book")]
names(cdb.st) <- paste(c("genre.count", "contributors.count",
                    "genre.experience", "team.experience", "team.tenure",
                    "tech.crea", "publisher.size",
                    "is.creator.owned", "is.edited", "is.book"),
                    ".std", sep = "")

cdb.st$is.book.std <- cdb.st$is.book.std*1 
cdb.st$is.edited.std <- cdb.st$is.edited.std*1 
cdb.st$is.creator.owned.std <- cdb.st$is.creator.owned.std*1 


sink(file = "4_descriptors/4_predictorsDescr.txt")
cat("=============================\n\n")
cat("Description of the predictable variables\n")
cat("=============================\n")
psych::describe(x = cdb.st)
sink()

# Exporting the correlation matrix
sink(file = "4_descriptors/4_predictorsCorr.txt")
cat("=============================\n\n")
cat("Correlation matix of the predictable variables\n")
cat("=============================\n")
cor(x = cdb.st); cor(cdb.var)
sink()


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(st.model, cex = 3)


plot(st.model)




anova(st.model)
