# Loading data sources
load(file = "2_output/comicsFocal.RData")

# Installing libraries
if (FALSE) {
  install.packages("Hmisc")
  install.packages("psych")
  install.packages("pastecs")
  install.packages("sm")
  install.packages("DAAG")
  install.packages("MASS")
  
}

# Loading required libraries
require(psych)
require(Hmisc)
require(pastecs)
require(sm)
require(DAAG)
library(MASS)

# ===== Preparation of the analysis variables =====
cdb <- cdb[, # Identification variable
           c("id.issue",
             # Dependant variables
             "revenue", "deviation",
             # Valuations
             "price.avg", "revenue.rank",
             # Devendat variables constructs
             "price", "units.sold",
             # Date variables
             "year", "g.years", 
             # Comic descripion
             "issue.num", "var.count", "comic.duration",
             "is.book", "is.cover.special", 
             # Ownership
             "is.creator.owned", "is.owner.in",
             # Issue content
             "genre.count", "appearances.count", "storylines.count",
             # Team composition
             "contributors.count", "is.single.contributor",
             "prop.editorial", "prop.creative", "prop.technical",
             # Team experience 
             "tenure.mean", "tenure.min",
             "pre.issues", "pre.comics", "pre.publisher",
             "pre.creator.issues","pre.creator.comics",
             "pre.issues.owned", "pre.comics.owned",
             # Team diversity
             "genre.exp", "genre.mix.exp",
             # Team previous performance
             "pre.higher.issue", "since.higher",
             "pre.lower.issue", "since.lower",
             "workload", "workload.inc",
             # Publisher data
             "pub.issues", "pub.sales", "pub.cont")]
# Renaming dependant variables
names(cdb)[which(names(cdb) == "deviation")] <- "revenue.dev"
names(cdb)[which(names(cdb) == "price.avg")] <- "value"
cdb$value.dev <- NA
names(cdb)[which(names(cdb) == "revenue.rank")] <- "rank"
cdb$rank.dev <- NA
cdb <- cdb[, c(1:3, 45, 4, 46, 5:44)]
# Adding deviation values
cdb$revenue.dev <- as.numeric(scale(cdb$revenue))
cdb$value.dev <- as.numeric(scale(cdb$value))
cdb$rank.dev <- as.numeric(scale(cdb$rank))

# ===== TG model simmulation =====
# Subsetting required variables
tg.cdb <- cdb[, # Dependant variables
              c("id.issue",
                "revenue", "revenue.dev", 
                "value" , "value.dev", 
                "rank", "rank.dev", 
                # Time variable
                "year",
                # Independant variables
                "units.sold", # Circulation
                "genre.count", # Number of genres
                "contributors.count", # 
                "is.single.contributor",
                "tenure.min",
                "genre.exp",
                "pre.issues",
                "pre.higher.issue",
                "pre.lower.issue",
                "pub.issues",
                "pub.sales",
                "pub.cont",
                "workload", 
                "workload.inc")]
# Within-year standardization
for (i in unique(tg.cdb$year)) {
  index <- which(tg.cdb$year == i)
  # Dependant variables
  tg.cdb$revenue[index] <- scale(tg.cdb$revenue[index])
  tg.cdb$value[index] <- scale(tg.cdb$value[index])
  tg.cdb$rank[index] <-scale(tg.cdb$rank[index])
  # Independant variables
  tg.cdb$units.sold[index] <-scale(tg.cdb$units.sold[index])
  # Number of contributors
  tg.cdb$genre.count[index] <-scale(tg.cdb$genre.count[index])
  tg.cdb$contributors.count[index] <-scale(tg.cdb$contributors.count[index])
  # Experience
  tg.cdb$tenure.min[index] <-scale(tg.cdb$tenure.min[index])
  # Knowledge diversity
  tg.cdb$genre.exp[index] <-scale(tg.cdb$genre.exp[index])
  # Knowledge experience
  tg.cdb$pre.issues[index] <-scale(tg.cdb$pre.issues[index])
  # PRevious performance
  tg.cdb$pre.higher.issue[index] <-scale(tg.cdb$pre.higher.issue[index])
  tg.cdb$pre.lower.issue[index] <-scale(tg.cdb$pre.lower.issue[index])
  # Publisher
  tg.cdb$pub.issues[index] <-scale(tg.cdb$pub.issues[index])
  tg.cdb$pub.sales[index] <-scale(tg.cdb$pub.sales[index])
  tg.cdb$pub.cont[index] <-scale(tg.cdb$pub.cont[index])
  # Workload
  tg.cdb$workload[index] <-scale(tg.cdb$workload[index])
}
# Adding general deviation
tg.cdb$revenue.dev <- as.numeric(scale(tg.cdb$revenue))
tg.cdb$value.dev <- as.numeric(scale(tg.cdb$value))
tg.cdb$rank.dev <- as.numeric(scale(tg.cdb$rank))
# Adding tenure squared variable
tg.cdb$tenure.2 <- (tg.cdb$tenure.min^2)/100 
# Adding publisher size variable
tg.cdb$pub.size <- with(tg.cdb, pub.issues*pub.sales*pub.cont)
# Adding year divisors
tg.cdb$y97.02 <- tg.cdb$year >= 1997 & tg.cdb$year <= 2002
tg.cdb$y03.08 <- tg.cdb$year >= 2003 & tg.cdb$year <= 2008
tg.cdb$y09.14 <- tg.cdb$year >= 2009 & tg.cdb$year <= 2014

# Description of the dataset for the analysis
sink(file = "4_descriptors/1_tgDataset.txt")
cat("=============================\n\n")
cat("Taylor and Greve dataset summary\n")
cat("=============================\n")
Hmisc::describe(tg.cdb)
sink()
# Description of the dataset for the analysis
sink(file = "4_descriptors/1_tgCorrelation.txt")
cat("=============================\n\n")
cat("Taylor and Greve correlation summary\n")
cat("=============================\n")
corr.test(x = tg.cdb)
sink()

# Calculating regression model for revenue
tg.revenue <- lm( data = tg.cdb, 
                  formula = revenue ~ genre.count + workload.inc + pre.higher.issue + is.single.contributor + contributors.count + genre.exp + pre.issues + workload + tenure.min + tenure.2 + pub.size)
# Comparison analysis for the models
sink(file = "4_descriptors/1_tgLinearModel.txt")
cat("=============================\n\n")
cat("Taylor and Greve linear model summary\n")
cat("=============================\n")
summary(tg.revenue)
sink()

# ===== Thesis model fitting =====
# Adding composed attributes
cdb$tenure.2 <- (cdb$tenure.min^2)/100
cdb$tenure.mean.2 <- (cdb$tenure.mean^2)/100
cdb$genre.div <- with(cdb, as.numeric(scale(genre.mix.exp/genre.exp)))
cdb$team.exp <- with(cdb, pre.issues*pre.comics*pre.publisher)
cdb$team.exp.2 <- with(cdb, as.numeric(scale((pre.issues/pre.comics)*pre.publisher)))
cdb$pub.size <- with(cdb, pub.issues*pub.sales*pub.cont)
cdb$pub.size.2 <- with(cdb, as.numeric(scale((pub.issues/pub.sales)*pub.cont)))
cdb$prop.co <-  with(cdb, pre.creator.issues/pre.issues)

# Scaling team proportions
cdb$tech.crea <- with(cdb, (prop.creative-0.5)*2*(prop.creative+prop.technical))
cdb$is.edited <- cdb$prop.editorial > 0


# Description of the dataset for the analysis
sink(file = "4_descriptors/2_fullDataset.txt")
cat("=============================\n\n")
cat("Full analysis dataset dataset summary\n")
cat("=============================\n")
Hmisc::describe(cdb)
sink()
# Description of the dataset for the analysis
sink(file = "4_descriptors/2_fullCorrelation.txt")
cat("=============================\n\n")
cat("Full analysis dataset correlation summary\n")
cat("=============================\n")
avoid <- c(1, which(names(cdb) == "g.years"))
corr.test(x = cdb[, -avoid])
sink()

# Testing the linear model
index <- sample(1:nrow(cdb), size = 40000)
cdb.train <- cdb[index, ]
cdb.test <- cdb[-index, ]

th.model <- glm( data = cdb.train,
                formula = revenue.st ~ 
                  is.creator.owned + 
                  is.book +
                  is.edited + 
                  comic.duration + 
                  team.exp.2*is.creator.owned + 
                  genre.exp*is.creator.owned + 
                  tech.crea*is.creator.owned +
                  tenure.min*is.creator.owned +
                  genre.count + since.lower +
                  contributors.count*tech.crea +
                  tenure.2 + pub.size*is.creator.owned,
                family = quasipoisson(link = "log"))
summary(th.model)

# Cross-validation
#cdb.cv <- cv.glm(data = cdb.train, glmfit = th.model, K = 3) # 3 fold cross-validation


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(st.model, cex = 3)
cv.glm(data = cdb, glmfit = th.model, K = 10)



# Description of the general linear model with logarithm link
sink(file = "4_descriptors/3_glmSummary.txt")
cat("================================================================================\n\n")
cat("\tLinear regression model using the GLM function with log link\n\n")
cat("================================================================================\n")
summary(th.model)
cat("--------------------------------------------------------------------------------\n\n")
cat("\t Linear Model Coefficients\n\n")
cat("--------------------------------------------------------------------------------\n")
coefficients(th.model)
cat("--------------------------------------------------------------------------------\n\n")
cat("\tANOVA analysis\n\n")
cat("--------------------------------------------------------------------------------\n")
anova(th.model)
sink()
# Printing graphical results
png(filename = "4_descriptors/3_glmPlot.png",
    width = 1200, height = 900)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(th.model)
dev.off()



anova(th.model)

# Within-year standarization
# Subsetting required variables
cdb.std <- cdb[, # Dependant variables
              c("revenue",
                "year",
                "g.years",
                "is.creator.owned", 
                "is.single.contributor",
                "is.book",
                "is.edited",  
                "since.lower",
                "since.higher",
                "pre.higher.issue",
                "pre.lower.issue",
                "genre.count",
                "workload",
                "comic.duration",
                "pub.size",
                "team.exp.2", 
                "genre.exp",
                "contributors.count",
                "tech.crea",
                "tenure.mean",
                "tenure.min",
                "tenure.2")]

cdb.std$revenue.st <- NA
cdb.std$revenue.log <- NA
cdb.std$log.higher.issue <- NA
# Within-year standardization
for (i in unique(cdb.std$year)) {
  index <- which(cdb.std$year == i)
  # Dependant variables
  cdb.std$revenue.st[index] <- scale(cdb.std$revenue[index])
  cdb.std$revenue.log[index] <- scale(log(cdb.std$revenue[index]))
  
  cdb.std$since.higher[index] <- scale(cdb.std$since.higher[index])
  cdb.std$since.lower[index] <- scale(cdb.std$since.lower[index])
  cdb.std$pre.higher.issue[index] <- scale(cdb.std$pre.higher.issue[index])
  cdb.std$pre.lower.issue[index] <- scale(cdb.std$pre.lower.issue[index])
  cdb.std$genre.count[index] <-scale(cdb.std$genre.count[index])
  cdb.std$workload[index] <-scale(cdb.std$workload[index])
  cdb.std$comic.duration[index] <-scale(cdb.std$comic.duration[index])
  cdb.std$pub.size[index] <-scale(cdb.std$pub.size[index])
  cdb.std$team.exp.2[index] <-scale(cdb.std$team.exp.2[index])
  cdb.std$genre.exp[index] <-scale(cdb.std$genre.exp[index])
  cdb.std$contributors.count[index] <-scale(cdb.std$contributors.count[index])
  cdb.std$tech.crea[index] <-scale(cdb.std$tech.crea[index])
  cdb.std$tenure.mean[index] <-scale(cdb.std$tenure.mean[index])
  cdb.std$tenure.min[index] <-scale(cdb.std$tenure.min[index])
}
cdb.std$tenure.2 <- (cdb.std$tenure.mean^2)/100




# Testing the linear model
index <- sample(1:nrow(cdb.std), size = 5000)
cdb.std.train <- cdb.std[index, ]
cdb.std.test <- cdb.std[-index, ]



st.model <- lm( data = cdb.std,
                formula = revenue.log ~ 
                  genre.count +
                  is.edited * is.book +
                  is.edited * tech.crea +
                  is.edited * contributors.count +
                  is.creator.owned * pub.size +
                  is.creator.owned * is.book +
                  is.creator.owned * tech.crea +
                  is.creator.owned * team.exp.2 + 
                  is.creator.owned * genre.exp +
                  is.creator.owned * tenure.min)
summary(st.model)
anova(st.model)
# diagnostic plots
png(filename = "4_descriptors/4_lm_plot.png",
    width = 1200, height = 1200)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
par(oma = c(2,3,2,2),
    mar =  c(5.1, 6.1, 4.1, 2.1))
    #mfrow = c(2,2))
plot(st.model,
     lwd = 4,
     cex = 2,
     cex.caption = 3,
     cex.axis = 3,
     cex.lab = 3,
     main = "", id.n = 0)
dev.off()


cdb.desc <- cdb[, # Dependant variables
                c("revenue",
                  "rank",
                  "genre.count", 
                  "genre.exp",
                  "contributors.count",
                  "team.exp.2",
                  "tenure.min", 
                  "tech.crea",
                  "pub.size",
                  "is.creator.owned",  
                  "is.book",
                  "is.edited")]

names(cdb.desc)[2] <- "revenue.log"
cdb.desc$revenue.log <- cdb.std$revenue.log


corr.test(cdb.desc[, c(3:12)])



# Useful functions 
coefficients(st.model)
confint(st.model, level=0.95)
#fitted(st.model)
#residuals(st.model)
anova(st.model)
vcov(st.model)
cdb.st.influ <- influence(st.model)

# Cross-validation
cdb.st.cv <- cv.lm(data = cdb.std.test, form.lm = th.model, m = 3) # 3 fold cross-validation
summary(cdb.st.cv)
# Variable selection
step <- stepAIC(st.model, direction="both")
step$anova # display results


# graphical representation
model.pred <- predict(th.model)
model.resi <- resid(th.model)
plot(model.pred, model.resi)

summary(tg.revenue)
anova(tg.revenue)

anova(th.model, st.model)
cdb$
  
  
# Testing the linear model
anova(th.model, tg.model)

# ==== Store the dataset as RData file
save(list = c("tg.cdb", "tg.revenue"), file = "2_output/cdbTG.RData")
save(list = c("cdb.std", "st.model"), file = "2_output/analysis.RData")

# ===== Generation of basic descriptive graphs =====
# Units sold
png(filename = "4_descriptors/1_graphUnitsSold.png",
    width = 800, height = 600)
par(mfrow=c(2,2)) 
boxplot(formula = units.sold ~ g.years, data = cdb, 
        main = "Units sold by year")
# Kernel density plot
plot(density(cdb$units.sold), 
     main = "Units sold")
# Histogram 
hist(cdb$units.sold, freq = FALSE, 
     main = "Units sold")
lines(density(cdb$units.sold))
#rug(jitter(cdb$units.sold)) 
# Comparative density
sm.density.compare(x = cdb$units.sold, group = cdb$year)
title(main = "Units sold by year")
dev.off()

# Contributors count
png(filename = "4_descriptors/1_graphContributors.png",
    width = 800, height = 600)
par(mfrow=c(2,2)) 
boxplot(formula = contributors.count ~ g.years, data = cdb, 
        main = "Contributors count by year")
# Kernel density plot
plot(density(cdb$contributors.count), 
     main = "Contributors count")
# Histogram 
hist(cdb$units.sold, freq = FALSE, 
     main = "Contributors count")
lines(density(cdb$contributors.count))
#rug(jitter(cdb$units.sold)) 
# Comparative density
sm.density.compare(x = cdb$contributors.count, group = cdb$year)
title(main = "Contributors count by year")
dev.off()

# Number of issues
png(filename = "4_descriptors/1_graphIssues.png",
    width = 800, height = 600)
par(mfrow=c(2,1)) 
#boxplot(formula = sales.year ~ g.years, data = cdb,
#        main = "Contributors count by year")
# Kernel density plot
plot(density(cdb$year), 
     main = "Issues publisherd by year")
# Histogram 
hist(cdb$sales.year, freq = FALSE, 
     main = "Contributors count")
lines(density(cdb$sales.year))
#rug(jitter(cdb$units.sold)) 
# Comparative density
#sm.density.compare(x = cdb$contributors.count, group = cdb$year)
#title(main = "Contributors count by year")
dev.off()


# ==== Linear regression model summary =====


# Description of the standardize linear model
sink(file = "4_descriptors/3_stdSummary.txt")
cat("================================================================================\n\n")
cat("\t Linear regression model using standarized values\n\n")
cat("================================================================================\n")
psych::describe(cdb.desc[, c(1:9)])
Hmisc::describe(cdb.desc)
cat("--------------------------------------------------------------------------------\n\n")
cat("\t Correlation factors among predictive variables \n\n")
cat("--------------------------------------------------------------------------------\n")
corr.test(cdb.desc[, c(3:12)])
cat("--------------------------------------------------------------------------------\n\n")
cat("\t Linear Model fitting summary \n\n")
cat("--------------------------------------------------------------------------------\n")
summary(st.model)
cat("--------------------------------------------------------------------------------\n\n")
cat("\t Linear Model coefficients\n\n")
cat("--------------------------------------------------------------------------------\n")
coefficients(st.model)
cat("--------------------------------------------------------------------------------\n\n")
cat("\tANOVA analysis\n\n")
cat("--------------------------------------------------------------------------------\n")
anova(st.model)
sink()




# Printing graphical results
png(filename = "4_descriptors/4_revenuelinearization.png",
    width = 1200, height = 900)
par(oma = c(2,1,1,1),
    mar =  c(7, 6, 4.1, 2.1),
    mfrow = c(2,2)) 

boxplot(formula = revenue ~ g.years, data = cdb.std, 
        main = "Issues revenue by year",
        cex = 2,
        ylab = "Revenue in USD",
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5,
        las = 3)

plot(density(cdb.std$revenue), 
     main = "Revenue density",
     cex = 2,
     cex.lab = 2,
     cex.axis = 2,
     cex.main = 2.5,
     xlab = "Revenue in USD")

boxplot(formula = revenue.log ~ g.years, data = cdb.std,
        main = "Issues revenue by year after processing",
        cex = 2,
        ylab = "Revenue in USD",
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5,
        las = 3)
plot(density(cdb.std$revenue.log), 
     main = "Revenue density after processing",
     cex = 2,
     cex.lab = 2,
     cex.axis = 2,
     cex.main = 2.5,
     xlab = "Standardized value")
dev.off()
