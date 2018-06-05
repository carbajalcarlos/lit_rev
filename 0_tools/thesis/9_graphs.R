require(plotrix)


# ===== Plotting revenue =====
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

# ===== Printing creator-owned ===== 
load("2_output/comicsFull.RData")
lbl <- c("Publisher-owned - ", "Creator-owned - ")
issues <- table(cdb.full$is.creator.owned*1)
issuesprop <-round(issues/sum(issues)*100,1)
comics <- cdb.full[which(duplicated(cdb.full$id.comic) == FALSE),]
comics <- unname(table(comics$is.creator.owned*1)[c(1,2)])
comicsprop <- round(unname(comics/sum(comics)*100),1)

# Printing
png(filename = "4_descriptors/4_ownership.png",
    width = 1200, height = 350)
par(oma = c(2,2,2,2),
    mar =  c(1, 1, 1, 4),
    mfrow = c(1,2)) 

fan.plot(x = issues,
         labels = paste(lbl, issuesprop, "%", sep = ""),
         main = "Ownership of issues published",
         max.span = pi, col = c("gray70", "gray30"), 
         ticks = 5,
         align = "right",
         labelpos = c(pi*2/3, pi*1/3))

fan.plot(x = comics,
         labels = paste(lbl, comicsprop, "%", sep = ""),
         main = "Ownership of comic book titles",
         max.span = pi, col = c("gray70", "gray30"), 
         ticks = 5,
         align = "right",
         labelpos = c(pi*2/3, pi*1/3))
dev.off()

# ===== Printing genre experience creator-owned ===== 
load(file = "2_output/comicsFull.RData")
con <- cdb.full[which(duplicated(cdb.full$id.team) == FALSE), ]
ind <- cdb.full[which(duplicated(cdb.full$id.contributor) == FALSE), ]
team <- rbind(con, ind)

false <- table(team$genre.count[which(team$is.creator.owned == FALSE)])
true <- table(team$genre.count[which(team$is.creator.owned == TRUE)])

top <- round(matrix(data = c(false[1:4]/sum(false), sum(false[5:length(false)])/sum(false), 
                       true[1:4]/sum(true),  sum(true[5:length(true)])/sum(true)),
              ncol = 5,
              byrow = TRUE )*100,1)


# Printing
png(filename = "4_descriptors/4_genre_exp.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("1","2","3","4","5+"),
        ylab = "Porcentage of observations",
        xlab = "Genre count",
        ylim = c(0,80),
        axes = FALSE,
        cex = 2,
        col = c("gray70", "gray30"), 
        main = "Genre experience according to ownership",
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:8)*10,
     labels = paste((0:8)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ===== Printing team experience creator-owned ===== 
load(file = "2_output/comicsFull.RData")
con <- cdb.full[which(duplicated(cdb.full$id.team) == FALSE), ]
ind <- cdb.full[which(duplicated(cdb.full$id.contributor) == FALSE), ]
team <- rbind(con, ind)

false <- team$count.issues[which(team$is.creator.owned == FALSE)]
true <- team$count.issues[which(team$is.creator.owned == TRUE)]


f1 <- length(false[which(false <=1)])
f2 <- length(false[which(false > 1 & false <=2)])
f3 <- length(false[which(false > 2 & false <=3)])
f4 <- length(false[which(false > 3 & false <=4)])
f5 <- length(false[which(false > 4)])

t1 <- length(true[which(true <=1)])
t2 <- length(true[which(true > 1 & true <=2)])
t3 <- length(true[which(true > 2 & true <=3)])
t4 <- length(true[which(true > 3 & true <=4)])
t5 <- length(true[which(true > 4)])

true <- c(t1,t2,t3,t4,t5)
false <- c(f1,f2,f3,f4,f5)

top <- round(matrix(data = c(false/sum(false), true/sum(true)), ncol = 5, byrow = TRUE)*100,1)

# Printing
png(filename = "4_descriptors/4_team_exp.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("1","2","3","4","5+"),
        ylab = "Porcentage of observations",
        xlab = "Team collaborations count",
        ylim = c(0,70),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Team experience according to ownership",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:7)*10,
     labels = paste((0:7)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

png(filename = "4_descriptors/4_debut.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
test <-hist(contributors$start[which(contributors$start < 2015)], 
            xlim = c(1970,2020),
            ylim = c(0,4000),
            main = "Contributors first publication",
            xlab = "Year of first publication",
            ylab = "Number of contributors",
            col = "gray70",
            cex = 2,
            cex.lab = 2,
            cex.axis = 2,
            cex.main = 2.5)
dev.off()

# ===== Printing tenure creator-owned ===== 
load(file = "2_output/comicsFull.RData")

false <- cdb$tenure.min[which(cdb$is.creator.owned == FALSE & cdb$tenure.min > 0)]
true <- cdb$tenure.min[which(cdb$is.creator.owned == TRUE & cdb$tenure.min > 0)]

hist(true)
hist(false)

f1 <- length(false[which(false <= 8)])
f2 <- length(false[which(false > 8 & false <= 16)])
f3 <- length(false[which(false > 16 & false <= 24)])
f4 <- length(false[which(false > 24 & false <= 32)])
f5 <- length(false[which(false > 32)])

t1 <- length(true[which(true <= 8)])
t2 <- length(true[which(true > 8 & true <= 16)])
t3 <- length(true[which(true > 16 & true <= 24)])
t4 <- length(true[which(true > 24 & true <= 32)])
t5 <- length(true[which(true > 32)])

true.m <- c(t1,t2,t3,t4,t5)
false.m <- c(f1,f2,f3,f4,f5)



top <- round(matrix(data = c(false.m/sum(false.m), true.m/sum(true.m)), nrow = 2, byrow = TRUE)*100,1)

# Printing
png(filename = "4_descriptors/4_tenure.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("0-8","9-16","17-24","24-32","33+"),
        ylab = "Porcentage of observations",
        xlab = "Tenure in years",
        ylim = c(0,40),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Team tenure according to ownership",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:4)*10,
     labels = paste((0:4)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ===== Printing team proportion creator-owned ===== 
  load(file = "2_output/comicsFull.RData")

false <- cdb$tech.crea[which(cdb$is.creator.owned == FALSE & cdb$tenure.min > 0)]
true <- cdb$tech.crea[which(cdb$is.creator.owned == TRUE & cdb$tenure.min > 0)]

hist(true)
hist(false)

f1 <- length(false[which(false <= -6)])
f2 <- length(false[which(false > -.6 & false <= -.2)])
f3 <- length(false[which(false > -.2 & false <= .2)])
f4 <- length(false[which(false > .2 & false <= .6)])
f5 <- length(false[which(false > .6)])

t1 <- length(true[which(true <= -.6)])
t2 <- length(true[which(true > -.6 & true <= -.2)])
t3 <- length(true[which(true > -.2 & true <= .2)])
t4 <- length(true[which(true > .2 & true <= .6)])
t5 <- length(true[which(true > .6)])

true.m <- c(t1,t2,t3,t4,t5)
false.m <- c(f1,f2,f3,f4,f5)



top <- round(matrix(data = c(false.m/sum(false.m), true.m/sum(true.m)), 
                    nrow = 2, byrow = TRUE)*100,1)

high <- 60
#ceiling(max(top)/10)*10

# Printing
png(filename = "4_descriptors/4_team_prop.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
test <- barplot(top, 
        beside = TRUE, 
        names.arg = c("High Tech","Low Tech","Balanced","Low Crea","High Crea"),
        ylab = "Porcentage of observations",
        xlab = "Team composition",
        ylim = c(0,high),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Team composition according to ownership",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:high)*10,
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ===== Printing publisher size creator-owned ===== 
load(file = "2_output/comicsFull.RData")

false <- cdb.std$pub.size[which(cdb.std$is.creator.owned == FALSE & cdb$tenure.min > 0)]
true <- cdb.std$pub.size[which(cdb.std$is.creator.owned == TRUE & cdb$tenure.min > 0)]

hist(true)
hist(false)
quantile(true, c(0,0.2,0.4,0.6,0.8,1))
quantile(false, c(0,0.2,0.4,0.6,0.8,1))

f1 <- length(false[which(false <= -.85)])
f2 <- length(false[which(false > -.85 & false <= -.75)])
f3 <- length(false[which(false > -.75 & false <= .5)])
f4 <- length(false[which(false > .5 & false <= 1)])
f5 <- length(false[which(false > 1)])

t1 <- length(true[which(true <= -.85)])
t2 <- length(true[which(true > -.85 & true <= -.75)])
t3 <- length(true[which(true > -.75 & true <= .5)])
t4 <- length(true[which(true > .5 & true <= 1)])
t5 <- length(true[which(true > 1)])

true.m <- c(t1,t2,t3,t4,t5)
false.m <- c(f1,f2,f3,f4,f5)

top <- round(matrix(data = c(false.m/sum(false.m), true.m/sum(true.m)), nrow = 2, byrow = TRUE)*100,1)

high <- ceiling(max(top)/10)

# Printing
png(filename = "4_descriptors/4_org_res.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("Very low","Low","Medium","High","Very high"),
        ylab = "Porcentage of observations",
        xlab = "Degree of organizational resources",
        ylim = c(0,high*10),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Organizational resources according to ownership",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:high)*10,
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ===== Printing editorial presence ===== 
load("2_output/comicsFull.RData")


lbl <- c("Editor absent - ", "Editor present - ")

editors1 <- length(which(cdb.std$is.edited == TRUE))
editors0 <- length(which(cdb.std$is.edited == FALSE))
sumx <- sum(editors0, editors1)
proportion <- c(editors0/sumx, editors1/sumx)

editors11 <- length(which(cdb.std$is.edited == TRUE & 
                            cdb.std$is.creator.owned == TRUE))
editors01 <- length(which(cdb.std$is.edited == FALSE & 
                            cdb.std$is.creator.owned == TRUE))
sumx1 <- sum(editors11, editors01)
proportion.co <- c(editors01/sumx1, editors11/sumx1)

editors10 <- length(which(cdb.std$is.edited == TRUE & 
                            cdb.std$is.creator.owned == FALSE))
editors00 <- length(which(cdb.std$is.edited == FALSE & 
                            cdb.std$is.creator.owned == FALSE))
sumx0 <- sum(editors00, editors10)
proportion.po <- c(editors10/sumx0, editors00/sumx0)


data <- matrix(data = c(editors11, editors01, editors10, editors00), 
               nrow = 2, byrow = TRUE)

high <- ceiling(max(apply(data, 2, sum)/nrow(cdb.std))*10)
dos <- nrow(cdb.std)/10
# Printing
png(filename = "4_descriptors/4_editor.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(data, 
        main="Proportion issues with editorial presence",
        ylab="Porcentage of observations",
        names.arg = c("Editor present","Editor absent"),
        col = c("gray30", "gray70"), 
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5,
        ylim = c(0,high*nrow(cdb.std)/10),
        axes = FALSE)
axis(side = 2,
     at = c(0,dos, dos*2, dos*3, dos*4, dos*5, dos*6, dos*7),
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
legend("topright",
       legend = c("Publisher-owned", "Creator-owned"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
dev.off()

# ===== Printing number of contributors by editorial presence ===== 
load(file = "2_output/comicsFull.RData")

false <- cdb$contributors.count[which(cdb$is.edited == FALSE)]
true <- cdb$contributors.count[which(cdb$is.edited == TRUE)]

hist(true)
table(false)

quantile(true, c(0,0.2,0.4,0.6,0.8,1))
quantile(false, c(0,0.2,0.4,0.6,0.8,1))


log10(1003)

f1 <- length(false[which(false <= 2)])
f2 <- length(false[which(false > 2 & false <= 4)])
f3 <- length(false[which(false > 4 & false <= 6)])
f4 <- length(false[which(false > 6 & false <= 8)])
f5 <- length(false[which(false > 8)])

t1 <- length(true[which(true <= 2)])
t2 <- length(true[which(true > 2 & true <= 4)])
t3 <- length(true[which(true > 4 & true <= 6)])
t4 <- length(true[which(true > 6 & true <= 8)])
t5 <- length(true[which(true > 8)])

true.m <- c(t1,t2,t3,t4,t5)
false.m <- c(f1,f2,f3,f4,f5)

top <- round(matrix(data = c(false.m/sum(false.m), true.m/sum(true.m)),
                    nrow = 2, byrow = TRUE)*100,1)

high <- ceiling(max(top)/10)

# Printing
png(filename = "4_descriptors/4_num_con.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("1-2","3-4","5-6","7-8","9+"),
        ylab = "Porcentage of observations",
        xlab = "Number of contributors in the team",
        ylim = c(0,high*10),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Number of contributors by editorial presence",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Editor absent", "Editor present"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:high)*10,
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ===== Printing team composition by editorial presence ===== 
load(file = "2_output/comicsFull.RData")

false <- cdb$tech.crea[which(cdb$is.edited == FALSE)]
true <- cdb$tech.crea[which(cdb$is.edited == TRUE)]

hist(true)
hist(false)

f1 <- length(false[which(false <= -6)])
f2 <- length(false[which(false > -.6 & false <= -.2)])
f3 <- length(false[which(false > -.2 & false <= .2)])
f4 <- length(false[which(false > .2 & false <= .6)])
f5 <- length(false[which(false > .6)])

t1 <- length(true[which(true <= -.6)])
t2 <- length(true[which(true > -.6 & true <= -.2)])
t3 <- length(true[which(true > -.2 & true <= .2)])
t4 <- length(true[which(true > .2 & true <= .6)])
t5 <- length(true[which(true > .6)])

true.m <- c(t1,t2,t3,t4,t5)
false.m <- c(f1,f2,f3,f4,f5)

top <- round(matrix(data = c(false.m/sum(false.m), true.m/sum(true.m)),
                    nrow = 2, byrow = TRUE)*100,1)


high <- 10 #ceiling(max(top)/10)

# Printing
png(filename = "4_descriptors/4_team_pro_editor.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(top, 
        beside = TRUE, 
        names.arg = c("High Tech","Low Tech","Balanced","Low Crea","High Crea"),
        ylab = "Porcentage of observations",
        xlab = "Team composition",
        ylim = c(0,high*10),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Team composition by editorial presence",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
axis(1, labels = FALSE, at = c(1,3,4,6,7,9,10,12,13,15))
legend("topright",
       legend = c("Editor absent", "Editor present"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0:high)*10,
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()

# ==== Printing the book condition =====

data <- table(cdb$is.edited, cdb$is.book)

high <- ceiling(max(data)/nrow(cdb.std)*10)
dos <- nrow(cdb.std)/10

png(filename = "4_descriptors/4_book.png",
    width = 900, height = 600)
par(oma = c(1,1,1,1),
    mar =  c(5.1, 6, 4.1, 2.1))
barplot(data, 
        beside = TRUE,
        ylab = "Porcentage of observations",
        xlab = "Book dichotomous variable",
        ylim = c(0,high*nrow(cdb.std)/10),
        axes = FALSE,
        col = c("gray70", "gray30"), 
        main = "Book issues by editorial presence",
        cex = 2,
        cex.lab = 2,
        cex.axis = 2,
        cex.main = 2.5)
legend("topright",
       legend = c("Editor absent", "Editor present"),
       cex=2, pch=15, pt.cex = 4, 
       col = c("gray70", "gray30"))
axis(side = 2,
     at = c(0,dos, dos*2, dos*3, dos*4, dos*5, dos*6),
     labels = paste((0:high)*10, "%", sep = ""),
     cex.axis = 1.5)
dev.off()