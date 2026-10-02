# PSC 103A: Statistical Analysis of Psychological Data
# Course Paper
# Prutha Patel

# Loading Data

datafr1 <- load('/ICPSR_36346/DS0001/36346-0001-Data.rda')
datafr2 <- load ('/ICPSR_38862/DS0001/38862-0001-Data.rda')
frame1 <- da38862.0001[,]
frame2 <- da36346.0001[,]
psc103Adata <- merge(frame1, frame2, by = "M2ID", all = FALSE)

# continuous Variable C5SDPL - Love/Attachment
# binary Variable C1PRSEX.x - Sex

# creating subset male and female groups
print(paste0("'", levels(psc103Adata$C1PRSEX.x), "'"))
psc103Adata_males = psc103Adata[psc103Adata$C1PRSEX.x %in% c("(1) MALE"), ]  # males
psc103Adata_females = psc103Adata[psc103Adata$C1PRSEX.x %in% c("(2) FEMALE"), ]  # females

# sample size for male and female groups
length(psc103Adata_females$C5SDPL)
length(psc103Adata_males$C5SDPL)

# histogram
par(mfrow=c(1,2)) # 1 row x 2 column display
hist(psc103Adata_males$C5SDPL,
     main ="Males",
     freq = F,   
     col="lightblue",
     xlim=c(3,7),
     xlab = "Love/Attachment")

hist(psc103Adata_females$C5SDPL,
     main ="Females",
     freq = F, 
     col="blue",
     xlim=c(3,7),
     xlab = "Love/Attachment")

# normality test for females
# p-value = 0.08609 > 0.05
shapiro.test(psc103Adata_females$C5SDPL)

# normality test for males
# p-value = 0.05187 > 0.05
shapiro.test(psc103Adata_males$C5SDPL)

# normal probability plot 
# females
qqnorm(psc103Adata_females$C5SDPL)
# males
qqnorm(psc103Adata_males$C5SDPL)

# two sample independent t-test function
t.test(x = psc103Adata_females$C5SDPL,
       y = psc103Adata_males$C5SDPL,
       alternative = c("greater"),
       mu = 0,
       paired = F,
       var.equal = F,
       conf.level = 0.95)

aggregate(C5SDPL~C1PRSEX.x, data=psc103Adata, var)