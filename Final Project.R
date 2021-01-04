#research question
#1. reliability (internal consistency; croback alpha)
#2. item analysis
#3. Is the sacle unidimenstional (FA)
library(psych)
library(GPArotation)
# Basic statistical analysis of data at the test level (e.g., distribution of total score

# Descriptive Statistics

dim(data_1)
# [1] observations = 47336; items = 15
attach(data_1)
mean(totalsc)
# Mean = 26.1881
sd(totalsc)
# Standard Deviation = 7.03245
summary(totalsc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   21.00   26.00   26.19   31.00   40.00 
hist(totalsc)

mean(age)
# 26.61554
sd(age)
# 12.3927
hist(age)
table(gender)
#     N     M     F     O 
#   176 17691 28954   515  


# Factor Analysis

cormat <- cor(data_1[,1:10])
pca_full <- principal(r = cormat, nfactors = 10) 
pca_full$loadings
# SS loadings    1.096 1.081 1.057 1.051 1.029 0.983 0.975 0.950 0.904 0.874
# 5 factors are identified
pca_full$communality

fa_init <- psych::fa(r = cormat, nfactors = 5)

fa_init_ml <- fa(r=cormat, nfactors=5, SMC=TRUE, fm="ml")
fa_init_ml$loadings

plot(seq(1,10,1), fa_init_ml$e.values, pch = 16,
     xlab="Number of Factors", ylab="Eigenvalues", main="Scree Plot")
abline(1, 0, lty="dotted")

fa_oblimin <- fa(r = cormat, nfactors = 1, fm = "ml")
fa_oblimin$loadings
# 1 factor

#### CTT Item Analysis ###

# Difficulty

colMeans(datai)
#      Q1       Q2       Q3       Q4       Q5       Q6       Q7       Q8       Q9      Q10 
#3.005387 3.088178 2.680750 2.909815 2.605691 2.555941 2.435483 2.296117 2.200946 2.409794 


apply(datai,2, var )  # 2 indicates columns
# Q1        Q2        Q3        Q4        Q5        Q6        Q7        Q8        Q9       Q10 
#0.7648807 0.6776369 0.9383226 0.6838492 0.9771042 0.8721637 0.8956790 0.9303544 0.9994845 1.1703564 

apply(datai,2, sd )  # 2 indicates columns
#       Q1        Q2        Q3        Q4        Q5        Q6        Q7        Q8        Q9       Q10 
# 0.8745746 0.8231871 0.9686705 0.8269517 0.9884858 0.9338971 0.9464032 0.9645488 0.9997422 1.0818301 

# Item difficulty indices and the average difficulty
diff <- colMeans(datai)
mean(diff)
# 2.61881

# Identify the easies and hardest item
sort(diff)
# easies: Q9 = 2.200946;  hardest: Q2 = 3.088178 

result_sort_diff <- sort.int(diff, index.return = TRUE, decreasing=TRUE)
result_sort_diff$x # p-levels of items in descending order
result_sort_diff$ix # item index whose p-levels are in descending order

## barplot
barplot(diff, main="Difficulty")

###  CTT Item Analysis - Item Discrimisniation

dataif <- as.data.frame(datai)
# data input must be data.frame; if not transform using "as.data.frame"
result_ctt <- CTT::itemAnalysis(dataif, itemReport=TRUE)
str(result_ctt)
# nItem: The number of items
# nPerson: The sample size used in calculating the values
# alpha: Cronbach's alpha
# scaleMean: Average of total score
# scaleSD: Standard deviation of total sum score
# alphaIfDeleted: Cronbach's alpha if the corresponding item were deleted
# pBis: The item total correlation, with the item's contribution removed from the total (CORRECTED) (for true dichotomy)
# bis: The item total biserial correlation, with the item's contribution removed
# from the total (CORRECTED)
# itemMean: Average of each item


bis <- result_ctt$itemReport$bis # item biserial correlations
mean(bis)
tmp <- sort.int(bis, index.return = TRUE)
tmp$x
tmp$ix

# mean bis = 0.7268325
# 0.5784572 0.6268226 0.7041272 0.7324152 0.7350403 0.7609913 0.7697587 0.7729052 0.7920146 0.7957924
# 8  4  2  5  9  1  7  3 10  6
# 0.578 - 0.796
barplot(bis)
names(bis) <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5","Item 6", "Item 7", "Item 8", "Item 9", "Item 10")
barplot(bis, main="Biserial")


ItemStats <- cbind(diff,bis)
colnames(ItemStats)<-c("Difficulty", "Corrected rbis")
round(ItemStats, 2)
write.csv(ItemStats, file="CTT_ItemAnalysis.csv")

###reliablility - Internal Consistency Measures - Cronbach's Alpha ###

rel_alpha <- psychometric::alpha(datai)
# 0.9109343

### Confidence interval for alpha
psychometric::alpha.CI(alpha = rel_alpha, k = 10, N = 47336, level = 0.95)

#       LCL     ALPHA       UCL
# 1 0.9097961 0.9109343 0.9120655

# 'alpha': coefficient alpha to use for CI construction
# 'k': number if items
# 'N': sample size
# 'level': Significance Level for constructing the CI, default is .90
