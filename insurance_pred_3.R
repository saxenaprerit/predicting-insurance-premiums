rm(list = ls())

setwd("C:/Users/ps11337/Desktop/Algorhythms")

library(rJava)
library(xlsxjars)
library(xlsx)
library(caTools)
library(NLP)
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(Rgraphviz) # Correlation plots
library(RColorBrewer) #for wordcloud
library(wordcloud) # routines to create word cloud
library(rpart)  # cart models
library(RWeka)  # R wrappers for weka tools
library(ROCR)   # model performance routines
library(pROC)   # model performance routines

setwd("C:/Users/ps11337/Desktop/datatest/Insurance")

Train=read.csv("Training Data - Premium Prediction.csv", stringsAsFactors = FALSE)
Test = read.csv("Test Data - Premium Prediction.csv", stringsAsFactors = FALSE)
str(Train)
summary(Train)

set.seed(123)
spl = sample.split(Train, 0.7)
Train1=subset(Train, spl==TRUE)
Train2=subset(Train, spl==FALSE)
str(Train1)
str(Train2)

Traincorr1=data.frame(Train$Var_13,
                      Train$Var_21,
                      Train$Var_33, Train$Var_35, Train$Var_39,
                      Train$Var_45)
str(Traincorr1)
# Trainmat<-cor(Traincorr)
# View(Trainmat)

Traincorr2=data.frame(Train$Var_15, 
                      Train$Var_27,
                      Train$Var_42, Train$Var_43, Train$Var_44)
str(Traincorr2)

# clustering

# Subset 1
# Traincorr1=unique(Traincorr1)
str(Traincorr1)

# Run k-means
k=10

set.seed(1)
KMC1 = kmeans(Traincorr1, centers = k, iter.max = 1000)
str(KMC1)

# Extract clusters
PremiumClusters1 = KMC1$cluster
KMC1$centers[2]

# Subset 2
# Traincorr2=unique(Traincorr2)
str(Traincorr2)

k=10

set.seed(1)
KMC2 = kmeans(Traincorr2, centers = k, iter.max = 1000)
str(KMC2)

# Extract clusters
PremiumClusters2 = KMC2$cluster
KMC2$centers[2]


# Standardizing
summary(Train1)
str(Train1)
omit <- c("Var_13", "Var_15", "Var_21", "Var_27", "Var_33", "Var_35", "Var_39", "Var_42",
          "Var_43", "Var_44", "Var_45")
common.vars <- colnames(Train1)%in%omit
data3 <- Train1[,which(common.vars=="TRUE")]
str(data3)

#YRSTEACH and YRSUT highly correlated
symnum(cor(data3,use="complete.obs"))
plot(data3)  # will always plot correlation of various elements

fit <- princomp(data3, cor=TRUE)

summary(fit) # print variance accounted for 
loadings(fit) # pc loadings, tells each component is combined of which columns
fit$scores # the principal components, look at cumulative proportion values

plot(fit,type="lines") # scree plot 

fit$scores[1:5,1:5] # just to see scores

predict(fit)
datacomp <- as.data.frame(predict(fit)[,1:9]) # creating data frame of components

str(datacomp)
summary(datacomp)

datacomp$Premium <- data3$Premium

set.seed(123)
spl = sample.split(datacomp, 0.7)
Train = subset(datacomp, spl == TRUE)
Test = subset(datacomp, spl == FALSE)

predictsal = lm(Premium~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9, data = datacomp)

summary(predictsal)

# done till here 17 dec

predtest=predict(predictsal, newdata = Test)
predtest

Test$Premium = predtest

Submission = data.frame(ID=integer(),Premium=double())
str(Submission)

Submission = rbind(Submission, data.frame(ID = Test$ID, Premium = Test$Premium))
str(Submission)




# Trainx <- as.data.frame(c(Trainx, Train1$Var_13, Train1$Var_15, Train1$Var_21))

library(vegan)
datastd<- decostand(Trainx,"range")
summary(Train1)

predPrem = lm(Premium ~Var_13+Var_15+Var_21+
                Var_27+Var_33+Var_35+
                Var_39+Var_42+Var_43+Var_44+Var_45, data = data3)
summary(predPrem)

predtest=predict(predPrem, newdata = Train2)
predtest

SSE = sum((Train2$Premium - predtest)^2)
SST = sum((Train2$Premium - mean(Train1$Premium))^2)
1 - SSE/SST

Train$Cluster1=PremiumClusters1
Train$Cluster2=PremiumClusters2

# For Test dataset

Testcorr1=data.frame(Test$Var_13,
                     Test$Var_21,
                     Test$Var_33, Test$Var_35, Test$Var_39,
                     Test$Var_45)
str(Testcorr1)
# Testmat<-cor(Testcorr)
# View(Testmat)

Testcorr2=data.frame(Test$Var_15, 
                     Test$Var_27,
                     Test$Var_42, Test$Var_43, Test$Var_44)
str(Testcorr2)

# clustering

# Subset 1
# Testcorr1=unique(Testcorr1)
str(Testcorr1)

# Run k-means
k=10

set.seed(1)
KMC1 = kmeans(Testcorr1, centers = k, iter.max = 1000)
str(KMC1)

# Extract clusters
PremiumTClusters1 = KMC1$cluster
KMC1$centers[2]

# Subset 2
# Testcorr2=unique(Testcorr2)
str(Testcorr2)

k=10

set.seed(1)
KMC2 = kmeans(Testcorr2, centers = k, iter.max = 1000)
str(KMC2)

# Extract clusters
PremiumTClusters2 = KMC2$cluster
KMC2$centers[2]

# Adding cluster values to Test
Test$Cluster1=PremiumTClusters1
Test$Cluster2=PremiumTClusters2

#Predicting

predPrem = lm(Premium ~ Var_13+Var_15+Var_21+
                Var_27+Var_33+Var_35+
                Var_39+Var_42+Var_43+Var_45+Cluster1+Cluster2, data = Train)
summary(predPrem)

predtest=predict(predPrem, newdata = Test)
predtest

Test$Premium = predtest

Submission = data.frame(ID=integer(),Premium=double())
str(Submission)

Submission = rbind(Submission, data.frame(ID = Test$ID, Premium = Test$Premium))
str(Submission)

# Var_36 removed - Highly correlated to Var_10
# Var_23 removed - Highly correlated to Var_41
# Var_41 removed - highly correlated to var_17
# Var_2 removed - highly correlated to Var_45
# Var_28 removed - highly correlated to Var_44
# Var_24 removed - highly correlated to 45
# Var_10 removed - improved model performance
# Var_26/Var_38 removed - improved model performance
# Var_44 removed - improved performance
# Var_19/25 removed - improved performance
# VaR_44 removed - improved performance

write.csv(Submission, "My_Submission_11.csv",row.names=FALSE)

#SSE = sum((Test$Premium - predtest)^2)
#SST = sum((Test$Premium - mean(Train$Premium))^2)
#1 - SSE/SST