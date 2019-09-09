library(caret)
source('clustreg.R')
source('clusterRegPredict.R')

SDOH_Chicago <- read.csv("sdoh_final_data.csv")
SDOH_Chicago <- SDOH_Chicago %>%
  filter(!e_totpop == "0") %>%
  filter(!EP_POV == "-999")

SDOH_Chicago$vcrime_r16.imp.mean <- ifelse(is.na(SDOH_Chicago$vcrime_r16), mean(SDOH_Chicago$vcrime_r16, na.rm=TRUE), SDOH_Chicago$vcrime_r16)

quantile(SDOH_Chicago$final_htn_num_2016_r, na.rm = TRUE)

SDOH_Chicago$final_htn_num_2016_q <- ifelse(SDOH_Chicago$final_htn_num_2016_r < 0.35,0,ifelse(SDOH_Chicago$final_htn_num_2016_r >=0.35,1,NA))

###2016 data
SDOH_Chicago_2016 <- SDOH_Chicago[,c(15:29,84:85)]
SDOH_Chicago_2016$final_htn_num_2016_q <- as.factor(SDOH_Chicago_2016$final_htn_num_2016_q)
SDOH_Chicago_2016 <- na.omit(SDOH_Chicago_2016)


##generate train and test
set.seed(2345)
trainIndex <- sample(1:nrow(SDOH_Chicago_2016), size = 0.7 * nrow(SDOH_Chicago_2016))
dataTrain <- SDOH_Chicago_2016[trainIndex,]
dataTest<- SDOH_Chicago_2016[-trainIndex,]


### cluster wise regression
dataTrain_clust1 <- clustreg(dataTrain, 1, 1, 581, 1)
dataTrain_clust2 <- clustreg(dataTrain, 2, 2, 581, 10)
dataTrain_clust3 <- clustreg(dataTrain, 3, 2, 581, 10)
dataTrain_clust4 <- clustreg(dataTrain, 4, 2, 581, 10)

##best r square
rsq_clust1 <- dataTrain_clust1$rsq.best
rsq_clust2 <- dataTrain_clust2$rsq.best
rsq_clust3 <- dataTrain_clust3$rsq.best
rsq_clust4 <- dataTrain_clust4$rsq.best

rsq_train <- c(rsq_clust1, rsq_clust2, rsq_clust3, rsq_clust4)
plot(1:4, rsq_train, main = "Scree Plot for Cluster-Wise Regression",
     xlab = "Number of Clusters", ylab = "R Squared", type = "b", col = "11")


##test dtaa
dataTest_clust1 <- clustreg.predict(dataTrain_clust1, dataTest)
dataTest_clust2 <- clustreg.predict(dataTrain_clust2, dataTest)
dataTest_clust3 <- clustreg.predict(dataTrain_clust3, dataTest)
dataTest_clust4 <- clustreg.predict(dataTrain_clust4, dataTest)

##diff in r square
rsq_test <- c(dataTest_clust1$rsq, dataTest_clust2$rsq, dataTest_clust3$rsq, dataTest_clust4$rsq)

rsq <- cbind.data.frame(rsq_train, rsq_test)
rsq$percent_diff <- (rsq$rsq_train - rsq$rsq_test) / rsq$rsq_train
rsq
