library(dplyr)
library(magrittr)
library(tidyr)
library(poLCA)
library(ggplot2)
library(caret)
library(AUC)
library(rpart)
library(rpart.plot)

setwd("~/Desktop/Summer 2019/Data Mining/group proj/sdoh/data")

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


##classification tree using all variables
ctree_1 <- rpart(formula = final_htn_num_2016_q ~ ., data = dataTrain, 
                 control = rpart.control(cp = 0, minsplit = 30, xval = 10))
ctree_1
printcp(ctree_1)
plotcp(ctree_1)
# par(mai=c(0.1,0.1,0.1,0.1))
# plot(ctree_1,main="Classification Tree",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
# text(ctree_1,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))

rpart.plot(ctree_1, box.palette="RdBu", main = "Classification Tree")

## use cp with lowest xerror
ctree_1_prune <- rpart(formula = final_htn_num_2016_q ~ ., data = dataTrain, 
                       control = rpart.control(cp =0.0089641, minsplit = 30, xval = 10))
rpart.plo
# par(mai=c(0.1,0.1,0.1,0.1))
# plot(ctree_1_prune,main="Classification Tree",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
# text(ctree_1_prune,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))
rpart.plot(ctree_1_prune, box.palette="RdBu", main = "Classification Tree")

##confusion matrix for pruned tress
table(dataTrain[, "final_htn_num_2016_q"] ,predict(ctree_1_prune, type = "class"), dnn = c("Actual Group", "Predicted Group"))


##test data
table(dataTest[,"final_htn_num_2016_q"],predict(ctree_1_prune, type = "class", newdata = dataTest), dnn = c("Actual Group", "Predicted Group"))
