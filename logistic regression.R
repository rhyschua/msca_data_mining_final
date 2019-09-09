library(dplyr)
library(magrittr)
library(tidyr)
library(poLCA)
library(ggplot2)
library(caret)
library(AUC)
library(gains)

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


##step wise algorithm then log reg
logreg_train <- glm(final_htn_num_2016_q ~ ., data = dataTrain, family = binomial(link = logit))
summary(logreg_train)
step(logreg_train, direction = "both")


##
logreg_best_train <- glm(formula =final_htn_num_2016_q ~ EP_AGE65 + EP_MINRTY + vcrime_r16.imp.mean, family = binomial(link = logit), data = dataTrain)

summary(logreg_best_train)


##confusion matrix using lowest AIC model
best_train_model <- logreg_best_train$fitted.values
best_train_model[best_train_model>=0.5]=1
best_train_model[best_train_model<0.5]=0
table(dataTrain$final_htn_num_2016_q,best_train_model, dnn = c("Actual Group", "Predicted Group"))


###test model and confusion matrix
logreg_best_test <- predict(logreg_best_train, newdata = dataTest, type = "response")
logreg_best_test[logreg_best_test>=0.5]=1
logreg_best_test[logreg_best_test<0.5]=0
table(dataTest$final_htn_num_2016_q,logreg_best_test, dnn = c("Actual Group", "Predicted Group"))


##gains
# gains(as.numeric(dataTest$final_htn_num_2016_q)-1,logreg_best_test,10)
# 
# plot(gains(as.numeric(dataTest$final_htn_num_2016_q)-1,logreg_best_test,10))


##ROC curve
test_glm <- glm(formula =final_htn_num_2016_q ~ EP_AGE65 + EP_MINRTY + vcrime_r16.imp.mean, family = binomial(link = logit), data = dataTest)
summary(test_glm)
#roc(logreg_best_test, dataTest$final_htn_num_2016_q)
plot(roc(test_glm$fitted.values, dataTest$final_htn_num_2016_q))
text(x=0.72, y=0.6, labels = paste("AUC= 0.669"))
