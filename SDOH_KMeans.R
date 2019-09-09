library(dplyr)
library(tidyr)
library(magrittr)

SDOH_Chicago <- read.csv(file="C:/Users/Megha/Documents/MScA/Summer 2019/Data Mining Principles/Final Project/sdoh_final_data.csv", header=TRUE, sep=",")
names(SDOH_Chicago)
str(SDOH_Chicago)

##Any row that has >= 20% of its data missing, will be removed from the dataset. Any row that has <20% of its data missing, will be used.
##Column means will be imputed for missing data
#is.na(SDOHChi16$SDOH_Chicago.vcrime_r16)

SDOH_Chicago <- SDOH_Chicago %>%
  filter(!e_totpop == "0")

vcrime_r16.imp.mean <- ifelse(is.na(SDOH_Chicago$vcrime_r16), mean(SDOH_Chicago$vcrime_r16, na.rm=TRUE), SDOH_Chicago$vcrime_r16)
vcrime_r16.imp.mean

SDOHChi16 <- data.frame (SDOH_Chicago$EP_POV, SDOH_Chicago$EP_UNEMP, SDOH_Chicago$EP_PCI, SDOH_Chicago$EP_NOHSDP, SDOH_Chicago$EP_AGE65,
                       SDOH_Chicago$EP_AGE17, SDOH_Chicago$EP_DISABL, SDOH_Chicago$EP_SNGPNT, SDOH_Chicago$EP_MINRTY, SDOH_Chicago$EP_LIMENG,
                       SDOH_Chicago$EP_MUNIT, SDOH_Chicago$EP_MOBILE, SDOH_Chicago$EP_CROWD, SDOH_Chicago$EP_NOVEH, SDOH_Chicago$EP_GROUPQ,
                       vcrime_r16.imp.mean)
names(SDOHChi16) <- c("EP_POV", "EP_UNEMP", "EP_PCI", "EP_NOHSDP", "EP_AGE65", "EP_AGE17", "EP_DISABL", "EP_SNGPNT", "EP_MINRTY",
                      "EP_LIMENG", "EP_MUNIT", "EP_MOBILE", "EP_CROWD", "EP_NOVEH", "EP_GROUPQ", "vcrime_r16.imp.mean")

SDOHChi16 <- SDOHChi16 %>%
  filter(!EP_POV == "-999") 
#Removed 3 rows (NA), removed 1 row (-999), imputed mean for 1 row

#Row 549 has -999 data for EP variables
# subset(SDOHChi16, (EP_PCI == '-999'))
# 
# SDOHChi16$EP_POV[SDOHChi16$EP_POV == "-999"] <- (mean(SDOHChi16$EP_POV, na.rm=TRUE))
# SDOHChi16$EP_UNEMP[SDOHChi16$EP_UNEMP == "-999"] <- (mean(SDOHChi16$EP_UNEMP, na.rm=TRUE))
# SDOHChi16$EP_PCI[SDOHChi16$EP_PCI == "-999"] <- (mean(SDOHChi16$EP_PCI, na.rm=TRUE))
# SDOHChi16$EP_NOHSDP[SDOHChi16$EP_NOHSDP == "-999"] <- (mean(SDOHChi16$EP_NOHSDP, na.rm=TRUE))
# SDOHChi16$EP_AGE65[SDOHChi16$EP_AGE65 == "-999"] <- (mean(SDOHChi16$EP_AGE65, na.rm=TRUE))
# SDOHChi16$EP_AGE17[SDOHChi16$EP_AGE17 == "-999"] <- (mean(SDOHChi16$EP_AGE17, na.rm=TRUE))
# SDOHChi16$EP_DISABL[SDOHChi16$EP_DISABL == "-999"] <- (mean(SDOHChi16$EP_DISABL, na.rm=TRUE))
# SDOHChi16$EP_MOBILE[SDOHChi16$EP_MOBILE == "-999"] <- (mean(SDOHChi16$EP_MOBILE, na.rm=TRUE))
# SDOHChi16$EP_NOVEH[SDOHChi16$EP_NOVEH == "-999"] <- (mean(SDOHChi16$EP_NOVEH, na.rm=TRUE))
# str(SDOHChi16)

smp_size <- floor(0.70 * nrow(SDOHChi16))

set.seed(123)
train_ind <- sample(seq_len(nrow(SDOHChi16)), size = smp_size)

train <- SDOHChi16[train_ind, ]
test <- SDOHChi16[-train_ind, ]




VAF <- numeric()
for(i in 2:10){
  temp = kmeans(scale(train),centers=i,nstart=50)
  VAF[i] = temp$betweenss/temp$totss
}
VAF[2:10]


#Scree Plot
plot(2:10,VAF[2:10], type="b",
     xlab = "Number of Clusters",
     ylab = "VAF")


train.4clust <- kmeans(scale(train), centers=4, nstart=50)
train.4clust

train.3clust <- kmeans(scale(train), centers=3, nstart=50)
train.3clust

train.2clust <- kmeans(scale(train), centers=2, nstart=50)
train.2clust

train.5clust <- kmeans(scale(train), centers=5, nstart=50)
train.5clust



##FOUR CLUSTERS

#Poverty Rate vs. Unemployment Rate
plot(train[,1],train[,2],xlab="POV", ylab="UNEMP", main="Poverty Rate vs. Unemployment Rate (4 Clusters)", type="n")
text(train[,1],train[,2], col=train.4clust$cluster+1)

#Poverty Rate vs. Crime Rate
plot(train[,1],train[,16],xlab="POV", ylab="Crime Rate", main="Poverty Rate vs. Crime Rate (4 Clusters)", type="n")
text(train[,1],train[,16], col=train.4clust$cluster+1)

#Poverty Rate vs Minority Rate
plot(train[,1],train[,9],xlab="POV", ylab="Minority Rate", main="Poverty Rate vs. Minority Rate (4 Clusters)", type="n")
text(train[,1],train[,9], col=train.4clust$cluster+1)

#PCI vs Crime Rate
plot(train[,3],train[,16],xlab="PCI", ylab="Crime Rate", main="Per Capita Income vs. Crime Rate (4 Clusters)", type="n")
text(train[,3],train[,16], col=train.4clust$cluster+1)



##THREE CLUSTERS
#Poverty Rate vs. Unemployment Rate
plot(train[,1],train[,2],xlab="POV", ylab="UNEMP", main="Poverty Rate vs. Unemployment Rate (3 Clusters)", type="n")
text(train[,1],train[,2], col=train.3clust$cluster+1)

#Poverty Rate vs. Crime Rate
plot(train[,1],train[,16],xlab="POV", ylab="Crime Rate", main="Poverty Rate vs. Crime Rate (3 Clusters)", type="n")
text(train[,1],train[,16], col=train.3clust$cluster+1)

#Poverty Rate vs Minority Rate
plot(train[,1],train[,9],xlab="POV", ylab="Minority Rate", main="Poverty Rate vs. Minority Rate (3 Clusters)", type="n")
text(train[,1],train[,9], col=train.3clust$cluster+1)

#PCI vs Crime Rate
plot(train[,3],train[,16],xlab="PCI", ylab="Crime Rate", main="Per Capita Income vs. Crime Rate (3 Clusters)", type="n")
text(train[,3],train[,16], col=train.3clust$cluster+1)


##TWO CLUSTERS
#Poverty Rate vs. Unemployment Rate
plot(train[,1],train[,2],xlab="POV", ylab="UNEMP", main="Poverty Rate vs. Unemployment Rate (2 Clusters)", type="n")
text(train[,1],train[,2], col=train.2clust$cluster+1)

#Poverty Rate vs. Crime Rate
plot(train[,1],train[,16],xlab="POV", ylab="Crime Rate", main="Poverty Rate vs. Crime Rate (2 Clusters)", type="n")
text(train[,1],train[,16], col=train.2clust$cluster+1)



#####TEST DATA
train_centers <- train.4clust$centers
test.4clust=kmeans(scale(test),centers=train_centers,nstart=1)
test.4clust$centers #cluster centroids
test.4clust$size #cluster sizes
test.4clust$betweenss/test.4clust$totss*100 #VAF

train.4clust$centers
train.4clust$size
train.4clust$betweenss/train.4clust$totss*100 #VAF

Full.4cluster =kmeans(scale(SDOHChi16), centers=train_centers, nstart=1)
Full.4cluster

SDOH_Chicago <- SDOH_Chicago %>%
  filter(!EP_POV == "-999")

SDOHChi16_Clusters <- data.frame(SDOH_Chicago$geoid, Full.4cluster$cluster)
names(SDOHChi16_Clusters) <- c("Census_Tract", "Cluster")                                 

write.csv(SDOHChi16_Clusters,"C:/Users/Megha/Documents/MScA/Summer 2019/Data Mining Principles/Final Project/SDOHChi2016_Clusters.csv", row.names = FALSE)
