setwd("C:/Users/Krishna/Downloads")
getwd()
bankdata=read.csv("C:/Users/krishna/Downloads/sampledatatest.csv",header=T)
library("dplyr")
library("tidyr")
library("tibble")
summary(bankdata)
class(bankdata$income)
unique(bankdata$income)
class(bankdata$educational_status)
unique(bankdata$educational_status)
length(bankdata$educational_status)
class(bankdata$dependents)
bankdata$transaction_location=as.factor(bankdata$transaction_location)
class(bankdata$transaction_location)
sum(is.na(bankdata))
sum(is.null(bankdata))
summary(bankdata$transaction_amount)
Var= bankdata$transaction_amount
Var.freq = sort(table(Var), decreasing = T)
library("ggplot2")

plot(bankdata$transaction_amount)
boxplot(bankdata$transaction_amount)
capabilities()
Sys.getenv("DISPLAY")
dev.capabilities()
dev.capture()
dev.off()
library("outliers")
df=bankdata$transaction_amount
boxplot(df,data=df)
outlier(bankdata$transaction_amount,opposite=FALSE)
bankdata$transaction_amount=cut(bankdata$transaction_amount,breaks=10,right=FALSE,labels=c("A","B","C","D","E","F","G","H","I","J"))
catincome=as.data.frame(catincome)
bankdata=cbind(bankdata,catincome)
unique(bankdata$transaction_amount)
length(unique(bankdata$transaction_amount))
hist(bankdata$transaction_amount)
bankdata$transaction_amount=log10(bankdata$transaction_amount)
hist(bankdata$transaction_amount)
boxplot(bankdata$transaction_amount)
boxplot(bankdata)
boxplot(bankdata$dependents)
hist(bankdata$transaction_amount)
ggplot(data=bankdata$customer_type,mapping=aes(x = Level)) + geom_bar()
bankdata$dependents=log(bankdata$dependents)
hist(bankdata$dependents)
ggplot(bankdata, aes(income, fill = transaction_amount)) + geom_bar()
ggplot(bankdata, aes(income, fill = customer_type)) + geom_bar()
ggplot(bankdata, aes(dependents)) + geom_bar()
ggplot(bankdata, aes(channel_id)) + geom_bar()
ggplot(bankdata, aes(transaction_location)) + geom_bar()
levels(bankdata$channel_id)[levels(bankdata$channel_id) == "ATM"] =0
levels(bankdata$channel_id)[levels(bankdata$channel_id) == "ATM1"] = 1
levels(bankdata$channel_id)[levels(bankdata$channel_id) == "ATM2"] = 2
levels(bankdata$channel_id)[levels(bankdata$channel_id) == "Teller"] = 3
levels(bankdata$service_category)[levels(bankdata$service_category) == "CASH DEPOSIT"] =1
levels(bankdata$service_category)[levels(bankdata$service_category) == "CASH WITHDRAWAL"] =2
levels(bankdata$service_category)[levels(bankdata$service_category) == "REMITTANCE"] =3
levels(bankdata$customer_type)[levels(bankdata$customer_type) == "B"] =1
levels(bankdata$customer_type)[levels(bankdata$customer_type) == "I"] =2
levels(bankdata$customer_type)[levels(bankdata$customer_type) == "C"] =3
levels(bankdata$sex)[levels(bankdata$sex) == "M"] =1
levels(bankdata$sex)[levels(bankdata$sex) == "F"] =2
levels(bankdata$sex)[levels(bankdata$sex) == "NULL"] =-999
levels(bankdata$sex)[levels(bankdata$sex) == "NULL"] =-999
levels(bankdata$income)[levels(bankdata$income) == "BUSINESS"] =1
levels(bankdata$income)[levels(bankdata$income) == "INVESTMENT"] =2
levels(bankdata$income)[levels(bankdata$income) == "RENTAL"] =3
levels(bankdata$income)[levels(bankdata$income) == "SALARY"] =4
levels(bankdata$income)[levels(bankdata$income) == "NULL"] =5
bankdata$transaction_amount=log10(bankdata$transaction_amount)
df1=as.data.frame(bankdata$transaction_location,header=F)
hist(df1$`bankdata$transaction_location`)
df1$`bankdata$transaction_location`=as.numeric(df1$`bankdata$transaction_location`)
df1$`bankdata$transaction_location`=log(df1$`bankdata$transaction_location`)
bankdata$transaction_location=log(bankdata$transaction_location)
newdata=subset(bankdata,select=c("channel_id","transaction_amount","customer_type","service_category","transaction_location","income","dependents"))
new_3=newdata[,-6]
Finalcut=scale(new_3)
for(i in c(1:7)){newdata[,i]= as.numeric(newdata[,i])}
str(newdata)
newdata_1=scale(newdata)
library("NbClust")
set.seed(1234)
nc=NbClust(newdata_1,min.nc = 2,max.nc=10,method = "kmeans")
plot(nc)
library("factoextra")
fviz_nbclust(nc)
boxplot(newdata_1[5])
hist(newdata_1)
fviz_cluster(km,newdata_1,  geom = "point", ellipse= FALSE, show.clust.cent = FALSE,palette = "jco", ggtheme = theme_classic())
km=kmeans(Finalcut,3,nstart=25)
hist(newdata$income)
new2=newdata_1[,-6]

nc=NbClust(Finalcut,min.nc = 2,max.nc=10,method = "kmeans")
summary(nc)
fviz_cluster(km,Finalcut,  geom = "point", ellipse= FALSE, show.clust.cent = FALSE,palette = "jco", ggtheme = theme_classic())
km1=kmeans(new2,3,nstart=25)
print(km1)
library("fpc")
library("dbscan")
dbscan::kNNdistplot(newdata_1, k = 8 )
summary(dbsc)
# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = new2, stand = FALSE,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
dbscan::kNNdistplot(Finalcut,k = 7 )
abline(h = 1.4, lty = 2)
print(db)
library("fpc")
set.seed(123)
db =fpc::dbscan(Finalcut, eps = 1.4, MinPts = 7,method = c("hybrid", "raw","dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)
abline(h = 1.4, lty = 2)
tgt=predict(newdata,db)
tgt=predict.dbscan(db,newdata )
saveRDS(db)
# Plot DBSCAN results

library("factoextra")
fviz_cluster(db, data = Finalcut, stand = T,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
dbscan::kNNdistplot(newdata_1,k = 8 )
abline(h = 1.8, lty = 2)
db1 =fpc::dbscan(newdata_1, eps = 1.8, MinPts = 8)
fviz_cluster(tgt, data = newdata, stand = FALSE,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
summary(db1)
print(db)
summary(db$cluster)
cluster.stats()
cluster.stats(db$cluster,d = NULL)
class(db$cluster)
segments=as.data.frame(db$cluster)
stats = cluster.stats(dist(Finalcut),db$cluster)
stats_km=cluster.stats(dist(Finalcut),km$cluster)
stats_km
summary(db$cluster)
table(db$cluster)
plot(db)
Finalcut
print(db)
category_bankdata=cbind(segments,bankdata)
ggplot(category_bankdata, aes(, fill =db$cluster)) + geom_bar()
boxplot(new_3$transaction_amount)
boxplot(db$cluster)
hist(db$cluster)
hist(bankdata$transaction_amount)
chisq.test(db$cluster,bankdata$transaction_amount)
library("clusterSim")
cluster.Description(Finalcut, db$cluster, sdType="sample",precission=4,modeAggregationChar=";")
cluster.Description(Finalcut, km$cluster, sdType="sample",precission=4,modeAggregationChar=";")

