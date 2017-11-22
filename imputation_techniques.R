setwd("E:/INSOFE/Internship/Sensorless_Drive_Diagnosis_Data_Set")
data <- read.csv("org_data.csv", header = T)
org_data <- read.table("Sensorless_drive_diagnosis.txt")
View(org_data)
# finding missing values

sum(is.na(data))

null_values <- data[,-which(colnames(is.na(data))>0)]
na_count <-sapply(data, function(y) sum(length(which(is.na(y)>0))))
na_count <- data.frame(na_count)
names(data)
View(na_count)
dim(na_count)
rows <- which(rowSums(na_count)>0)


missing_data <- subset(data,select = c(4,11,14,23))

#write.csv(missing_data,"miss_orginal.csv",row.names = F)

# imputation of missing values

#using central imputation

library(DMwR)

data_central <- centralImputation(data)
View(data_central)
central_values <- subset(data_central,select = c(4,11,14,23))
View(central_values)
#write.csv(central_values,"central_imputation.csv",row.names = F)
boxplot(data_central$V4)
sum(is.na(central_values))

#imputing using KNN imputation

dat_knn <- knnImputation(data,k=5)
sum(is.na(dat_knn))
knn_values <- subset(dat_knn,select = c(4,11,14,23))

write.csv(knn_values,"knn_imputation.csv",row.names = T)

library(ggplot2)
par(mfrow=c(2,3))
boxplot(org_data$V4,main = "boxplot for orginal V4")
boxplot(data_central$V4,main = "boxplot for central imputation of V4")
boxplot(dat_knn$V4,main = "boxplot for knn imputation of V4")
plot(org_data$V4,main = "distribution of orgianal v4")
plot(data_central$V4,main = "distribution of central imputation of V4")
plot(dat_knn$V4,main = "distribution of knn imputed of V4")

#comparision for V11

par(mfrow=c(2,3))
boxplot(org_data$V11,main = "boxplot for orginal ")
boxplot(data_central$V11,main = "boxplot for central imputation")
boxplot(dat_knn$V11,main = "boxplot for knn imputation")
plot(org_data$V11,main = "distribution of orgianal")
plot(data_central$V11,main = "distribution of central imputation")
plot(dat_knn$V11,main = "distribution of knn imputed")

#comparision for v14

par(mfrow=c(2,3))
boxplot(org_data$V14,main = "boxplot for orginal ")
boxplot(data_central$V14,main = "boxplot for central imputation")
boxplot(dat_knn$V14,main = "boxplot for knn imputation")
plot(org_data$V14,main = "distribution of orgianal")
plot(data_central$V14,main = "distribution of central imputation")
plot(dat_knn$V14,main = "distribution of knn imputed")

#comparision for V23

par(mfrow=c(2,3))
boxplot(org_data$V23,main = "boxplot for orginal ")
boxplot(data_central$V23,main = "boxplot for central imputation")
boxplot(dat_knn$V23,main = "boxplot for knn imputation")
plot(org_data$V23,main = "distribution of orgianal")
plot(data_central$V23,main = "distribution of central imputation")
plot(dat_knn$V23,main = "distribution of knn imputed")



heatmap()

# For variables with larger values, Knn imputation given good approximations
# for variables with smaller values, both imputations are not giving better results.




#**********************************
#merging the data
#**********************************

d1 = data.frame(id = 1:3, exp =seq(10,30,10))
d2 = data.frame(id = c(1,2,4,5,6),age =seq(10,50,10))
d1
d2
dim(d2)
  
#inner join (A intersection B)

merge(d1,d2, by.x = "id",by.y ="id" )

#outerjoin (A union B)

merge(d1,d2,by.x="id",by.y = "id",all = T)

#left join(A-B)

merge(d1,d2,by.x = "id",by.y = "id",all.x = T)

#right join(B-A)
merge(d1,d2,by.x = "id",by.y = "id",all.y = T)

#write.csv(dat_knn,"data_for_merging.csv",row.names = F)

merge_data <- read.csv("data_for_merging.csv",header = T)

data1 <- subset(merge_data,select = c(1:25,49))
data2 <- subset(merge_data,select = c(26:49))
dim(data1)
dim(data2)

new_merge_data <- merge(data1,data2,by.x = "V49",by.y = "V49")
View(new_merge_data)
gc()

cor(org_data)

library(corrplot)
corrplot(cor(org_data),main = "correlation of the data")
var(org_data$V1)

max(org_data$V1) - min(org_data$V1)
sd(org_data$V1)
