setwd("E:/INSOFE/Internship/Sensorless_Drive_Diagnosis_Data_Set")
data <- read.csv("org_data.csv", header = T)

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

write.csv(missing_data,"miss_orginal.csv",row.names = F)

# imputation of missing values

#using central imputation

library(DMwR)

data_central <- centralImputation(data)
View(data_central)
central_values <- subset(data_central,select = c(4,11,14,23))
View(central_values)
write.csv(central_values,"central_imputation.csv",row.names = F)

sum(is.na(central_values))

#imputing using KNN imputation

dat_knn <- knnImputation(data,k=5)
sum(is.na(dat_knn))
knn_values <- subset(dat_knn,select = c(4,11,14,23))

write.csv(knn_values,"knn_imputation.csv",row.names = T)

# For variables with larger values, Knn imputation given good approximations
# for variables with smaller values, both imputations are not giving better results.




