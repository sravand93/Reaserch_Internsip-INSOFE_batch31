#setting the working directory
setwd("E:/INSOFE/Internship/Data")

load("workspace.RData")

#libraries

library(ggplot2)
library(caret)
library(MLmetrics)
library(vegan)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(class)
library(e1071)


#reading the data from the text file
diagnosed_data <- read.table("Sensorless_drive_diagnosis.txt")

#understanding the data
str(diagnosed_data) 
dim(diagnosed_data)
View(diagnosed_data)

#The data given is completely in numreric format including the target
# hence change the target variable into factor
diagnosed_data$V49 <- as.factor(diagnosed_data$V49)
str(diagnosed_data)

# AS we can calculate the correlation only for the numericals
#lets remove the targeet variable which was now converted to a categorical
diagnosed_data_without_target <- subset(diagnosed_data,select = -c(49))
diagnosed_data_target <- subset(diagnosed_data,select = c(49))

#Visualizing the data

#checking the correlation between the variabes
cor_data <- cor(diagnosed_data_without_target)
corrplot::corrplot(cor_data,title = "correlation plot b/w the attributes", method = 'number')
unique_cors <- cor_data[upper.tri(cor_data)]
sum(abs(unique_cors)>0.7)

#write.csv(cor_data,"correlation_matrix.csv")

############ PLOTS  ##############
plot(density(diagnosed_data$V1))
#plots for different variables
library(ggplot2)
a <- ggplot(diagnosed_data,aes(diagnosed_data[,21]))
a+geom_area(stat = "bin")
#a+geom_histogram()+geom_density(aes(color="red"))
a+geom_density()
+geom_histogram(aes(color ="blue"))

#Plotting and  saving of Histogram plots for all the individual variables
for (i in 1:49){
  dev.copy(jpeg,filename=paste(names(diagnosed_data[i]),"plot.jpg",sep="_"))
  hist(diagnosed_data[,i],xlab = names(diagnosed_data[i]),ylab = 'Frequency')
  dev.off()
}

#plotting and saving of density plots for all the individual variables
for (i in 1:49){
  
  plot= ggplot(diagnosed_data,aes(diagnosed_data[i]))+geom_density()
  print(plot)
  dev.copy(jpeg,filename=paste(names(diagnosed_data[i]),"density_plot.jpg",sep = "_"))
  dev.off()
}

#plotting the graphs between the varaibles

for (i in 1:48){
  ggplotObj <- ggplot(data = diagnosed_data,
                    mapping = aes(x = diagnosed_data[i], y = diagnosed_data$V49))+geom_point()
  print(ggplotObj)
  dev.copy(jpeg,filename=paste(names(diagnosed_data[i]),"relation_plot.jpg",sep = "_"))
  dev.off()
}

for (i in 1:48){
  ggplotObj <- ggplot(data = diagnosed_data,
                      mapping = aes(x = diagnosed_data[i], y = diagnosed_data$V49))+geom_line()
  print(ggplotObj)
  dev.copy(jpeg,filename=paste(names(diagnosed_data[i]),"relation_line_plot.jpg",sep = "_"))
  dev.off()
}

ggplotObj <- ggplot(data = diagnosed_data,
                    mapping = aes(x =diagnosed_data$V49 , y =diagnosed_data[21] ))+geom_point()

##########  PLOTS  END #############


#calcuating the descrriptive statics like mean, variance,stadard Deviation
mean_data <- apply(diagnosed_data,2,"mean")
median_data <- apply(diagnosed_data,2,"median")
sMPLE <- sd(diagnosed_data$V1)
std_dev_data <- apply(diagnosed_data,2, sd)
min_val_data <- apply(diagnosed_data,2,min)
max_val_data <- apply(diagnosed_data,2,max)
missing_val_data <- sum(is.na(diagnosed_data));missing_val_data
descrpt_stats <- data.frame(mean=mean_data,median = median_data,std_dev = std_dev_data,
                         min_val = min_val_data,max_val = max_val_data)
View(descrpt_stats)

#write.csv(descrpt_stats,"descriptive_statistics.csv")

# finding all the statics without the target varaible
mean_data <- apply(diagnosed_data_without_target,2,"mean")
median_data <- apply(diagnosed_data_without_target,2,"median")
std_dev_data <- apply(diagnosed_data_without_target,2, sd)
min_val_data <- apply(diagnosed_data_without_target,2,min)
max_val_data <- apply(diagnosed_data_without_target,2,max)
View(mean_data)


##################  PLOTS   ##################
plot(descrpt_stats$mean,type = "l")
par(mfrow = c(2,3))
plot(mean_data,type = "b")
plot(median_data,type = "l")
plot(std_dev_data,type = "l")
plot(min_val_data,type = "l")
plot(max_val_data,type = "l")


################   PLOTS END  #########################

#Detecting outliers using boxplot
scatter.smooth(diagnosed_data$V1)

for (i in 1:2){
  dev.copy(jpeg,filename=paste(names(diagnosed_data[i]),"boxplot.jpg",sep = "_"))
  boxplot(diagnosed_data[,i],main = "diagnosed_data[i]")
  dev.off()
}

var(diagnosed_data$V1)
var(x=diagnosed_data$V19,y=diagnosed_data$V20)

#finding the variance

variance <- apply(diagnosed_data, 2,var)
View(variance)
variance <- data.frame(variance)
#write.csv(variance,"variance.csv")

#write.csv(diagnosed_data,"org_data.csv",row.names = F)

# Data pre-processsing:

# missing values

sum(is.na(diagnosed_data))

# There are no missing values in the data....

summary(diagnosed_data)


# standardizing all the numerical attributes
library(vegan)
diagnosed_data_without_target <- decostand(diagnosed_data_without_target,"standardize")
str(diagnosed_data_without_target)
summary(diagnosed_data_without_target)

boxplot(diagnosed_data_without_target,title = "Boxplot")
#####################################

#Practicing ggplot

# p <- ggplot(diagnosed_data,aes(x=diagnosed_data$V1,y=diagnosed_data$V2))
# p+geom_line(aes(color = diagnosed_data$V49))
# 
# 
# #install.packages("RColorBrewer")
# library(RColorBrewer)
# hist(diagnosed_data,col=brewer.pal(11,"Set3"),main="Set11 3 colors")
# 
# plot(diagnosed_data)
# 
# plot(diagnosed_data_without_target,col=brewer.pal(48,"Set1"))


#install.packages("tableplot")

# library(tableplot)
# tableplot(diagnosed_dataZ)Z

#######################################################################

# Splitting the data into test, train and validation
library(caret)
set.seed(1235)
indices <- createDataPartition(diagnosed_data$V49,p=0.6,list = F)
diagnosed_data_train <- diagnosed_data[indices,]
diagnosed_data_tval <- diagnosed_data[-indices,]
tindices <- createDataPartition(diagnosed_data_tval$V49,p=0.5,list = F)
diagnosed_data_valid <- diagnosed_data_tval[tindices,]
diagnosed_data_test <- diagnosed_data_tval[-tindices,]
dim(diagnosed_data_train)
dim(diagnosed_data_valid)
dim(diagnosed_data_test)

# performing PCA for dimentionality reduction 
data_for_PCA <- diagnosed_data_without_target
PCA_diagnosed_data <- princomp(data_for_PCA,cor = TRUE)

plot(PCA_diagnosed_data)
screeplot(PCA_diagnosed_data,type = "line")
summary(PCA_diagnosed_data)
PCA_diagnosed_data$loadings
pca_score <- PCA_diagnosed_data$scores
corrplot::corrplot(cor(pca_score))
plot(PCA_diagnosed_data)
plot(pca_score)
s = cov(diagnosed_data_without_target)

pca_final <- PCA_diagnosed_data$scores[,1:20]
pca_with_target <- cbind(pca_final,diagnosed_data_target)

#now splitting the PCA data into train and test and validation
ind <- createDataPartition(pca_with_target$V49,p=0.6, list = F)
pca_train <- pca_with_target[ind,]
pca_tval <- pca_with_target[-ind,]
tind <- createDataPartition(pca_tval$V49,p=0.5,list = F)
pca_val <- pca_tval[tind,]
pca_test <- pca_tval[-tind,]

#save.image("E:/INSOFE/Internship/Data/workspace.RData")

# logistic model building without pca
library(nnet)
logistic <- multinom(formula = V49~.,data = diagnosed_data_train)
#summary(logistic)
prob_logistic <- predict(logistic,type ="class")
dim(prob_logistic)
dim(logistic$fitted.values)
View(prob_logistic)

pred_log_train <- predict(logistic,diagnosed_data_train[,-49],type = 'class')
conf_mat_log_train <- confusionMatrix(pred_log_train,diagnosed_data_train$V49)
accuracy = Accuracy(pred_log_train,diagnosed_data_train$V49)
Recall   = Recall(diagnosed_data_train$V49,pred_log_train)
precision = Precision(diagnosed_data_train$V49,pred_log_train)
F1_score = F1_Score(diagnosed_data_train$V49,pred_log_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

pred_log_test <- predict(logistic,diagnosed_data_valid[-49],type = "class")
conf_mat <- table(pred_log_test,diagnosed_data_valid$V49)
matrecs <- confusionMatrix(pred_log_test,diagnosed_data_valid$V49)

accuracy <- Accuracy(pred_log_test,diagnosed_data_valid$V49)
Recall <- Recall(diagnosed_data_valid$V49,pred_log_test)
precision <- Precision(diagnosed_data_valid$V49,pred_log_test)
F1_score <- F1_Score(diagnosed_data_valid$V49,pred_log_test)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)


#logistic model building with PCA
logistic_pca <- multinom(formula =V49~.,data = pca_train)
#summary(logistic_pca)
pred_log_pca_train <- predict(logistic_pca,pca_train[,-21],type = 'class')
pred_log_pca_val<- predict(logistic_pca,pca_val[-21],type = "class")

conf_mat_log_pca_train <- confusionMatrix(pred_log_train,pca_train$V49) 
accuracy <- Accuracy(pred_log_pca_train,pca_train$V49)
Recall <- Recall(pca_train$V49,pred_log_pca_train)
precision <- Precision(pca_train$V49,pred_log_pca_train)
F1_score <- F1_Score(pca_train$V49,pred_log_pca_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

table(pred_log_pca,pca_val$V49)
confusionMatrix(pred_log_pca,pca_val$V49)
confusionMatrix(pca_val$V49,pred_log_pca)
head(fitted(logistic_pca))
predicted_data <- cbind(pca_val[-21],pred_log_pca)

accuracy <- Accuracy(pred_log_pca,pca_val$V49)
Recall <- Recall(pca_val$V49,pred_log_pca)
precision = Precision(pca_val$V49,pred_log_pca)
F1_score =F1_Score(pca_val$V49,pred_log_pca)
AUC(pred_log_pca,pca_val$V49)

evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)


########Another method to calculate the metrics for a multiclass confusion matrix
#calculating the metrics using confusion matrix
cm <- table(pred_log_pca,pca_val$V49)
confusionMatrix(pca_val$V49,pred_log_pca)
library(MLmetrics)
Precision(pca_val$V49,pred_log_pca)
Recall(pca_val$V49,pred_log_pca)
n  <- sum(cm)  #of instances
nc <- nrow(cm) #of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

Accuracy <- sum(diag)/n
precision <- diag/colsums
recall <- diag/rowsums
f1 <- 2*precision* recall/(precision+recall)
data.frame(precision,recall,f1)


#############  Descision Trees  #################

##### C50 MODEL

library(C50) #used to perform c50 model 
?C5.0

model_c5.0 <- C5.0(x=diagnosed_data_train[,-49],y=diagnosed_data_train[,49])
summary(model_c5.0)
plot(model_c5.0)

model_c5.0_val <- predict(model_c5.0,diagnosed_data_valid[,-49])
cm <-table(model_c5.0_val,diagnosed_data_valid$V49)
Accuracy <- sum(diag(cm))/sum(cm)
#metrics
accuracy=Accuracy(model_c5.0_val,diagnosed_data_valid$V49)
Recall=Recall(diagnosed_data_valid$V49,model_c5.0_val)
precision=Precision(diagnosed_data_valid$V49,model_c5.0_val)
F1_score=F1_Score(diagnosed_data_valid$V49,model_c5.0_val)
AUC(diagnosed_data_valid$V49,model_c5.0_val)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)


pred_c5.0_train <- predict(model_c5.0,diagnosed_data_train[,-49])
cm_train <- table(pred_c5.0_train,diagnosed_data_train$V49)
Accuracy_train <- sum(diag(cm_train))/sum(cm_train)

accuracy=Accuracy(pred_c5.0_train,diagnosed_data_train$V49)
Recall=Recall(diagnosed_data_train$V49,pred_c5.0_train)
precision=Precision(diagnosed_data_train$V49,pred_c5.0_train)
F1_score=F1_Score(diagnosed_data_train$V49,pred_c5.0_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

##### C5.0 for PCA data

model_c.50_pca <- C5.0(x = pca_train[,-21],y = pca_train[,21])
summary(model_c.50_pca)

pred_c50_pcA_train <- predict(model_c.50_pca,pca_train[,-21])
accuracy=Accuracy(pred_c50_pcA_train,pca_train$V49)
Recall=Recall(pca_train$V49,pred_c50_pcA_train)
precision=Precision(pca_train$V49,pred_c50_pcA_train)
F1_score=F1_Score(pca_train$V49,pred_c50_pcA_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

pred_c50_pcA_valid <- predict(model_c.50_pca,pca_val[,-21])
accuracy=Accuracy(pred_c50_pcA_valid,pca_val$V49)
Recall=Recall(pca_val$V49,pred_c50_pcA_valid)
precision=Precision(pca_val$V49,pred_c50_pcA_valid)
F1_score=F1_Score(pca_val$V49,pred_c50_pcA_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

####### CART Model

model_cart <- rpart(formula = V49~.,data = diagnosed_data_train)
summary(model_cart)
rpart.plot(model_cart,cex=0.3)

pred_cart_train <- predict(model_cart,diagnosed_data_train[,-49],'class')
cnf_mat_cart_train <- table(pred_cart_train,diagnosed_data_train$V49)
accuracy=Accuracy(pred_cart_train,diagnosed_data_train$V49)
Recall=Recall(diagnosed_data_train$V49,pred_cart_train)
precision=Precision(diagnosed_data_train$V49,pred_cart_train)
F1_score=F1_Score(diagnosed_data_train$V49,pred_cart_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

model_cart_val <- predict(model_cart,diagnosed_data_valid[,-49],'class')
cnf_mat <- table(model_cart_val,diagnosed_data_valid$V49)
accuracy_cart <- sum(diag(cnf_mat))/sum(cnf_mat)

accuracy=Accuracy(model_cart_val,diagnosed_data_valid$V49)
Recall=Recall(diagnosed_data_valid$V49,model_cart_val)
precision=Precision(diagnosed_data_valid$V49,model_cart_val)
F1_score=F1_Score(diagnosed_data_valid$V49,model_cart_val)
AUC(model_cart_val,diagnosed_data_valid$V49)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

#####  CART(rpart) model on PCA data ######

model_cart_pca <- rpart(formula = V49 ~.,data = pca_train)
summary(model_cart_pca)
rpart.plot(model_cart_pca,box.palette = 0)

pred_cart_pca_train <- predict(model_cart_pca,pca_train[,-21],'class')
conf_mat_cart_pca_train <- confusionMatrix(pred_cart_pca_train,pca_train$V49)
accuracy=Accuracy(pred_cart_pca_train,pca_train$V49)
Recall=Recall(pca_train$V49,pred_cart_pca_train)
precision=Precision(pca_train$V49,pred_cart_pca_train)
F1_score=F1_Score(pca_train$V49,pred_cart_pca_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)


pred_cart_pca_valid <- predict(model_cart_pca,pca_val[,-21],'class')
conf_mat_cart_pca_valid <- confusionMatrix(pred_cart_pca_valid,pca_val$V49)
?confusionMatrix
accuracy=Accuracy(pred_cart_pca_valid,pca_val$V49)
precision=Precision(pca_val$V49,pred_cart_pca_valid)
Recall=Recall(pca_val$V49,pred_cart_pca_valid)
F1_score=F1_Score(pca_val$V49,pred_cart_pca_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

###############

######  KNN model

model_Knn <- knn(train = diagnosed_data_train[,-49],
                 test = diagnosed_data_valid[,-49],cl = diagnosed_data_train$V49,
                 k=5)
summary(model_Knn)
conf_mat_knn_valid <- table(diagnosed_data_valid$V49,model_Knn)
accuracy_knn_valid <- sum(diag(conf_mat_knn_valid))/sum(conf_mat_knn_valid)

accuracy=Accuracy(model_Knn,diagnosed_data_valid$V49)
Recall=Recall(diagnosed_data_valid$V49,model_Knn)
precision=Precision(diagnosed_data_valid$V49,model_Knn)
F1_score=F1_Score(diagnosed_data_valid$V49,model_Knn)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)

#using library caret
knn_model <- knn3(V49 ~ .,data = diagnosed_data_train,k=5)
summary(knn_model)

pred_knn3_train <- predict(knn_model,diagnosed_data_train[,-49],'class')
conf_mat_KNN3_train <- table(model_knn3_train,diagnosed_data_train$V49)
accuaracy_knn3_train <- sum(diag(conf_mat_KNN3_train))/sum(conf_mat_KNN3_train)
accuracy=Accuracy(pred_knn3_train,diagnosed_data_train$V49)
Recall=Recall(diagnosed_data_train$V49,pred_knn3_train)
precision=Precision(diagnosed_data_train$V49,pred_knn3_train)
F1_score=F1_Score(diagnosed_data_train$V49,pred_knn3_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_score)


model_knn3_valid <- predict(knn_model,diagnosed_data_valid[,-49],'class')
conf_mat_knn3_valid <- table(model_knn3_valid,diagnosed_data_valid$V49)
accuracy_knn3_valid <- sum(diag(conf_mat_knn3_valid))/sum(conf_mat_knn3_valid)


knn_model_pca <- knn3(V49~.,data = pca_train,k=5)

pred_knn3_pca_train <- predict(knn_model_pca,pca_train[,-21],'class')
conf_mat_knn3_pca_train <- confusionMatrix(pred_knn3_pca_train,pca_train$V49)

pred_knn3_pca_valid <- predict(knn_model_pca,pca_val[,-21],'class')
conf_mat_knn3_pca_valid <- confusionMatrix(pred_knn3_pca_valid,pca_val$V49)
accuracy=Accuracy(pred_knn3_pca_valid,pca_val$V49)
precision=Precision(pca_val$V49,pred_knn3_pca_valid)
Recall=Recall(pca_val$V49,pred_knn3_pca_valid)
F1_Score=F1_Score(pca_val$V49,pred_knn3_pca_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


########### SVM ###########

model_svm <- svm(V49~.,data = diagnosed_data_train)
summary(model_svm)

model_svm_train <- predict(model_svm,diagnosed_data_train[,-49])
conf_mat_svm_train <- table(model_svm_train,diagnosed_data_train$V49)
accauracy_svm_train <- sum(diag(conf_mat_svm_train))/sum(conf_mat_svm_train)

model_svm_validate <- predict(model_svm,diagnosed_data_valid[,-49])
conf_mat_svm_valid <- table(model_svm_validate,diagnosed_data_valid$V49)
accuaracy_svm_valid <- sum(diag(conf_mat_svm_valid))/sum(conf_mat_svm_valid)

library(MLmetrics)
accuracy=Accuracy(model_svm_validate,diagnosed_data_valid$V49)
precision=Precision(diagnosed_data_valid$V49,model_svm_validate)
Recall=Recall(diagnosed_data_valid$V49,model_svm_validate)
F1_Score=F1_Score(diagnosed_data_valid$V49,model_svm_validate)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


model_svm_pca <- svm(V49 ~.,data = pca_train)
summary(model_svm_pca)

pred_svm_pca_train <- predict(model_svm_pca,pca_train[,-21])
conf_mat_svm_pca_train <- confusionMatrix(pred_svm_pca_train,pca_train$V49)

accuracy=Accuracy(pred_svm_pca_train,pca_train$V49)
Precision=Precision(pca_train$V49,pred_svm_pca_train)
Recall=Recall(pca_train$V49,pred_svm_pca_train)
F1_Score=F1_Score(pca_train$V49,pred_svm_pca_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=Precision,'F1_score'=F1_Score)


pred_svm_pca_valid <- predict(model_svm_pca,pca_val[,-21])
conf_mat_svm_pca_val <- confusionMatrix(pred_svm_pca_valid,pca_val$V49)
accuracy=Accuracy(pred_svm_pca_valid,pca_val$V49)
Precision=Precision(pca_val$V49,pred_svm_pca_valid)
Recall=Recall(pca_val$V49,pred_svm_pca_valid)
F1_Score=F1_Score(pca_val$V49,pred_svm_pca_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=Precision,'F1_score'=F1_Score)

# Random forest model

set.seed(1113)
model_randomForest <- randomForest(V49~.,data = diagnosed_data_train,
                                   importance =TRUE,ntree = 100)


model_randomForest$importance
#varImpPlot(model_randomForest)

#predicion on train and evaluation metrics
pred_randomForest_train <- predict(model_randomForest,diagnosed_data_train[,-49])
conf_mat_train_randmforest <- confusionMatrix(pred_randomForest_train,diagnosed_data_train$V49)
accuracy <-Accuracy(pred_randomForest_train,diagnosed_data_train$V49)
Recall <- MLmetrics::Recall(diagnosed_data_train$V49,pred_randomForest_train)
precision <- MLmetrics::Precision(diagnosed_data_train$V49,pred_randomForest_train)
F1_Score <- MLmetrics::F1_Score(diagnosed_data_train$V49,pred_randomForest_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)

#predictions on validation data and evaluation metrics

library(MLmetrics)
pred_randomForest_valid <- predict(model_randomForest,diagnosed_data_valid[,-49])
conf_mat_validate_randomforest <- confusionMatrix(pred_randomForest_valid,diagnosed_data_valid$V49)
accuracy = MLmetrics::Accuracy(pred_randomForest_valid,diagnosed_data_valid$V49)
Recall <- MLmetrics::Recall(diagnosed_data_valid$V49,pred_randomForest_valid)
prec <- MLmetrics::Precision(diagnosed_data_valid$V49,pred_randomForest_valid)
f1_score <- MLmetrics::F1_Score(diagnosed_data_valid$V49,pred_randomForest_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)

model_randomForest_PCA <- randomForest(V49~.,data = pca_train,ntree = 100,importance=TRUE)

#prediction on PCA_train
pred_randomForest_PCA_train <- predict(model_randomForest_PCA,pca_train[,-21])

conf_mat_randomforest_pca_train <- confusionMatrix(pred_randomForest_PCA_train,pca_train$V49)
accuracy = MLmetrics::Accuracy(pred_randomForest_PCA_train,pca_train$V49)
Recall <- MLmetrics::Recall(pca_train$V49,pred_randomForest_PCA_train)
prec <- MLmetrics::Precision(pca_train$V49,pred_randomForest_PCA_train)
f1_score <- MLmetrics::F1_Score(pca_train$V49,pred_randomForest_PCA_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


#prediction on validate data

pred_randomForest_PCA_valid <- predict(model_randomForest_PCA,pca_val[,-21])
conf_mat_randomforest_pca_train <- confusionMatrix(pred_randomForest_PCA_valid,pca_val$V49)
accuracy = MLmetrics::Accuracy(pred_randomForest_PCA_valid,pca_val$V49)
Recall <- MLmetrics::Recall(pca_val$V49,pred_randomForest_PCA_valid)
prec <- MLmetrics::Precision(pca_val$V49,pred_randomForest_PCA_valid)
f1_score <- MLmetrics::F1_Score(pca_val$V49,pred_randomForest_PCA_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)



#### Boosting ########

#install.packages("gbm")

library(gbm)

model_xgboost <- gbm(formula = V49~.,data = diagnosed_data_train,n.trees = 1000)

pred_xgboost_train <- predict(model_xgboost,diagnosed_data_train[,-49],n.trees = 1000, 
                              type = "response")
sink("xgboost_predfile.txt")
pred_xgboost_train
sink()


dim(diagnosed_data_train)
dim(pred_xgboost_train)
View(pred_xgboost_train)

#pred_2<-apply(pred,1,which.max)
pred <- data.frame(pred_xgboost_train)
maxi<-function(x){max(x)}
maxim<-data.frame(apply(pred,1,max))
View(maxim)
a<- maxim
# str(pred_xgboost_train)
# which(max(pred_xgboost_train[1524,,1]))
# pred_xgboost_train[1524,,1]
# View(a)
#a<- data.frame()
dim(a)
dim(pred)
# b<-data.frame(which.max(pred[25263,]))
# which.max(pred_xgboost_train[25263,,1])

labels<-a
for(i in 1:35112){
    labels[i,] = which.max(pred[i,])
}
labels$apply.pred..1..max. -> labels$prediction
labels$apply.pred..1..max.<- NULL

conf_mat_xgboost_train <- confusionMatrix(labels$prediction,diagnosed_data_train$V49)
accuracy = MLmetrics::Accuracy(labels$prediction,diagnosed_data_train$V49)
Recall <- MLmetrics::Recall(diagnosed_data_train$V49,labels$prediction)
precision <- MLmetrics::Precision(diagnosed_data_train$V49,labels$prediction)
F1_Score <- MLmetrics::F1_Score(diagnosed_data_train$V49,labels$prediction)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


pred_xgboost_valid <- predict(model_xgboost,diagnosed_data_valid[,-49],n.trees = 1000, 
                              type = "response")

dim(pred_xgboost_valid)

pred_valid <- data.frame(pred_xgboost_valid)
maxi<-function(x){max(x)}
maxim<-data.frame(apply(pred_valid,1,max))
a<- maxim
labels<-a
for(i in 1:11704){
  labels[i,] = which.max(pred[i,])
}
labels$apply.pred_valid..1..max.-> labels$prediction
labels$apply.pred_valid..1..max.<- NULL

conf_mat_xgboost_valid <- confusionMatrix(labels$prediction,diagnosed_data_valid$V49)
accuracy = MLmetrics::Accuracy(labels$prediction,diagnosed_data_valid$V49)
Recall <- MLmetrics::Recall(diagnosed_data_valid$V49,labels$prediction)
precision <- MLmetrics::Precision(diagnosed_data_valid$V49,labels$prediction)
F1_Score <- MLmetrics::F1_Score(diagnosed_data_valid$V49,labels$prediction)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


### xgboost on PCA data

model_xgboost_pca <- gbm(formula = V49~.,data = pca_train,n.trees = 1000)

preds_xgboost_train_pca <- predict(model_xgboost_pca,pca_train[,-49],n.trees = 1000)
dim(preds_xgboost_train_pca)

pred <- data.frame(preds_xgboost_train_pca)
maxi<-function(x){max(x)}
maxim<-data.frame(apply(pred,1,max))
a<- maxim

labels<-a
for(i in 1:35112){
  labels[i,] = which.max(pred[i,])
}
labels$apply.pred..1..max.-> labels$prediction
labels$apply.pred..1..max.<- NULL

conf_mat_xgboost_valid <- confusionMatrix(labels$prediction,pca_train$V49)
accuracy <- MLmetrics::Accuracy(labels$prediction,pca_train$V49)
Recall <- MLmetrics::Recall(pca_train$V49,labels$prediction)
precision <- MLmetrics::Precision(pca_train$V49,labels$prediction)
F1_Score <- MLmetrics::F1_Score(pca_train$V49,labels$prediction)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)

preds_xgboost_valid_pca <- predict(model_xgboost,pca_val[,-49],n.trees = 1000)
dim(preds_xgboost_valid_pca)
pred <- data.frame(preds_xgboost_valid_pca)
maxi<-function(x){max(x)}
maxim<-data.frame(apply(pred,1,max))
a<- maxim

labels<-a
for(i in 1:11704){
  labels[i,] = which.max(pred[i,])
}
labels$apply.pred..1..max.-> labels$prediction
labels$apply.pred..1..max..<- NULL

conf_mat_xgboost_valid <- confusionMatrix(labels$prediction,pca_val$V49)
accuracy <- MLmetrics::Accuracy(labels$prediction,pca_val$V49)
Recall <- MLmetrics::Recall(pca_val$V49,labels$prediction)
precision <- MLmetrics::Precision(pca_val$V49,labels$prediction)
F1_Score <- MLmetrics::F1_Score(pca_val$V49,labels$prediction)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


######## Stacking #########

train_predictions <- data.frame(log = pred_log_train,c50 = model_c5.0_train,
                                cart = model_cart_train,svm =model_svm_train,
                                knn = pred_knn3_train,target = diagnosed_data_train$V49)


train_pca_predictions <- data.frame(log =pred_log_pca_train,c50 = pred_c50_pcA_train,
                                    cart = pred_cart_pca_train,svm = pred_svm_pca_train,
                                    knn = pred_knn3_pca_train,target = pca_train$V49)

valid_prediction <- data.frame(log = pred_log_test,c50 = model_c5.0_val,
                               cart = model_cart_val,svm = model_svm_validate,
                               knn = model_knn3_valid,target = diagnosed_data_valid$V49)

valid_pca_prediction <- data.frame(log = pred_log_pca_val,c50 = pred_c50_pcA_valid,
                                   cart = pred_cart_pca_valid,svm = pred_svm_pca_valid,
                                   knn = pred_knn3_pca_valid,target = pca_val$V49)


# combine the pca and original prediction for both train and validate

stack_train <- rbind(train_predictions,train_pca_predictions)
stack_validate <- rbind(valid_prediction,valid_pca_prediction)

str(stack_train)
dim(stack_train)
names(stack_train)

# convert all the variables except target variable into numerical to apply svm

numeric_stack_train <- sapply(stack_train[, !(names(stack_train) %in% "target")], 
                        function(x) as.numeric(as.character(x)))
numeric_stack_train <- as.data.frame(numeric_stack_train)
numeric_stack_train_with_target <- cbind(numeric_stack_train,target=stack_train$target)
names(numeric_stack_train_with_target)
str(numeric_stack_train_with_target)

numeric_stack_valid <- sapply(stack_validate[, !(names(stack_validate) %in% "target")], 
                              function(x) as.numeric(as.character(x)))
numeric_stack_valid <- as.data.frame(numeric_stack_valid)
numeric_stack_valid_with_target <- cbind(numeric_stack_valid,target =stack_validate$target)

model_stack <- svm(target~.,data = numeric_stack_train_with_target)

pred_stack_train <- predict(model_stack,numeric_stack_train_with_target[-6])
conf_mat_stack_train <- confusionMatrix(pred_stack_train,numeric_stack_train_with_target$target)
accuracy <- MLmetrics::Accuracy(pred_stack_train,numeric_stack_train_with_target$target)
Recall <- MLmetrics::Recall(numeric_stack_train_with_target$target,pred_stack_train)
precision <- MLmetrics::Precision(numeric_stack_train_with_target$target,pred_stack_train)
F1_Score <- MLmetrics::F1_Score(numeric_stack_train_with_target$target,pred_stack_train)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


pred_stack_valid <- predict(model_stack,numeric_stack_valid_with_target)
conf_mat_stack_train <- confusionMatrix(pred_stack_valid,numeric_stack_valid_with_target$target)
accuracy <- MLmetrics::Accuracy(pred_stack_valid,numeric_stack_valid_with_target$target)
Recall <- MLmetrics::Recall(numeric_stack_valid_with_target$target,pred_stack_valid)
precision <- MLmetrics::Precision(numeric_stack_valid_with_target$target,pred_stack_valid)
F1_Score <- MLmetrics::F1_Score(numeric_stack_valid_with_target$target,pred_stack_valid)
evaluation_metrics <- c("accuracy"=accuracy,'Recall'=Recall,
                        'precision'=precision,'F1_score'=F1_Score)


#### perform PCA on the stacked train data as the data will be correlated

#save.image("E:/INSOFE/Internship/Data/workspace.RData")

#### model on PCA  ##########

# #applying PCA on train data
# 
# numeric_stack_train_pca <- princomp(numeric_stack_train,cor = TRUE)
# summary(numeric_stack_train_pca)
# cor(numeric_stack_train)
# 
# pca_train_stack <- as.data.frame(predict(numeric_stack_train_pca,numeric_stack_train))[1:4]
# numeric_stack_train_pca_with_target<- cbind(numeric_stack_train_pca,target = stack_train$target)
# 
# #applying the pca on validation data also
# numeric_stack_valid_pcA <- as.data.frame(predict(numeric_stack_train_pca,numeric_stack_valid))[1:4]
# numeric_stack_valid_pcA_with_target <- cbind(numeric_stack_valid_pcA,target = stack_validate$target)
# 
# #build svm model on the pca data
# 
# model_stack_pca <- svm(target~.,numeric_stack_train_pca_with_target)
# pred_stack_pca_train<- predict(model_stack_pca,stack)
