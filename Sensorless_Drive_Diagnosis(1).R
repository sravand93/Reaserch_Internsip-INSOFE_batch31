
#setting the working directory
setwd("E:/INSOFE/Internship/Data")

#reading the data from the text file
diagnosed_data <- read.table("Sensorless_drive_diagnosis.txt")

#understanding the data
str(diagnosed_data) 
dim(diagnosed_data)

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
corrplot::corrplot(cor_data,title = "correlation plot b/w the attributes")
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
?plot

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
write.csv(variance,"variance.csv")

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

boxplot(diagnosed_data_without_target$V19)

#####################################

#Practicing ggplot

p <- ggplot(diagnosed_data,aes(x=diagnosed_data$V1,y=diagnosed_data$V2))
p+geom_line(aes(color = diagnosed_data$V49))


#install.packages("RColorBrewer")
library(RColorBrewer)
hist(diagnosed_data,col=brewer.pal(11,"Set3"),main="Set11 3 colors")

plot(diagnosed_data)

plot(diagnosed_data_without_target,col=brewer.pal(48,"Set1"))


#install.packages("tableplot")

library(tableplot)
tableplot(diagnosed_dataZ)Z

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
dim(diagnosed_data_test)
dim(diagnosed_data_train)
dim(diagnosed_data_valid)

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

#now splitting the pca data into train and test and validation
ind <- createDataPartition(pca_with_target$V49,p=0.6, list = F)
pca_train <- pca_with_target[ind,]
pca_tval <- pca_with_target[-ind,]
tind <- createDataPartition(pca_tval$V49,p=0.5,list = F)
pca_val <- pca_tval[tind,]
pca_test <- pca_tval[-tind,]

save.image("E:/INSOFE/Internship/Data/workspace.RData")

# logistic model building without pca
library(nnet)
logistic <- multinom(formula = V49~.,data = diagnosed_data_train)
summary(logistic)
prob_logistic <- predict(logistic,type ="class")
dim(prob_logistic)
dim(logistic$fitted.values)
#plot the ROC curve to determine the cutoff from that elbow curve
library(ROCR)
dim(diagnosed_data_train)
pred_log_test <- predict(logistic,diagnosed_data_valid[-49],type = "class")
conf_mat <- table(pred_log_test,diagnosed_data_valid$V49)
matrecs <- confusionMatrix(pred_log_test,diagnosed_data_valid$V49)

#logistic model building with PCA

logistic_pca <- multinom(formula =V49~.,data = pca_train)
summary(logistic_pca)
pred_log_pca <- predict(logistic_pca,pca_val[-21],type = "class")
confusionMatrix(pred_log_pca,pca_val$V49)
head(fitted(logistic_pca))
predicted_data <- cbind(pca_val[-21],pred_log_pca)
dim(predicted_data)
head(predicted_data)

#trying to plot the graph

library(reshape2)

#plotting row wise data using long data format

long_pred_data <- melt(predicted_data,id.vars = c("Comp.1","Comp.2","Comp.3","Comp.4",
                                                  "Comp.5", "Comp.6","Comp.7","Comp.8",
                                                  "Comp.9","Comp.10","Comp.11","Comp.12",
                                                  "Comp.13","Comp.14","Comp.15","Comp.16",
                                                  "Comp.17","Comp.18","Comp.19","Comp.20"), 
                                                   value.name = "probability")
head(long_pred_data)
names(long_pred_data)

library(ggplot2)
ggplot(long_pred_data,aes(x=Comp.10,y=probability,colour = probability))+
  geom_line()+facet_grid(variable ~.,scales ="free")

