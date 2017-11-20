
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

#Visualizing the data

#checking the correlation between the variabes

cor_data <- cor(diagnosed_data)

corrplot::corrplot(cor_data,method = "number",title = "correlation plot b/w the attributes")

unique_cors <- cor_data[upper.tri(cor_data)]

sum(abs(unique_cors)>0.7)

#write.csv(cor_data,"correlation_matrix.csv")
#From the correlation plot there are some variables that are highly correlated

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
                    mapping = aes(x = diagnosed_data[21], y = diagnosed_data$V49))+geom_line()



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

write.csv(descrpt_stats,"descriptive_statistics.csv")


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
?ggplot







