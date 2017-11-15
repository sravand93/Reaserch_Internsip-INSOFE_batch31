
#setting the working directory
setwd("E:/INSOFE/Internship/Data")

#reading the data from the text file
diagnosed_data <- read.table("Sensorless_drive_diagnosis.txt")

#understanding the data

str(diagnosed_data) 

#The data given is completely in numreric format including the target

#Visualizing the data

cor_data <- cor(diagnosed_data)

corrplot::corrplot(cor_data,title = "correlation plot b/w the attributes")

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



#calcuating the descrriptive statics like mean, variance,stadard Deviation

mean <- apply(diagnosed_data,2,"mean")
median <- apply(diagnosed_data,2,"median")


#write.csv(diagnosed_data,"org_data.csv",row.names = F)


