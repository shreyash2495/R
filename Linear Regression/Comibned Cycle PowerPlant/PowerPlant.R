#set the working directory
setwd("C:/R")

#reading the data
data <- read.csv("PowerPlant.csv")

#View the data
View(data)

#checking the missing values and the mean,median,mode
summary(data)

#first 6 records
head(data)

#last 6 records
tail(data)

#to check the outliers in the variable AT
bx=boxplot(data$AT)

#to check the outliers in the variable V
bx=boxplot(data$V) 

#to check the outliers in the variable AP
bx=boxplot(data$AP)   

#to check the distribution of the variable AP
quantile(data$AP,seq(0,1,0.01))

#handling the outliers in the variable AP
data$AP = ifelse(data$AP<1000,1000,data$AP)
data$AP = ifelse(data$AP>1028,1028,data$AP)

#to check the outliers in the variable AP
bx=boxplot(data$AP) 

#to check the outliers in the variable RH
bx=boxplot(data$RH)

#to check the distribution of the variable AP
quantile(data$RH,seq(0,1,0.01))

#handling the outliers in the variable RH
data$RH = ifelse(data$RH<38,38,data$RH)

#to check the outliers in the variable RH
bx=boxplot(data$RH)

#create training and testing data
t1 = sample(1:nrow(data),0.8*nrow(data))
t_train1 = data[t1,]
t_test1 = data[-t1,]
 
#install package car
install.packages("car")

#load the package
library(car)

#to create model
mod1 <- lm(PE~.,data = t_train1)
summary(mod1)

#to check VIF of the model
vif(mod1)

#the VIF of AT is greater than 5 so we exclude the variable AT and will create the model again
#to create second model
mod2 <- lm(PE~V+AP+RH,data = t_train1)
summary(mod2)

#to check VIF of the model
vif(mod2)

#predictions
#prediction based on model 1
prediction <- predict(mod1,t_test1)
head(prediction)

#to view prediction
View(prediction)

#to compare prediction of model 1 with test data and adding column prediction to data
output <- cbind(t_test1,prediction)
head(output)
final_output <- output[-c(1:4)]   #to remove column 1:4
head((final_output))


#prediction based on model 2
prediction1 <- predict(mod2,t_test1)
head(prediction1)

#to view prediction
View(prediction1)

#to compare prediction of model 2 with test data and adding column prediction to data
output <- cbind(t_test1,prediction1)
head(output)
final_output <- output[-c(1:4)]   #to remove column 1:4
head((final_output))

#to plot graph
plot(final_output$PE,final_output$prediction1)

plot(final_output$PE,final_output$prediction)


#to check regression evaluation

#install the package DMwr
install.packages("DMwR")

#load the package DMwr
library(DMwR)

regr.eval(final_output$PE,final_output$prediction1) #for model 2

regr.eval(final_output$PE,final_output$prediction)  #for mmodel 1
