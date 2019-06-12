library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

#Read data
df <- read.csv('student-mat.csv', sep=';')
#To get only the numeric columns
num.cols <- sapply(df,is.numeric)
#Filter numeric columns for correlation
corr.data <- cor(df[,num.cols])
#Print corr.data
print(corr.data)
#Correlation Visualization
print(corrplot(corr.data,method = 'color'))
print(corrgram(df))
print(corrgram(df,order = TRUE, 
               lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt))
#Visualization of G3 - target class
print(ggplot(df,aes(x=G3)) + geom_histogram(bins = 20,alpha=0.5, fill='blue'))
#Split data into Train and Test set
library(caTools)
set.seed(101)
#Split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)

#70% of my training data
train <- subset(df,sample == TRUE)

#30% of test data
test <- subset(df,sample == FALSE)

#Train and build model

model <- lm(G3 ~.,data = train)

#Run the model

#Interpret the model
print(summary(model))

#Visualizing the residuals
res <- residuals(model)
res <- as.data.frame(res)
print(ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5))

#Prediction
G3.prediction <- predict(model,test)

results <- cbind(G3.prediction,test$G3)
colnames(results) <- c('predicted','actual')
results <-as.data.frame(results)

# Take care of negative values - make results are showing negative marks, which is not possible
to_zero <-function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
#Apply zero function
results$predicted <- sapply(results$predicted,to_zero)

#MSE and RMSE to evaluate the model

mse <- mean((results$actual-results$predicted)^2)
print(mse)

rmse <- sqrt(mean((results$actual-results$predicted)^2))
print(rmse)


#R-squared

sse <- sum((results$predicted-results$actual)^2)
sst <- sum((mean(df$G3) - results$actual)^2)

r2 <- 1 -(sse/sst)
print(r2)