# First load the data into a variable Data. Also discard such entries with missing values. 

Data = read.csv("power.csv", header = T, na.strings = "x")
Data = na.omit(Data) 

# Load the package ggplot2
library(ggplot2)

# Plot scatterplots for each predictor vs the PE attribute
ATvsPE = ggplot(data = Data, aes(x=AT, y=PE))
ATvsPE + geom_point(colour = "deepskyblue") + geom_smooth(colour = "black")

VvsPE = ggplot(data = Data, aes(x=V, y=PE))
VvsPE + geom_point(colour = "deepskyblue") + geom_smooth(colour = "black")

APvsPE = ggplot(data = Data, aes(x=AP, y=PE))
APvsPE + geom_point(colour = "brown1") + geom_smooth(colour = "black")

RHvsPE = ggplot(data = Data, aes(x=RH, y=PE))
RHvsPE + geom_point(colour = "brown1") + geom_smooth(colour = "black")

# Calculate the correlation coefficients and covariance for each relationship
cor(Data$AT,Data$PE)
cov(Data$AT,Data$PE)

cor(Data$V,Data$PE)
cov(Data$V,Data$PE)

cor(Data$AP,Data$PE)
cov(Data$AP,Data$PE)

cor(Data$RH,Data$PE)
cov(Data$RH,Data$PE)


#	Generate the multi linear regression models using all the predictors 
# against the target. 

multi_linear_model = lm(PE ~ AT + V + AP + RH, data = Data)
summary(multi_linear_model)

# Perform a 5-fold cross-validation to calculate the average (unbiased estimate) Mean Square Error (MSE) from scratch.

# Randomly shuffle the data
Data = Data[sample(nrow(Data)),]

# Create 5 equally sized folds
folds = cut(seq(1,nrow(Data)),breaks=5,labels=FALSE)

# Define a function to calculate the Mean Square Error
MSE = function(error){ mean(error^2) }

# Initialize a variable to store the total MSE for all 5 rounds
Total_MSE = 0

# Perform cross-validation process
for(i in 1:5){
  #Segment data into test and training data by fold
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = Data[testIndexes, ]
  trainData = Data[-testIndexes, ]
  #Use the train data to train the model
  multi_linear_model = lm(PE ~ AT + V + AP + RH, data = trainData)
  #Generate the predicted values
  pred = predict.lm(multi_linear_model, testData[1:4])
  # Calculate the error, which is the difference between predicted and actual
  # values from test data
  error = pred - testData[5]
  # Increment the Total_MSE by the error for each round
  Total_MSE = Total_MSE + MSE(error)
}
# Finally, divide the Total_MSE by 5 to get the average MSE
avg_MSE = Total_MSE/5

avg_MSE


