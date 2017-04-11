setwd("E:/DataScience/Practise")
data <- read.csv("regression.csv")
View(test)


plot(data, pch = 16)

model <- lm(Y ~ X, data)

abline(model)

predictedY <- predict(model, data)

points(data$X, predictedY, col = "blue", pch=4)

error <- model$residuals
rmse <- function(error)
{
  sqrt(mean(error^2))
}
predictionRMSE <- rmse(error)   # 5.703778
predictionRMSE
model <- svm(Y ~ X , data)
predictedY <- predict(model, data)
points(data$X, predictedY, col = "red", pch=4)

error <- data$Y - predictedY
svrPredictionRMSE <- rmse(error)

# perform a grid search
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

print(tuneResult)

plot(tuneResult)


# On this graph we can see that the darker the region is the better our model is 
# (because the RMSE is closer to zero in darker regions).


tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)


# As we zoomed-in inside the dark region we can see that there is several darker patch.
# From the graph you can see that models with C between 200 and 300 and ???? between
# 0.8 and 0.9 have less error.


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

error <- data$Y - tunedModelY  

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)   
tunedModelRMSE
