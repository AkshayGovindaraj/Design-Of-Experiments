# Design of Experiments 1st assignment on Linear Regression

car_data = read.csv('A1_data.csv')
colnames(car_data)[1] <- "Toyota"
colnames(car_data)[2] <- "Ford"
colnames(car_data)[3] <- "GM"

#Scatterplots
par(mfrow=c(1,2))
plot(car_data$Toyota,car_data$GM)
plot(car_data$Ford,car_data$GM)

#Regression Model
model = lm(GM ~ Toyota + Ford, data=car_data)
summary(model)

#Residual Plot
par(mfrow=c(1,1))
plot(model$fitted.values, model$residuals)