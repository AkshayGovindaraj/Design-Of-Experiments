# Design of Experiments 1st assignment on Linear Regression

car_data = read.csv('A1_data.csv')
colnames(car_data)[1] <- "Toyota"
colnames(car_data)[2] <- "Ford"
colnames(car_data)[3] <- "GM"

model = lm(GM ~ Toyota + Ford, data=car_data)

summary(model)