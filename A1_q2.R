# Question 2 Studying Crime data

crime_data = read.table("Ericksen.dat.txt", header = TRUE)

# Part a : Regression with all variables

model = lm(Undercount ~ Minority + Crime + Poverty + Language + HighSchool + Housing + Conventional, data=crime_data)

summary(model)

# Residual Plot
plot(model$fitted.values, model$residuals)
