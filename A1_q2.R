# Question 2 Studying Crime data
library(leaps)
crime_data = read.table("Ericksen.dat.txt", header = TRUE)

# Part a : Regression with all variables

model = lm(Undercount ~ Minority + Crime + Poverty + Language + HighSchool + Housing + Conventional, data=crime_data)
summary(model)

# Residual Plot
plot(model$fitted.values, model$residuals)

# City variable - Undercount difference
plot(crime_data$City,crime_data$Undercount, main="Undercount variation between Cities and Countryside")

# Part c : Best Subsets model
regsubsets.out <-
  regsubsets(bwt ~ age + lwt + race.cat + smoke + preterm + ht + ui + ftv.cat,
             data = lbw,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
regsubsets.out