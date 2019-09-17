#DOE HW5 Last Q
MyData <- read.csv(file="Book1.csv", header=TRUE, sep=",")

model2 = lm(formula = y ~ A+B+C+D+E+A*B+A*C+A*D+A*E+B*C+B*D+B*E+C*D+C*E+D*E, data=MyData)

coeff_list = sort(abs(model2$coefficients))
coeff_list = coeff_list[1:15]
quantile_list = matrix(0,1,15)

for(i in c(1:15)){
  quantile_list[i] = qnorm(0.5 + (i-0.5)/30)
}

plot(quantile_list, coeff_list)

model3 = lm(formula = y ~ A+B+E+A*E+A*D+A*B+C*E, data=MyData)
plot(model3)

