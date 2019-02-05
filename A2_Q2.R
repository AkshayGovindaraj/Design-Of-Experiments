#Design of Experiments: Assignment 2 Question 2
trq_data = read.table("bolt.dat", header = TRUE)

#Preparing data
torque <- data.frame(trq_data$P.O, trq_data$C.W, trq_data$HT, trq_data$M.B)
trq_df <- torque[ which(torque$trq_data.M.B == 'M'), ]
trq_vector = c(trq_df$trq_data.P.O,trq_df$trq_data.C.W,trq_df$trq_data.HT)
type <- c(rep("P.O",10), rep("C.W",10), rep("HT",10))
new_df <- data.frame(trq_vector, type)

#ANOVA
model <- aov(trq_vector~type, data=new_df)
summary(model)

#Residual Plots
plot(model$fitted.values, model$residuals)

#Multiple Comparisons
pairwise.t.test(new_df$trq_vector, new_df$type, p.adj = "none")
