#Design of Experiments: Assignment 2 Question 3
bp_data = read.table("BloodPressure.dat.txt", header = TRUE)

#Preparing data
reading_device <- c(bp_data$Dev1, bp_data$Dev2, bp_data$Dev3)
reading_doctor <- c(bp_data$Doc1, bp_data$Doc2, bp_data$Doc3)
device <- c(rep("Dev1",15), rep("Dev2",15), rep("Dev3",15))
dim(device) = c(45,1)
doctor <- c(rep("Doc1",15), rep("Doc2",15), rep("Doc3",15))
dim(doctor) = c(45,1)
new_df <- data.frame(reading_device,reading_doctor,device,doctor)

#ANOVA
model_device <- aov(reading_device~device, data=new_df)
summary(model_device)

model_doctor <- aov(reading_doctor~doctor, data=new_df)
summary(model_doctor)

#Add Residual Plot
par(mfrow=c(1,2))
plot(model_device$fitted.values, model_device$residuals, main = "Devices")
plot(model_doctor$fitted.values, model_doctor$residuals, main = "Doctor")


#Confidence Interval
t = qt(0.975,2)      # t-value for 2 dof
mean_device = mean(new_df$reading_device)
mean_doctor = mean(new_df$reading_doctor)
mstr_device = 0.01
mstr_doctor = 248.16

lb_dc = mean_doctor - t*sqrt(mstr_doctor/45)
lb_dv = mean_device - t*sqrt(mstr_device/45)
ub_dc = mean_doctor + t*sqrt(mstr_doctor/45)
ub_dv = mean_device + t*sqrt(mstr_device/45)
