#DOE HW3
bp_data = read.table("bloodpressure1.dat.txt", header = TRUE)

model1 <- lm(decreaseA ~ initialA, data=bp_data)
summary(model1)

model2 <- lm(decreaseB ~ initialB, data=bp_data)
summary(model2)
