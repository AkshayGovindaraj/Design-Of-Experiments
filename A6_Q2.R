#Design of Experiments: Assignment 6 Question 2
library(gplots)
coredrill = read.table("CoreDrill", header = FALSE)

par(mfrow=c(3,2))
plotmeans(V11 ~ V7, data = coredrill, frame = FALSE)
mtext("Main Effect for OD Force", side = 3, line = -3, outer = TRUE)

interaction.plot(x.factor     = coredrill$V6,
                                   trace.factor = coredrill$V7, 
                                   response     = coredrill$V11, 
                                   fun = mean,
                                   type="b",
                                   col=c("black","red","green"),  ### Colors for levels of trace var.
                                   pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                                   fixed=TRUE,                    ### Order by factor order in data
                                   leg.bty = "o")

mtext("Interation Effect for OD Force", side = 3, line = -3, outer = TRUE)

model = lm(V11 ~ V6+V7+I(V5*V6)+I(V2*V6)+I(V2*V5)+I(V3*V7)+I(V4*V7)+I(V6*V7), data=coredrill)
anova(model)

v10.model = lm(V11 ~ V2*V3 + V2*V4 + V2*V5 + V2*V6 + V2*V7 + V3*V4 + V3*V5 + V3*V6 + V3*V7 + V4*V5 + V4*V6 + V4*V7 + V5*V6 + V5*V7 + V6*V7,data=coredrill)
summary(v10.model)
