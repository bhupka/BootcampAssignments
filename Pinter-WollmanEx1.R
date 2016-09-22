ChickWeightTime0 <- ChickWeight[ChickWeight$Time == 0,]
ChickWeightDiet0 <- ChickWeightTime0$Diet
time_diet_anova0 <- aov(ChickWeightTime0$weight~ChickWeightDiet0)

TukeyHSD(time_diet_anova0)

boxplot(weight~Diet, data = ChickWeightTime0, 
        main = "Weight:Diet Day 0", xlab = "Diet", ylab = "Weight", 
        col = (c("dodgerblue", "coral1", "darkgoldenrod1", "seagreen3")))

ChickWeightTime21 <- ChickWeight[ChickWeight$Time == 21,]
ChickWeightDiet21 <- ChickWeightTime21$Diet
time_diet_anova21 <- aov(ChickWeightTime21$weight~ChickWeightDiet21)

TukeyHSD(time_diet_anova21)

boxplot(weight~Diet, data = ChickWeightTime21, 
        main = "Weight:Diet Day 21", xlab = "Diet", ylab = "Weight",
        col = (c("maroon1", "lightseagreen", "lawngreen", "darkorange2")))

#ChickWeight <- within(ChickWeight, {
#  Chick <- factor(Chick)
#  Diet <- factor(Diet)
#  Time <- factor(Time)
#})

sumAOV <- summary(aov(weight ~ Diet*Time + Error(Chick), data = ChickWeight))

Diet1 <- ChickWeight[ChickWeight$Diet == 1,]
Diet2 <- ChickWeight[ChickWeight$Diet == 2,]
Diet3 <- ChickWeight[ChickWeight$Diet == 3,]
Diet4 <- ChickWeight[ChickWeight$Diet == 4,]

plot(Diet1$Time, Diet1$weight, col = c("aquamarine"), xlab = "Time", 
     ylab = "Weight", main = "The Effects of Diet on Chick Growth")
points(Diet2$Time, Diet2$weight, col = c("goldenrod1"))
points(Diet3$Time, Diet3$weight, col = c("chocolate1"))
points(Diet4$Time, Diet4$weight, col = c("darkseagreen"))

abline(lm(weight ~ Time, data = Diet1), col = c("aquamarine"))
abline(lm(weight ~ Time, data = Diet2), col = c("goldenrod1"))
abline(lm(weight ~ Time, data = Diet3), col = c("chocolate1"))
abline(lm(weight ~ Time, data = Diet4), col = c("darkseagreen"))

legend(0, 300, c("Diet 1", "Diet 2", "Diet 3", "Diet 4"), 
       col = c("aquamarine", "goldenrod1", "chocolate1", 
               "darkseagreen"), pch = 20, pt.cex = 2, bty = "n")

plot.new()

rainbow_indiv <- rainbow(50)
time_indiv <- ChickWeight$Time
weight_indiv <- ChickWeight$weight

plot(time_indiv, weight_indiv, 
     main = "Individual Growth by Color", xlab = "Time (days)",
     ylab = "Weight (g)")

for (i in chick_indiv){
  chick_indiv <- ChickWeight[ChickWeight$Chick == i, ]
  points(chick_indiv$Time, chick_indiv$weight, col = rainbow_indiv[i])
} 

