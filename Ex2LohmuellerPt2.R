setwd("/Users/BHupka/Documents/UCLA/Classes/201 Bootcamp/Bootcamp Work")
zz <- read.table("pheno.sim.2014-1.txt", header = T)
paste("The bottom quartile of the phenotype is:", round(quantile(zz$glucose_mmolperL, 0.25), digits = 3))
paste("The 3rd quartile of the phenotype is:", round(quantile(zz$glucose_mmolperL, 0.75), digits = 3))

quant_line_1 <- round(quantile(zz$glucose_mmolperL, 0.25), digits = 3)
quant_line_2 <- round(quantile(zz$glucose_mmolperL, 0.75), digits = 3)

plot(density(zz$glucose_mmolperL), main = "Dist of phenotypes")

abline(v=quant_line_1, col = "red", lty = 2)
abline(v=quant_line_2, col = "blue", lty = 2)