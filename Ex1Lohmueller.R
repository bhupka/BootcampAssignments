get_Heights <- function(x){
  heights <- rnorm(x, mean = 69, sd = 10)
  return(mean(heights))
}

mean_heights_100 <- rep(0, 1000)
mean_heights_1000 <- rep(0, 1000)

for (i in 1:1000){
  mean_heights_100[i] <- get_Heights(100)
  mean_heights_1000[i] <- get_Heights(1000)
}

bins<-seq(65,73,by=0.5)
hist(mean_heights_100,breaks=bins)$breaks

hist(mean_heights_1000,breaks=bins)$breaks

count_100<-hist(mean_heights_100,breaks=bins)$counts
count_1000<-hist(mean_heights_1000,breaks=bins)$counts

pdf(file="Ex1_Lohmueller.pdf", width=6,height=6); 

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) 

  
barplot(rbind(count_100,count_1000),col=c(2,4),beside=T,names.arg=
          seq(65,72.5,by=0.5),xlab="Average Heights (inches)",ylab="Count")

legend(6,350,c("n = 100","n = 1000"),
       col=c(2,4),lwd=4)

dev.off() 

