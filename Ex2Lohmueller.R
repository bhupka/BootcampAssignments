setwd("/Users/BHupka/Documents/UCLA/Classes/201 Bootcamp/Bootcamp Work")
snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)

# What are the dimensions of the data?
dim(snpsDataFrame)


# Let's look at the top (or head) of the data set:
head(snpsDataFrame)

# Note:  If A is the more common allele and B is the more rare allele (i.e. major and minor alleles, repsectively)
#        Then, AA= 0,  AB= 1, BB = 2


# What are the column names? 
names(snpsDataFrame)

# What are the row names? 
row.names(snpsDataFrame)

# Because the data are really just a large numeric matrix, we convert the dataframe to a matrix:
snps=as.matrix(snpsDataFrame)

################### LOOKING CLOSELY AT ONE SNP #######################

# With row names we can easily extract certain SNPs using the id's
testSNP=snps["rs218206_G",]

table(testSNP)

# What is proportion of heterozygotes at this locus?
het=sum(testSNP==1)/length(testSNP)

# What if there is missing data?
testSNP=snps["rs6717613_A",]

# Try these commands
table(testSNP)
testSNP==1
length(testSNP)
is.na(testSNP)

# Now let's compute the observed heterozygosity
het=sum(testSNP==1)/length(testSNP)  # Note how this fails
het=sum(testSNP==1,na.rm=TRUE)/sum(!is.na(testSNP))  # but this doesn't 


###### EXPLORATORY PLOT OF SNP ALLELE FREQUENCY VS. OBSERVED HETEROZYGOSITY #####

# To inspect the data, let's compute the frequency of each SNP and compare it 
# to the observed heterozygosity (i.e. the proportion of individuals who are heterozygotes)

# What is the frequency of the minor allele?
freq=sum(testSNP,na.rm=TRUE)/(2.0*sum(!is.na(testSNP)))

# Now, let's define functions that do this for a generic set of SNP data
calc_freq=function(x){
  return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
  return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}

# And now let's apply the functions to each and every SNP
freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)

# And now we can make exploratory plots
#plot(freq,het,xlab="Frequency",ylab="Heterozygosity")  # Scatter plot

# Let's add a line to show what relationship we'd expect under Hardy-Weinberg expectations
p=seq(0,0.5,by=0.05)   # Set-up a vector with a sequence of allele frequencies
points(p,2*p*(1-p),type="l",col=2) # Plot the HW expectation as a line in red

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  #here we use the built-in function for the chi-sq distribution:
  exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
  chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
  return(chisq)
}

# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

#check to see that the chisquare statistcs are the same:
#first do this by computing Pearson's correlation coefficient:
cor.test(chisqs,chisqs2)

#we can also do a quick scatterplot:

#plot(chisqs,chisqs2)
pvals <- pchisq(chisqs,1,lower.tail=FALSE)
num_pval <- sum(length(pvals))
paste("Proportion of pvals under 0.05:", round(sum((pvals < 0.05)/num_pval), digits = 3))
paste("Proportion of pvals under 0.01:", round(sum((pvals < 0.01)/num_pval), digits = 3))
paste("Proportion of pvals under 0.001:", round(sum((pvals < 0.001)/num_pval), digits = 3))

print(num_pval)

exp_pvals <- rep(0, num_pval) 
  
for (i in 1:num_pval){
  exp_pvals[i] <- (i/num_pval)
}
sort_pvals <- sort(pvals)

log_sort_pvals <- -log10(sort_pvals)
log_exp_pvals <- -log10(exp_pvals)


quartz()
max_pval <- max(log_sort_pvals, log_exp_pvals)

plot(log_sort_pvals, log_exp_pvals,
     xlab = "−log10(expected P−value)",
     ylab = "−log10(observed P−value)",
     xlim = c(0, max_pval),
     ylim = c(0, max_pval)
     )

abline(0,1, col = "dodgerblue", lty = 4) 


