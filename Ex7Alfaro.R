bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

compounded <- bankAccounts

for (j in 2015:2020) {
  for (i in 1:length(bankAccounts)) {
    compounded[i] <- compounded[i] - house[i] - food[i] - fun[i] + income[i]
    compounded[i] <- compounded[i] * interestRate + compounded[i] 
  }
  if (j%%2==1){
    compounded[1] <- compounded[1]+(5.0)
    compounded[3] <- compounded[3]+(5.0)
  }
}

print(compounded)

