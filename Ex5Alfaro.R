bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);

#Now look at the error message the following lines of code produce. 
#Can you think of a way to modify this loop so that the loop will compund the interest?
compounded <- rep(0,6)
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }
print(compounded)

#HINT: variables must be initialized before you can perform operations on them
#HINT 2: look at the rep() function and see if you can use that to initialize a 
#variable that will help you.