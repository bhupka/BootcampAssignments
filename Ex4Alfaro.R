###you can use the if statement with the modulus operator to conditionally perform operations
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  if(years[ii] %% 4 == 0){
    cat(years[ii], "Hooray, presidential elections!", sep = "\t", fill = T)
  }  
  }
  
}