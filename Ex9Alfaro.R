#write a function that takes a number, and prints ‘small’ 
#if number less than or equal to -1; ‘medium’ if 
#between -1 and + 1’big’ if greater than or equal to + 1
 
num_In <- function(x){
  
  if (x<=(-1)){
    return("small")
  }
  else if (x>=1){
    return("big") 
  }
  else {
    return("medium")
  }  
}  


