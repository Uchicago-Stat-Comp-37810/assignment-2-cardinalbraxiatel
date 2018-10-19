compare_outcomes <- function(iterations, burnIn){
  
  if(burnIn > iterations){return("burnIN can't be longer than iterations")}
  #the above line was added to prevent errors of non-finite length vectors
  source(file = "C:\\Users\\Patrick's Computer\\Documents\\GitHub\\assignment-2-cardinalbraxiatel\\HW2.R")
  
  a_vals <- matrix(nrow = 10, ncol = 2)
  
  for(i in 1:10){
  a_int <- runif(1)
  b_int <- runif(1)
  sd_int <- runif(1)
  
  startvalue <- c(a_int,b_int,sd_int)
  
  chain_final = (Metropolis(iterations, burnIn ))
  
 
  chain_final <- chain_final[-burnIn,]
    mean_a <- mean(chain_final[,1])
  std <- sd(chain_final[,1])
  a_vals[i,1] <- mean_a
  a_vals[i,2] <- std
  print(a_vals[i,])
  
  
  }
  
}