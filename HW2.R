######## Metropolis algorithm ################
Metropolis <- function(param = c(1,2,3)){
  burnIn = 1000
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3))) #generates a vector of length 3, containing
  # a vector corresponding to normal sampling with three different values of the sd
  # this will be the unknown function. Param allows a user inputed mean
  
}

# Prior distribution

trueA <- 5 # this whole section just creates some independent xs and then corresponding linear ys
trueB <- 0 #the true terms set the values of the necessary terms for the distribution
trueSd <- 10
sampleSize <- 31 

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2) #uncorrelated xs
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd) #linear + noise

likelihood <- function(param){ #this function requires us to enter a vector corresponding
  a = param[1] #to what we think a, b, and sd are
  b = param[2]
  sd = param[3]
  
  pred = a*x + b #prediction uses the input parameters to estimate what y hat would be if our param 
  #is correct
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) #between the y actual outputs
  #and the prediction, on a log scale, this returns the probabilities of observing a vector y
  #given that we have the correct model
  sumll = sum(singlelikelihoods) #sums the above
  return(sumll)   
}
prior <- function(param){ #creates prior distributions for a,b,sd
  a = param[1] #takes inputs from our guess
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T) #gives the density of getting a from a unif distribution
  bprior = dnorm(b, sd = 5, log = T) #gives density of getting b from norm dist mean = 0
  sdprior = dunif(sd, min=0, max=30, log = T) #gives density of getting sd from uniform dist
  return(aprior+bprior+sdprior) #returns sum of above
}




posterior <- function(param){ # returns posterior distribution
  return (likelihood(param) + prior(param)) #sums the liklihood of getting the y vector given a certain
  #given a distribution. This is a sum because we are working with logs, mean this is roughly equivalent
  #to likelihood*prior
}

run_metropolis_MCMC <- function(startvalue, iterations){ #start value indicates starting value for chain
  chain = array(dim = c(iterations+1,3)) #creates an array to accomodate starting value plus interations
  # for each of the different values from previous function?
  chain[1,] = startvalue #plugs starting value into chain array first row, all columns
  for (i in 1:iterations){ #for loop aimed at iterations
    proposal = proposalfunction(chain[i,]) #feeds first row into proposalfunction, producing a 
    #vector of length 3, where each value is dependent on the value drawn from
    #chain and the preprogrammed sd gets a proposal value
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) #again, the exp is due to the nature of workig
    #with logs and is thus connected to posterior(proposal)/posterior(chain)
    if (runif(1) < probab){ #if our random uniform sample is less than probability we accept it and 
      chain[i+1,] = proposal#add it to the chain
    }else{ # we reject it and duplicate the previous entry for chain into the next
      chain[i+1,] = chain[i,]
    }
  }
  return(chain) #when this process finishes for the number of iterations it returns the chain
  
  
}

startvalue = c(4,0,10) 
chain = run_metropolis_MCMC(startvalue, 10000) #this runs the above code with given values

burnIn = 5000 
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #acceptance rate is 1 - mean of the number of 
#duplicates





### Summary: #######################

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))
}