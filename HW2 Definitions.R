

# Prior distribution

trueA <- 5 # actual slope term to generate y 
trueB <- 0 # actual intercept term to generate y
trueSd <- 10 # actual SD used for error term
sampleSize <- 31 #actual sample size used to create length(x)

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2) #uncorrelated xs created, author states this 
#-1/2 to +1/2 technique prevents correlation between the slope and the intercept

# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd) #linear y, dependent on x + normal error


proposalfunction <- function(param){ #proposalfunction which gets our mystery distribution
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3))) #generates a vector of length 3, containing
  # a vector corresponding to normal sampling with three different values of the sd
  # this will be the unknown function. Param allows a user inputed mean
  
}
likelihood <- function(param){ #this function requires us to enter a vector corresponding to 
  #what we think our parameters may be, or what the markov chain has updated them too
  a = param[1] #tentative a = 1st term in parameter
  b = param[2] #tentative b = 2nd term in parameter
  sd = param[3] #tentative c = 3rd term in parameter
  
  pred = a*x + b #prediction uses the input parameters to estimate what y hat would be if our param 
  #is correct
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) #between the y actual outputs
  #and the prediction, this returns the probabilities of observing a vector y given that we have the
  #correct parameters. This also uses log scale.
  
  sumll = sum(singlelikelihoods) #sums the individual liklihoods for each y_i and pred_i
  return(sumll)   #returns the previous line's sum
}

prior <- function(param){ #creates prior distributions for a,b,sd
  a = param[1] # a = first parameter in param,
  b = param[2] # b = second parameter in param,
  sd = param[3] # c = third parameter in param,
  aprior = dunif(a, min=0, max=10, log = T) #gives the probability of getting "a" from a 
  #uniform distribution with min = 0 and max = 10
  bprior = dnorm(b, sd = 5, log = T) #gives probability of getting b from a normal
  #distribution with mean = 0, sd = 5
  sdprior = dunif(sd, min=0, max=30, log = T) #gives probability of getting sd from uniform distribution
  # with min = 0 and max =30
  #logs  = TRUE since we have decided to work with them
  return(aprior+bprior+sdprior) #returns sum of the individual probabilities which is ok since we're using
  #logs
}




posterior <- function(param){ # returns posterior distribution
  return (likelihood(param) + prior(param)) #sums the liklihood of getting the y vector given a certain
  # along with the probability of getting that distribution. This is a sum because we are working with logs, mean this is roughly equivalent
  #to likelihood*prior
}

run_metropolis_MCMC <- function(startvalue, iterations){ #this function will update the markov chain
  #start value indicates starting value for chain and iterations is the length of the for loop
  chain = array(dim = c(iterations+1,3)) #creates an array to accomodate starting value plus interations
  # for a, b, and sd
  chain[1,] = startvalue #plugs starting values into chain array first row
  for (i in 1:iterations){ #for loop aimed at iterations that will update the chain
    proposal = proposalfunction(chain[i,]) #feeds first row into proposalfunction, producing a 
    #vector of length 3, where each value is dependent on the value drawn from
    #chain and the preprogrammed sd. This vector is sotred as proposal
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) #again, the exp is due to the nature of workig
    #with logs and is thus connected to posterior(proposal)/posterior(chain). This represents the
    #ratio of whether the proposal is more positive than the current, which indicates whether or not
    # we are moving into areas of increasing probability
    if (runif(1) < probab){ #if our random uniform sample is less than probability we accept it and 
      chain[i+1,] = proposal#add it to the chain
    }else{ # alternatively, if the above condition is not true, we reject it.
      chain[i+1,] = chain[i,] #and duplicate the current value back into the chain
    }
  }
  return(chain) #when this process finishes for the number of iterations it returns the chain
  
  
}


### Summary: #######################
summary_plots <- function(burnIn, chain, y, x){
  
  par(mfrow = c(2,3)) #plots the below graphs in a 2x3 matrix of plots
  hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" )
  #the above line plots a histogram of the posterior of a, without the burnIn
  abline(v = mean(chain[-(1:burnIn),1])) #creates a line at the mean of the markov chain for a
  #without the burnIN
  abline(v = trueA, col="red" ) #creates a line at the true a value
  
  hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
  #the above line plots a histogram of the posterior of b, without the burnIn
  
  abline(v = mean(chain[-(1:burnIn),2])) #creates a line at the mean of the markov chain for b
  #without the burnIN
  
  abline(v = trueB, col="red" ) #creates a line at the true b value
  
  hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
  #the above line plots a histogram of the posterior of sd, without the burnIn
  
  abline(v = mean(chain[-(1:burnIn),3]) ) #creates a line at the mean of the markov chain for sd
  #without the burnIN
  
  abline(v = trueSd, col="red" ) #creates a line at the true sd value
  
  plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a", )
  #plots the trend of how the Markov chain progressed for a
  
  abline(h = trueA, col="red" ) #creates a line at the true value of a
  
  plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", )
  #plots the trend of how the Markov chain progressed for b
  
  abline(h = trueB, col="red" ) #creates a line at the true value of b
  
  plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd", )
  #plots the trend of how the Markov chain progressed for sd
  
  abline(h = trueSd, col="red" ) #creates a line at the true value of sd
  
  # for comparison:
  summary(lm(y~x)) #prints summary of linear model of y dependent on x
}