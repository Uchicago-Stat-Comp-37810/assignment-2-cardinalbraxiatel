######## Metropolis algorithm ################
Metropolis <- function(param = c(1,2,3)){ #my function so I can run this whole thing from the command line


startvalue = c(4,0,10) #we now run a simulation, using these as our starting values

source(file = "C:\\Users\\Patrick's Computer\\Documents\\GitHub\\assignment-2-cardinalbraxiatel\\HW2 Definitions.R")

chain = run_metropolis_MCMC(startvalue, 10000) #this runs the above function with appropriate
# values

burnIn = 5000 # the burn in period removes the first 5000 entries which are probably heavily
#biased by the starting values
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #acceptance rate is 1 - mean of the number of 
#duplicates

summary_plots(burnIn, chain, y, x)
}