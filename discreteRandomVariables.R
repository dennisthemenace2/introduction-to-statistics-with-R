#Exercises for discrete random variables


#Task a)

#Simulate a random experiment by tossing 10 independent coins.
#Let X be the random variable of the sum of the heads
#Plot the estimated probability mass function of X


##generate random coin throws...
N = 1000 ## repetition of the experiment
K = 10 ## number of coins

results = c() 
for( i in 1:N){
  results = c(results, sum( as.numeric(runif(K)>0.5) ) ) ### because you can intrepte true and false as 1/0
}

hist(results,freq = F,main='simulated probability mass function')

# b) add a marker for the true probabilities to the plot

successes = 0:10
prob = dbinom( x = successes,prob = 0.5,size = 10)
points(successes,prob,col='red')

##just a plot of the pdf
barplot(prob,names.arg = 0:10)

#c) calculate the expected value and variance of the simulated pmf
# For checking the results:
# expected value = n*p
# variance = n*p(1-p)

expected_value = 0
for(i in 0:10){
  expected_value = expected_value+ i*(sum(results==i)/N)
}

variance = 0
for(i in 0:10){
  variance = variance+ (i-expected_value)^2 *(sum(results==i)/N)
}

#d) plot the cumulative distribution function (CDF) for this experiment, using the pbinom function

plot(0:10,pbinom(q=0:10,size=10,prob=0.5),ylab=("P(X<=x)"),xlab="Number of successes")
##or this one looks nicer
barplot(pbinom(q=0:10,size=10,prob=0.5),names.arg = 0:10)

#e) estimate the probability P(X>=6) that at least 6 or more coins show heads.
#   use the simulated density for the culclation and use the pbinom function
#   to estimtate this probability 

##sum over the probabilities
simulated_pbe = 0
for(i in 6:10){
  simulated_pbe = simulated_pbe+sum(results==i)/N
}
sprintf("P(X>6) from the simulation:%f",simulated_pbe )

##  using the cummulative density function.
true_pbe = pbinom(5,size=10,prob = 0.5,lower.tail = F)
sprintf("true P(X>6):%f",true_pbe )



