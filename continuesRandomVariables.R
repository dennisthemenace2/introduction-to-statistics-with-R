## Exercises for continues random variables


## Tasks

#a) consider again the bus stop example from the slides:
#Example:
#A man arrives at a bus stop at a random time (that is, with no regard for the scheduled service) to
#catch the next bus. Buses run every 30 minutes without fail, hence the next bus will come any time
#during the next 30 minutes with evenly distributed probability (a uniform distribution). Find the
#probability that a bus will come within the next 10 minutes.

# Estimate the probability P(X>20) with simulation and estimate the exact probability
# using the R cumulative probability function for uniform distributions punif

##bus stop simulated
timetoleave = runif(10000,0,30)
sum( timetoleave<=10 ) / length(timetoleave)

#Exact probability using punif
1-punif(20,0,30,lower.tail = T)


#b) Plot the pdf of a normal distribution.
#   Use variables that allow it to easily change the used mean and variance
#   Further, indicate location of mu+-sigma, mu+-2sigma, mu+-3sigma in the plot
#   Finally, calculate the probability P(mu-sigma<X<mu+sigma).
mu = 0
var = 1
#plot the pdf
plot(seq(-5,5,0.1),dnorm(seq(-5,5,0.1),mean = mu,sd = sqrt(var)),type = 'l')

#indicate mu+-sigma
for(i in 1:3){
  abline( v=(mu-sqrt(var)*i),col='red' )
  abline( v=(mu+sqrt(var)*i),col='red' )
}

# P(mu-sigma<X<mu+sigma)
probBetween2std = 1- (2*pnorm(mu-sqrt(var),mean=mu,sd=sqrt(var)))


## c) Plot the pdf, cdf, and quantile function of the standard normal distribution
#     How can you use the uniform distribution and quantile function to generate 
#     random samples from any pdf. Use random samples from the uniform distribution and the
#     quantile function to generate samples from a standard normal distribution.

##plot functions
curve(dnorm,xlim=c(-5,5),main='pdf')
curve(pnorm,xlim=c(-5,5),main='cdf')
curve(qnorm,main='quantile function')

#create random samples
hist(qnorm(runif(10000)),freq = F,xlab='x',breaks = 1000)
curve(dnorm,add = T,col='red')

