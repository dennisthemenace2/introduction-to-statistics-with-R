#### Exercise for sampling distribution

#a) Consider the example for the estimation of the sample distribution in the slides again.
#   Implement the example. Also estimate the sample distribution with simulation.
#   If you can for different population and sample sizes.

##Define parameters of the example
POPULATION_SIZE =3
SAMPLE_SIZE =2
N = 1000

##create population
createPopulation = function(POPULATION_SIZE){
  population = c(1:POPULATION_SIZE)
}

population = createPopulation(POPULATION_SIZE)

library(gtools)

####estimate population mean and var
estimatePopulationParamters = function(population,SAMPLE_SIZE){
  permutations= permutations(length(population), SAMPLE_SIZE, set = F,population, repeats.allowed=T)
  samples_means = rowMeans(permutations)
  population_mean = mean( samples_means)
  population_var =  sum( (population-population_mean)^2 )/length(population)
  list('population_mean'=population_mean,'population_var'=population_var)
}

populationParamters = estimatePopulationParamters(population,SAMPLE_SIZE)
sprintf("Population mean:%f",populationParamters$population_mean)
sprintf("Population var:%f",populationParamters$population_var)

##############


##draws N samples from the population and calculates the mean of each sample
drawSamples=function(population,SAMPLE_SIZE,N){
  result = c()
  for(i in 1:N){
    result = c(result,mean(sample(population,size = SAMPLE_SIZE, replace = T)))
  }
  result
}

sampleMeans  = drawSamples(population,SAMPLE_SIZE,N)
sampleDistribution_mean = mean(sampleMeans)
sampleDistribution_var = var(sampleMeans)

sprintf("Sample distribution mean:%f",sampleDistribution_mean)
sprintf("Sample distribution var:%f",sampleDistribution_var)

sprintf("Population mean:%f",populationParamters$population_mean)
sprintf("Population var:%f",populationParamters$population_var)


#b)
# The central limit theorem tells us, that with an increase in sample size,
# the sample distribution will become increasingly normal centered around the population mean
# with a variance of the populations variance/sample size

# Simulate the sample distribution for an increasing sample size and notice how it becomes
# increasingly normal.

POPULATION_SIZE =100
SAMPLE_SIZE =30
N = 10000

population = createPopulation(POPULATION_SIZE)

sampleMeans  = drawSamples(population,SAMPLE_SIZE,N)

hist(sampleMeans,freq = F,breaks = 1000)


#c) So far, we only used populations that were uniform distributed, but the central limit
#   theorem tells us that the population distribution can be any distribution.
#   Make up an distribution as you please, and estimate their sample distribution
#   with simulation and see if this is indeed normal.

my_distribution = c(3,10,4,0,0,1,0,9,6,1,1,8)
my_distribution = my_distribution/ length(my_distribution) ##has to sum to one
##show
barplot(my_distribution,names.arg = 1:length(my_distribution),main = "Population distribution")

my_distributionSampleMeans= c()

for(i in 1:10000){
  mySample= sample( x = 1:length(my_distribution),size = 30,replace = T,prob =my_distribution )
  my_distributionSampleMeans = c(my_distributionSampleMeans, mean(mySample ) )
}

hist(my_distributionSampleMeans,freq = F,breaks = 1000)


#d) 
# If the population is normal to begin with then the sample mean also has a normal
# distribution, regardless of the sample size. Estimate the sample distribution from a
# standard normal distributed population with a sample size of 2 and plot the sample distribution.

normalPopulationSample = c()
for(i in 1:10000){
  normalPopulationSample = c(normalPopulationSample,mean(rnorm(2)))
}
## Well, its apparently normal, maybe
hist(normalPopulationSample,freq = F,breaks = 1000)

curve(dnorm,col='red',add = T)


