#### Exercise for estimation of intervals

#The population distribution is a standard normal.
# 1)Estimate the sampling distribution. Then, the 95% confidence interval for the mean of the sampling distribution.
# Estimate how many percent of the sample means does not fall into the sample distribution 
# confidence intervall.
# 2)Take another sample from the population. Estimate the 95% confidence interval for this
# sample and estimate how many samples does not fall into this bound.
# 3)Estimate the confidence interval of the mean for each sample and estimate how many
# does not have the true mean in it.


SAMPLE_SIZE = 1000

normalPopulationSample = c()
countForHavingPopulationMean = 0
N_SAMPLES =10000
for(i in 1:N_SAMPLES){
  s = rnorm(SAMPLE_SIZE)
  normalPopulationSample = c(normalPopulationSample,mean(s))
  if(0>= qnorm(0.025, mean =mean(s),sd =sqrt(var(s)/SAMPLE_SIZE))
     & 0<=qnorm(0.025, mean =mean(s),sd =sqrt(var(s)/SAMPLE_SIZE),lower.tail = F) ){
    countForHavingPopulationMean = countForHavingPopulationMean+1
  }

}
meanNotInConfBound = 1-countForHavingPopulationMean/N_SAMPLES
#should be 5%.... that the true population mean is within 95% of the samples
sprintf('population mean not in bound:%f',meanNotInConfBound)
hist(normalPopulationSample,freq = T,breaks = 1000)


##estimate sample distribtion bounds
sampleDistributionMean = mean(normalPopulationSample)
sampleDistributionVar =var(normalPopulationSample) 
failed_sampleDist = normalPopulationSample<qnorm(0.025, mean =sampleDistributionMean,sd =sqrt(sampleDistributionVar)  ) | normalPopulationSample>qnorm(0.025,mean =sampleDistributionMean,sd =sqrt(sampleDistributionVar),lower.tail = F)
#check how much percent failed to estimate sample mean within this bound
sum(failed_sampleDist)/length(failed_sampleDist)## 5% fail

##estimate a bound from a sample
populationSample = rnorm(SAMPLE_SIZE)
sampleMean = mean(populationSample)
sampleVar  = var(populationSample) 

failed_sample = normalPopulationSample<qnorm(0.025, mean =sampleMean,sd =sqrt(sampleVar/SAMPLE_SIZE)  ) | normalPopulationSample>qnorm(0.025,mean =sampleMean,sd =sqrt(sampleVar/SAMPLE_SIZE),lower.tail = F)
#The inverse does not apply. the repetition will not lead to mean within this boud, which kinda makes scense 
sum(failed_sample)/length(failed_sample)# 


# b)  
#   Consider the example from the slides again: 
#   A sample of n=15 drawn from a normal distributed population has a sample mean 35 and standard deviation 14
#   Construct a 95% confidence interval for the population mean. Compare to the estiamte of 
#   using a normal distribution.
n=15
mu =35
sigma=14
x = seq(20,50,0.01)

require("metRology")

plot(x,dt.scaled(x, n-1, mean = mu,sd = sigma/sqrt(n)) ,type='l',xlab = 'height',ylab='p(x)')
tleft = qt.scaled(0.025, n-1, mean = mu, sd = sigma/sqrt(n))
tright = qt.scaled(0.975, n-1, mean = mu, sd = sigma/sqrt(n),lower.tail = F)

segments(tleft, 0, tleft, dt.scaled(tleft , n-1,mean = mu ,sd=sigma/sqrt(n)),col='black')
segments(tright, 0, tright, dt.scaled(tright , n-1,mean = mu ,sd=sigma/sqrt(n)),col='black')

lines(x,dnorm(x ,mean = mu ,sd=sigma/sqrt(n)) ,type='l',col='red')
left= qnorm(0.025,mean =  mu ,sd=sigma/sqrt(n))
right= qnorm(0.025,mean =  mu ,sd=sigma/sqrt(n),lower.tail = F)

segments(left, 0, left, dnorm(left , mean = mu ,sd=sigma/sqrt(n)),col='red')
segments(right, 0, right, dnorm(right , mean = mu ,sd=sigma/sqrt(n)),col='red')

legend("topleft", legend=c("Estimate using the t-distribution", "Estimate using the normal distribution"),
       col=c("black", "red"), lty=1, cex=0.8)
sprintf("Estiamte for t-distribution:[%f,%f]",tleft,tright)
sprintf("Estiamte for normal distribution:[%f,%f]",left,right)

