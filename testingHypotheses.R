#### Exercise for hypothesis tests


#a) Consider the pain reliever example again.
# Implement it using the normalized and unnormalized test statistic.


###pain reliever
mu_0= 3.5
sigma_0 = 2.1

sample_mean =3.1
sample_sigma = 1.5

N = 50
alpha = 0.05


#### without normalizing of the test statistic
x = seq(-2,10,0.01)
plot(x,dnorm(x,mean= mu_0, sd=sigma_0 ),type='l',xlab='Time',ylab="P(x)")
segments(mu_0,0,mu_0,dnorm(mu_0,mean= mu_0, sd=sigma_0 ))

lines(x,dnorm(x,mean= sample_mean, sd= sample_sigma/sqrt(N) ),col='red')

critical = qnorm(alpha,mean= sample_mean, sd= sample_sigma/sqrt(N),lower.tail = F)
segments(critical,0,critical,dnorm(critical,mean= sample_mean, sd= sample_sigma/sqrt(N)),col='red')

##estimate p value
p_value = pnorm(mu_0,mean= sample_mean, sd= sample_sigma/sqrt(N),lower.tail = F)

legend("topright", legend=c("Original pain reliever population distribution", "New pain reliever sample distribution"),
       col=c("black", "red"), lty=1, cex=0.8)

# With creating a normalized test statistics
Z = (sample_mean-mu_0) /( sample_sigma/sqrt(N))
C = qnorm(alpha )
conf = (sample_mean-mu_0) -qnorm(c(0.05,0.95),mean=(sample_mean-mu_0),sd=( sample_sigma/sqrt(N)))

curve(dnorm,xlim = c(-5,5))
abline(v=C)
points(Z,0,col='red')
###test test
pnorm(Z)##p-value

lines(seq(-4,4,0.01), dt(seq(-4,4,0.01),df=N-1),col='red')
t = (sample_mean-mu_0) /( sample_sigma/sqrt(N))
pt(t,df = N-1)


#b) Consider the exmaple from the slides:
#   The price of a popular tennis racket at a national chain store is $179.
#   Portia bought five of the same racket at an online auction site for the
#   following prices:{155, 179, 175, 175, 161}. Assuming that the auction prices of
#   rackets are normally distributed, determine whether there is sufficient evidence in the sample, at the 5% level of significance, to conclude that the average price of the racket is less than $179 if purchased at an online auction
#   Do the test in R and estimate the p-value

###with t distribution
mu_0 = 179
sample_data = c(155,179,175,175,161)
mean(sample_data)
sqrt(var(sample_data))
T_statistic = (mean(sample_data)-mu_0 ) /  (sqrt(var(sample_data)) / sqrt(length(sample_data)) )

C= qt(0.05,df = length(sample_data)-1)
curve(dt(x ,df=length(sample_data)-1) ,xlim = c(-5,5))
segments(C,0,C,dt(x = C,df = length(sample_data)-1))
points(T_statistic,0,col='red')
pvalue = pt(T_statistic ,df = length(sample_data)-1)

