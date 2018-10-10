#### Exercise for two sample problems


#a) Consider the example with two companies again.
#   To compare customer satisfaction levels of two competing cable television companies, 174 customers of Company 1 and 355 customers of Company 2 were randomly selected and were asked to rate their cable companies on a five-point scale, with 1 being least satisfied and 5 most satisfied.
#   The survey results are summarized in the following table:
n1 =174 # number of samples company 1
n2 =355 # number of samples company 2
x1 =3.51 # sample mean company 1
x2 =3.24 # sample mean company 2
s1 =0.51 # sample standard deviation for company1
s2 =0.52 # sample standard deviation for company2
#Construct a point estimate and a 99% confidence interval for μ1 −μ2, the difference in average satisfaction levels of customers of the two companies as measured on this five-point scale.

diff = x1-x2
diffs = sqrt(s1^2/n1 + s2^2/n2)

x = seq(0,0.5,0.01) 
plot(x,dnorm(x,mean= diff, sd=diffs ),type='l',xlab='Difference in satisfaction',ylab="P(x1-x2)") 

left =qnorm(0.005,diff,sd=diffs)
right = qnorm(0.005,diff,sd=diffs,lower.tail = F)
segments(left,0,left,dnorm(left,mean= diff, sd=diffs),col='red')
segments(right,0,right,dnorm(right,mean= diff, sd=diffs),col='red')

# b) Continue with the example from the slides.
#    Does company1 has a higher mean satisfaction? At 1% significance
#    Estimate with the standardized sample statistic and unstandardized.

##create standerdized test statistic
Z = diff/diffs
C = qnorm(0.01,lower.tail = F)
if(Z>C){
  print("Reject H0, and Ha is accepted")
}else{
  print("H0 is not rejected")
}
pvalue = pnorm(Z,lower.tail = F)
sprintf("The p-value is=%f",pvalue)

##unstandardized
x = seq(-0.2,0.5,0.01) 

plot(x,dnorm(x,mean= diff, sd=diffs ),type='l',xlab='Difference in satisfaction',ylab="P(x1-x2)") 
pvalueUnstanderdized= pnorm(0,diff,diffs)

left = qnorm(0.01,diff,diffs)
segments(left,0,left,dnorm(left,diff,diffs),col='red')
if(pvalue!=pvalueUnstanderdized){
  print('error p-values have to be identical')
}
sprintf("p-value standerdized:%f, p-value unstandardized:%f",pvalue,pvalueUnstanderdized)


# c) consider the cars example from the slides again.
#   Suppose chemical engineers wish to compare the fuel economy obtained by two different
#   formulations of gasoline. Since fuel economy varies widely from car to car, if the mean fuel economy
#   of two independent samples of vehicles run on the two types of fuel were compared, even if one
#   formulation were better than the other the large variability from vehicle to vehicle might make any
#   difference arising from difference in fuel difficult to detect. 
#   The cars of the same make and model and driven under similar
#   circumstances. Here the collected sample from each car.
car1 = c(17.0 ,13.2 ,35.3,13.6, 32.7 ,18.4, 22.5, 26.8 ,15.1 )
car2 = c(17.0, 12.9, 35.4,  13.2, 32.5 ,18.1, 22.5, 26.7, 15.0 )
#   Test if the mean fuel economy of fuel 1 is greater at a 5% level of significance. 

T_val = mean(car1-car2) 
t_var = ( sqrt(var(car1-car2))/sqrt(length(car1) ) )

cv = qt(c(0.05), df= length(car1)-1,lower.tail = F)  

convInterval = T_val - cv*t_var

pvalue = pt(T_val/t_var,df = length(car1)-1,lower.tail = F)
sprintf("p-value:%f",pvalue)

plot(seq(-10,10,0.01),dt(seq(-10,10,0.01),length(car1)-1),type ='l' )
segments(cv,0,cv,dt(cv,length(car1)-1),col='red')
points(T_val/ t_var,0,col='green')

##compare with R results.
t.test(car1,car2,paired = T,alternative = "greater")
