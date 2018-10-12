#### Exercise for hypothesis tests


#a) implement the chi2 test for the heart rate example in the slides.
babyHeartRate = matrix(c(11,7,
                         17,5),ncol=2,byrow = T)
babyHeartRate = as.data.frame(babyHeartRate)

calculateChiStat=function(mat){
  rs = rowSums(mat)
  cs = colSums(mat)
  n = sum(cs)
  E =rs%*%t(cs)/n
  sum( (mat-E)^2 / E )
}

E = calculateChiStat( babyHeartRate)

x = seq(0,7,0.01)
df = (nrow(babyHeartRate) -1) * (ncol(babyHeartRate) -1) 
plot(x, dchisq(x,df),type = 'l' )
right = qchisq(0.01,df,lower.tail = F)
abline(v= right)
points(E,0,col='red')
pchisq(E,df,lower.tail = F)



# a) Consider the example from the slides again and implement the F-test in R
#  2 types of Blood test stripes for glucose readings. Is their variance different at a 10% level of significance ? 
#  And implement if Type B has a lower variance than Type A at a 10% level of significance.
#  Type A: 16 samples, s1² = 2.09
#  Type B: 21 samples, s2² = 1.10

s2_1 = 2.09
s2_2 =1.1

n1=16
n2=21

F_stat = s2_1/s2_2

left = qf(c(0.05),n1-1, n2-1, lower.tail = TRUE)
right = qf(c(0.05),n1-1, n2-1, lower.tail = F)

p1 =pf(F_stat, n1-1, n2-1, lower.tail = TRUE)
p2 = pf(F_stat, n1-1, n2-1, lower.tail = FALSE)

p_value = min(p1,p2)

x  = seq(0,5,0.01)
plot(x,df(x, n1-1, n2-1),type = 'l')
points(F_stat,0,col='red')
abline(v=left)
abline(v=right)


##right 10%

right = qf(c(0.15),n1-1, n2-1, lower.tail = F)

plot(x,df(x, n1-1, n2-1),type = 'l')
points(F_stat,0,col='green')
abline(v=left)
abline(v=right)


# b) Consdier the simple one way ANOVA again and implement the test in R.


######## anova

GPAscores = matrix(c(
  2.59, 3.64, 4.00, 2.78,
  3.13, 3.19, 3.59, 3.51,
  2.97, 3.15, 2.80, 2.65,
  2.50, 3.78, 2.39, 3.16,
  2.53, 3.03, 3.47, 2.94,
  3.29, 2.61, 3.59, 2.32,
  2.53, 3.20, 3.74, 2.58,
  3.17, 3.30, 3.77, 3.21,
  2.70, 3.54, 3.13, 3.23,
  3.88, 3.25, 3.00, 3.57,
  2.64, 4.00, 3.47, 3.22),ncol=4,byrow = T)


GPAscores =as.data.frame(GPAscores)
colnames(GPAscores) = c("Mathematics", "English", "Education", "Biology")

tmp=c()
for(i in 1:ncol(GPAscores)){
  tmp = c(tmp, GPAscores[,i] )  
}

GPAscoresFactor = data.frame('Class'= c(rep("Mathematics",nrow(GPAscores)),
                                        rep("English",nrow(GPAscores)),
                                        rep("Education",nrow(GPAscores)),
                                        rep("Biology",nrow(GPAscores))))

GPAscoresFactor = as.data.frame(GPAscoresFactor)
GPAscoresFactor$Class = as.factor(GPAscoresFactor$Class)
GPAscoresFactor = cbind(GPAscoresFactor, 'score'=tmp)

##assumes first to be id
calculateFratio= function(mat){
  
  meanall = mean(mat[,2])
  
  mst = 0
  mse = 0
  means = c()
  ids = unique(mat[,1])
  for(i in 1:(length(ids)) ){
    m =  mean(mat[ mat[,1]==ids[i] ,2])
    n = sum(mat[,1]==ids[i])
    mst = mst + n*(m-meanall)^2
    s =  var(mat[ mat[,1]==ids[i] ,2])
    mse = mse+(n-1)*s
    
  }
  mst = mst / (length(ids)-1)
  mse = mse / (nrow(mat)-length(ids))

  mst / mse
}


F_stat = calculateFratio(GPAscoresFactor)

K=length(unique(GPAscoresFactor$Class))
n= nrow(GPAscoresFactor)
df1 = K -1
df2 = n -K 


x = seq(0,5,0.01)

plot(x,df(x,df1,df2),type='l')
points(F_stat,0,col='green')

right = qf(0.05,df1,df2,lower.tail = F)

abline(v= right)

pvalue = pf(F_stat,df1,df2,lower.tail = F)
