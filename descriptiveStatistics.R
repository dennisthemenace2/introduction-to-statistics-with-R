### This file contains exercises regarding descriptive statistics


#TASKS
# a) plot the student scores data 
# b) calculate mean,mode, median, and add it to the plot
# c) call the boxplot function in R with the student scores data


#student score:
scores=c(86, 80, 25, 77, 73, 76, 100, 90, 69, 93,
  90, 83, 70, 73, 73, 70, 90, 83, 71, 95,
  40, 58, 68, 69, 100, 78, 87, 97, 92, 74)
#a)

##plot a histogram
hist(scores,freq = F)


##define a function
plotMyDataRelative=function(data){
  
  cases = unique(data) ## get uniqie data points
  cases =cases[order(cases)] 
  heights = c()## create an empty array for the results
  for(i in 1:length(cases)){
    heights = c(heights,sum(data==cases[i])/length(data) )
  }
  
  barplot(heights,names.arg = cases)
  ##resturn the estimates so that I can use them later
  list( 'heights'= heights,'values'=cases)
}
## call my plot function, save results for later mode estimation
result = plotMyDataRelative(scores)



# b)

##calculate mean, mode, and std. dev. and plot these

##plot
hist(scores,freq = F)
#plot mean
points(mean(scores),0,col='green')
points(median(scores),0,col='red')
points(result$values[which.max(result$heights)],0,col='blue')

#plot mu+std and mu-std
abline(v=mean(scores)+sqrt(var(scores)),col='green' )
abline(v=mean(scores)-sqrt(var(scores)),col='green' )


#c) make a boxplot for the data
boxplot(scores)
