## Exercise for basic probabilities


#TASKS:

#a) implement the scenario and simulate the experiment.
#scenario:
#A box contains 10 white and 10 black marbles. Construct a sample space for the experiment of randomly
#drawing out, with replacement, two marbles in succession and noting the color each time. (To draw “with
#replacement” means that the first marble is put back before the second marble is drawn.)

#a)
marbles = c(rep('Black',10),rep('White',10) ) 
N= 10000
success = 0
for(i in 1:N){
  s = sample(marbles,2,replace = T) ## take two marbles with replacement
  ##check if they are the same
  if(s[1]==s[2]){
    success = success+1
  }
}

prob = success/N
print(prob)


###b

#Example 2:
#  Suppose you take out two cards from a standard pack of cards one after another, without replacing the first card. What is probability that the first card is the ace of spades, and the second card is a heart?
#  The two events are dependent events because the first card is not replaced.
#There is only one ace of spades in a deck of 52 cards. So:
#  P(1st card is the ace of spades)=1/52
#If the ace of spaces is drawn first, then there are 51
#cards left in the deck, of which 13 are hearts:
#P(2nd card is a heart | 1st cardis the ace of spades)=13/51
#So, by the multiplication rule of probability, we have:
#  P(ace of spades, then a heart)=1/52 * 13/51= 1/204

#program with simulation.
# how does the the number of trials effect the estimated probability

cards = c(paste('heart',1:13),paste('spades',1:13),paste('diamond',1:13),paste('clubs',1:13))

Ns = seq(100,10000,100)
probs = c() ## save results
for(k in 1:length(Ns)){ ## double for loop
  success = 0 # count successes
  for(i in 1:Ns[k]){
    s = sample(cards,2,replace = F)
    if(s[1]=='spades 13'){### first one highest spades
      split = strsplit(s[2],split = " ") 
      if(split[[1]][1]=='heart'){ ## second one is a heart ?
        success = success +1
      }
    } 
  }
  probs = c(probs,success/Ns[k] )
}

plot(Ns,probs,type = 'l',xlab='Number trials', ylab='P(1st ace of spades, then a heart)')
abline(h= 1/204,col='red') ## true value
