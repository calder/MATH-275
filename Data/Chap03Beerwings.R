##-------------------------------------
##Beerwings Chapter 3.3

#Import the Beerwings data set (Tools > Import Data Set > Text
#Or, if Beerwings.csv is in your working directory, then
#uncomment below and run.
#Beerwings <- read.csv("Beerwings.csv")

tapply(Beerwings$Hotwings, Beerwings$Gender, mean)

observed <- 14.5333- 9.3333 #store observed mean differences    

#Get hotwings variable
hotwings <- subset(Beerwings, select = Hotwings, drop = T)

#set.seed(0)
N <- 99999  #set number of times to repeat this process
 result <- numeric(N) # space to save the random differences
 for(i in 1:N)
  {
  index <- sample(30, size = 15, replace = FALSE) # sample of numbers from 1:30
  result[i] <- mean(hotwings[index]) - mean(hotwings[-index])
}

##Plot

hist(result, xlab = "xbarM - xbarF", main = "Permutation distribution for hot wings")
abline(v = observed, col = "blue", lty=5)

#-------------------------
#Another visualization of distribution
#uncomment dev.new() if you're working in R.
#dev.new()   
plot.ecdf(result)
abline(v = observed,col = "blue", lty = 5)


#Compute P-value
(sum(result >= observed)+1)/(N + 1)  #P-value

#----------------------------------------
#Modified 1/9/2014
