##Verizon Example 3.3
#Permutation test

#Uncomment below if you haven't imported Verizon yet
#Verizon <- read.csv("Verizon.csv")

tapply(Verizon$Time, Verizon$Group, mean)


Time <- subset(Verizon, select=Time, drop=T)
Time.ILEC <- subset(Verizon, select=Time, subset=Group=="ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time, subset=Group=="CLEC", drop=T)

observed <- mean(Time.ILEC)-mean(Time.CLEC)
N <- 9999  #set number of times to repeat this process

set.seed(99)
result <- numeric(N) # space to save the random differences
for(i in 1:N)
  {
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- mean(Time[index]) - mean(Time[-index])
}

hist(result, xlab = "xbar1 - xbar2", 
main="Permutation Distribution for Verizon repair times")
abline(v = observed, col = "blue", lty=5)

(sum(result <= observed)+1)/(B + 1)  #P-value


#-------------------------------------------------------
#Verizon Example 3.5
##median, trimmed means

tapply(Verizon$Time, Verizon$Group, median)

#Difference in means
observed <- median(Time.ILEC)-median(Time.CLEC)
observed

#Differnce in trimmed means
observed2 <- mean(Time.ILEC, trim=.25)-mean(Time.CLEC,trim=.25)
observed2

B <- 9999  #set number of times to repeat this process

#set.seed(99)
result <- numeric(N) # space to save the random differences
result2<-numeric(N)
for(i in 1:N)
  {
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- median(Time[index]) - median(Time[-index])
  result2[i] <- mean(Time[index],trim=.25)-mean(Time[-index],trim=.25)
}

hist(result, xlab = "median1 - median2", 
    main="Permutation Distribution for medians")
abline(v = observed, col = "blue", lty=5)


dev.new()
hist(result2, xlab = "trimMean1 - trimMean2", 
     main="Permutation Distribution for trimmed means")
abline(v = observed, col = "blue", lty=5)

#P-value difference in means
(sum(result <= observed) + 1)/(N + 1)  

#P-value difference in trimmed means
(sum(result2 <= observed2) + 1)/(N + 1) 

#------------------------------------------------
#Verizon Example 3.5, continued
##Time > 10 and ratio of variances

#difference in proportions of time > 10
observed <- mean(Time.ILEC > 10) - mean(Time.CLEC > 10)
observed

#ratio of variances
observed2 <- var(Time.ILEC)/var(Time.CLEC)

B <- 9999  #set number of times to repeat this process

#set.seed(99)

 result <- numeric(N)
 result2 <- numeric(N)
 for(i in 1:N)
  {
  index <- sample(1687, size=1664, replace = FALSE)
  result[i] <- mean(Time[index] > 10) - mean(Time[-index] > 10)
  result2[i] <- var(Time[index])/var(Time[-index])
}

hist(result, xlab = "Difference in proportions",  main="Repair times > 10 hours")
abline(v = observed, lty=5, col = "blue")


dev.new()
hist(result2, xlab = "variance1/variance2",  main="Ratio of variances")
abline(v = observed, lty=5, col = "blue")


#P-value difference in proportion
 (sum(result <= observed)+1)/(N + 1)  #P-value

#P-value ratio of variances
 (sum(result2 <= observed2)+1)/(N + 1)  #P-value
