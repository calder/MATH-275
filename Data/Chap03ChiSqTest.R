#------------------------------------------------
#Chapter 3.4.1
#Here is a function that computes the chi-square
#test statistic

chisq<-function(Obs)
{ #Obs is the observed contingency table
  Expected <- outer(rowSums(Obs),colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}


#--------------------------------------------------------
#Import the GSS2002 data set
#GSS2002 <- read.csv("GSS2002.csv")

Education <- subset(GSS2002, select=Education, drop=T)
DeathPenalty <- subset(GSS2002, select=DeathPenalty,drop=T)

table(Education, DeathPenalty)

observed <- chisq(table(Education,DeathPenalty))

#Find those rows where there is at least one NA
index<- which(is.na(Education) | is.na(DeathPenalty))

#Remove those rows from the two variables and define
#DeathPenalty2 to be the data frame with missing values removed
Educ2 <- Education[-index]
DeathPenalty2 <-  DeathPenalty[-index]

N <- 10^4-1
result<-numeric(N)

for (i in 1:N)
 {
   my.permutation <-sample(DeathPenalty2)
   my.table <- table(Educ2, my.permutation)
   result[i]<-chisq(my.table) 
 }

#Create a histogram
 hist(result, xlab = "chi-square statistic", main = "Distribution of chi-square statistic",
    prob = TRUE, ylim = c(0,.2))
#The prob=TRUE option scales the histogram to have area 1
    
 abline(v = observed, col = "blue", lty = 5)
 
#optional: impose the density curve onto the histogram 
curve(dchisq(x, df = 4), from = 0, to = 25, col = "green", add = T) 

#Compute P-value
(sum(result >= observed)+1)/(N + 1)  

#----------------------------------------------------------------

#Chapter 3.6
candy.mat <- rbind(c(42,20,38), c(33,27,50))
candy.mat

chisq.test(candy.mat)

#Chapter 3.8
Homeruns <- subset(Phillies2009, select = Homeruns, drop = T)
lambda <- mean(Homeruns)
dpois(0:5, lambda)
table(Homeruns)

table(Homeruns)/162


#modified LMC 1/9/2014
