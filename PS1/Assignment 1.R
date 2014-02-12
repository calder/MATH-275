# Calder Coalson
# Assignment 1

# Problem 2.4
d = read.csv("/Volumes/COLLAB/Departments/MATH/MATH-Shared/Chihara/Math_275/FlightDelays.csv")
table(d$DepartTime)
barplot(table(d$DepartTime))
table(d$Delayed30, d$Day)
prop.table(table(d$Delayed30, d$Day), 2)[2,]
boxplot(FlightLength~Carrier, d)
# Yes.

# Problem 2.5
d = read.csv("/Volumes/COLLAB/Departments/MATH/MATH-Shared/Chihara/Math_275/GSS2006.csv")
table(d$DeathPenalty)
barplot(table(d$DeathPenalty))
table(d$OwnGun)
summary(d$OwnGun)
# NA count
table(d$DeathPenalty, d$OwnGun)
prop.table(table(d$DeathPenalty, d$OwnGun), 2)
# Yes.