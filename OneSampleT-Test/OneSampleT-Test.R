#One Sample T-Test#

#Import the data
long19=read.csv("C:/19long.csv", header = TRUE, na.strings = '.')
str(long19)
View(long19)

#Raise two decimals
long19=round(long19,2)
View(long19)

#Get the Descriptive Statistic
attach(long19)
library(psych)
describe(long19)
View(long19)

opar=par(no.readonly = TRUE)

#Separate the facet
par(mfrow=c(1,2))

#Create the graph(boxplot, histogram)
boxplot(long19, ylab='degree',col = 'orange')
hist(long,breaks = 10,col = 'orange',xlab = 'Country', ylab = 'Degree',ylim=c(0,15), main = '10y Treasury Bond(December,2019)')

options('scipen'=20)

#Statistic Analysis
t.test(long,alternative = c('two.sided'),mu=1.86, conf.level = 0.95)

#Create Statistic Analysis Graph
mu=1.86
se=0.35
data=rnorm(1000,mu,se)
data=sort(data)
plot(data, dnorm(data, mu, se),type = 'l', main = '10y Treasury Bond', xlim = c(-1,4))
abline(v=mu, col='green',lty=6)
abline(v=mu+1.96*se, col='blue',lty=6)
abline(v=mu-1.96*se, col='blue',lty=6)
abline(v=1.28,col='red',lty=6)
detach(long19)
