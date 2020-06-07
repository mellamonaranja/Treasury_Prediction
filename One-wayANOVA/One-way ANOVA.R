#One-way ANOVA(Analysis of Variance)#

#Import the data
distance=read.csv("C:/dis3years.csv",header = TRUE, na.strings = '.')

View(distance)
str(distance)

#Change factor to vector
distance$year=factor(distance$year,levels = c(1,2,3),labels=c('2015','2016','2017'))
View(distance)

attach(distance)

#Get the Descriptive Statistic
library(psych)
describeBy(Distance,year,mat=T)

#Create the graph(histogram, boxplot, Kernel Density Curve)
library(ggplot2)
ggplot(distance,aes(x=year,y=Distance),ylim=c(0,60))+
    geom_boxplot(outlier.colour = 'red',outlier.fill = 'skyblue',outlier.shape = '*')+
    ggtitle('Distance by year')+
    theme_classic()+
    theme(title = element_text(color = 'orange',size = 15),element_line('purple'),element_rect(color = 'pink'))

ggplot(distance,aes(x=Distance))+
    geom_histogram(binwidth = 5,fill=heat.colors(18),colour='grey')+
    facet_grid(.~year)+
    ggtitle('Distance by year')+
    theme_classic()

ggplot(distance,aes(x=year))+
    geom_density(fill='orange',colour=NA,alpha=.3)+
    geom_line(stat = 'density')+
    expand_limits(y=0)+
    ggtitle('Kernel Density Curve')

ggplot(distance,aes(x=Distance,colour=year))+
    geom_density(fill='orange',colour=NA,alpha=.3)+
    geom_line(stat = 'density')+
    expand_limits(y=0)+
    ggtitle('Kernel Density Curve by year overlap')

ggplot(distance,aes(x=Distance))+
    geom_density(fill=topo.colors(1),colour=NA,alpha=.3)+
    geom_line(stat = 'density')+
    expand_limits(y=0)+
    ggtitle('Kernel Density Curve by year')+
    facet_grid(year~.)+
    xlim(0,30)

ggplot(distance,aes(x=Distance))+
    geom_histogram(binwidth = 2,fill='purple',colour='white',alpha=.5)+
    geom_density(fill=NA, colour='black', alpha=.8)+
    geom_line(stat='density')+
    expand_limits(y=5)+
    ggtitle("Histogram")

#Statistic Analysis-Homogeneity of variance
#I : Bartlett Test
bartlett.test(Distance~year, data=distance)
distance=aov(Distance~year, data=distance)
summary(distance)

#II : Levene Test
library(car)
leveneTest(Distance~year,data = distance, center=mean)

#Multicamparison test
#I : If the number of sample by group is same-Tukey HSD, Duncan LSR
TukeyHSD(distance)

install.packages("agricolae")
library(agricolae)

duncan.test(distance,'group',group=TRUE,console=TRUE)

#II : If the number of sample by group isn't same-Scheffe
scheffe.test(distance,'group',group=FALSE,console=TRUE)

#Create Statistic Analysis Graph
tukeyPlot=TukeyHSD(distance)
plot(tukeyPlot,col=topo.colors(5))

duncanPlot=duncan.test(distance,'group')
plot(duncanPlot,xlim = c(0,1000),ylim = c(0,1550),col=topo.colors(5))

x=9.57 #2015
se=0.42
data=rnorm(1000,x,se)
data=sort(data)
plot(data, dnorm(data,x,se),col='purple',type='l',main = 'Distance by year')
abline(v=x,col='pink',lty=6)

par(new=T)
x=9.26 #2016
se=0.38
data=rnorm(1000,x,se)
data=sort(data)
plot(data, dnorm(data,x,se),col='blue',type='l')
abline(v=x,col='pink')

par(new=T)
x=10.30 #2017
se=0.39
data=rnorm(1000,x,se)
data=sort(data)
plot(data, dnorm(data,x,se),col='green',type='l')
abline(v=x,col='pink')

#Statistic Analysis-Heteroscedasticity of variance
#I : Welch's ANOVA Test
oneway.test(Distance~year, data=distance,var.equal = FALSE)
install.packages('nparcomp')
library(nparcomp)
result=mctp(Distance~year, data = distance)
summary(result)

#Appendix, Non-parametric statistics : Kruskal Wallis H Test
kruskal.test(Distance~year, data = distance)
install.packages('userfriendlyscience')
library(userfriendlyscience)
posthocTGH(distance$Distance, distance$year)
