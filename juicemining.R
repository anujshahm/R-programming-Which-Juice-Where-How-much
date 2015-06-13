#library (andrequire) load the installed packages and checks (ticks) it
#lattice implementation of grpahics. Read about it in help pdf.
#if you have tigerstats you have lattice in it as well
install.packages("tigerstats")
library(tigerstats)
library(lattice)
install.packages("ggplot2")
library(ggplot2)

#read the csv file and store it in oj 
oj <- read.csv("C:\\users\\anuj\\desktop\\oj.csv")

#$ index the var 'store' & storing data as factor vars will ensure they work correctly in statistical modelling. Conv cat vars to factor vars.  
oj$store <- factor(oj$store)

#access elements 1 to 3 
oj[1:3,]

#create data frames t1, t2 (t21 later in the code)
#tapply function here breaks the vector into groups of sales and brand apply the mean to it and return result of those groups only. na.rm is Yes remove the missing elements
t1=tapply(oj$logmove,oj$brand,FUN=mean,na.rm=TRUE)
t1
t2=tapply(oj$logmove,INDEX=list(oj$brand,oj$week),FUN=mean,na.rm=TRUE)
t2

#ONLY THIS IS IMP HERE: t2 [1, or 2, or 3,] take data from the t2 above and plt the weekly data of whichever brand we want from it with respect to sales 
#type l = line graph (can use p to plot points, h for hist and so on)
#xlab, ylab are labels on x and y axis
#ylim are the end limits from 7 to 12 since sales is from 7 to 12 if you see the sales attribute column 
plot(t2[1,],type= "l",xlab="week",ylab="dominicks",ylim=c(7,12),main="Dominick weekly sale")
plot(t2[2,],type= "l",xlab="week",ylab="minute.maid",ylim=c(7,12),main="Minute Maid weekly sale")
plot(t2[3,],type= "l",xlab="week",ylab="tropicana",ylim=c(7,12),main="Tropicana weekly sale")

#concatenate sales from 1,2,3 from t2, 121 weeks from week 40 to 160 and store in week1 var and concatenate week1 for all 3 t2[1, and 2, and 3]
logmove=c(t2[1,],t2[2,],t2[3,])
week1=c(40:160)

week=c(week1,week1,week1)

#replicate values in 1,2,3 of all weeks, concatenate and store in brand var
brand1=rep(1,121)
brand2=rep(2,121)
brand3=rep(3,121)

brand=c(brand1,brand2,brand3)

#loaded ggplot2, grpahics, mthods, nlme, stats4 packages and then got outputs!
#functions are self explanatory
#to produce separate plots, I am conditioning on the factor var by using numerical|factor
#you can use \n to split the title into two lines
#layout of separate plots can be determined with the layout argument
#read about the plots

xyplot(logmove~week|factor(brand),type= "l",layout=c(1,3),col="black", main="Sales of all 3 brands \n to compare")
boxplot(logmove~brand,data=oj, main="Box plots for sale of 3 brands")
histogram(~logmove|brand,data=oj,main="Histogram plot \n for sale of all 3 brands", layout=c(1,3))
densityplot(~logmove|brand,data=oj,layout=c(1,3),main="To study distribution of sales separately", plot.points=FALSE)
densityplot(~logmove,groups=brand,data=oj,main="To study distribution of sales all together", plot.points=FALSE)

xyplot(logmove~week,data=oj,col="black", main="Scatter plot sale Vs week")
xyplot(logmove~week|brand,data=oj,main="Scatter plot wrt to brand \n sale Vs week", layout=c(1,3),col="black")
xyplot(logmove~price,data=oj,main="Scatter plot sale Vs price", col="black")
xyplot(logmove~price|brand,data=oj,main="Scatter plot sale Vs price \n for all 3 brands", layout=c(1,3),col="black")

smoothScatter(oj$price,oj$logmove, main="Smooth scatter \n price Vs sale", xlab = "price", ylab = "logmove")


densityplot(~logmove,groups=feat, main="Density plot - presence or absence \n of advertisement", data=oj, plot.points=FALSE)
xyplot(logmove~price,groups=feat, data=oj)

#to further solidify the inferences, I am analyzing one store at a time i.e. store number 5 out of 83. Create and store in var obj
oj1=oj[oj$store == 5,]
#plot graphs
xyplot(logmove~week|brand,data=oj1,type="l",layout=c(1,3), col="black", main="Weekly sales of all 3 brands")
xyplot(logmove~price,data=oj1,col="black",main="sale - price effect")
xyplot(logmove~price|brand,data=oj1, main="sale - price effect \n for all 3 brands", layout=c(1,3),col="black")

densityplot(~logmove|brand,groups=feat,main="Density plot for all 3 brands \n presence or absence \n of advertisement", data=oj1,plot.points=FALSE)
xyplot(logmove~price|brand,groups=feat,data=oj1, main="Scatter plot for all 3 brands \n presence or absence \n of advertisement")

#data frame t21 has income and store, mean, no missing values
t21=tapply(oj$INCOME,oj$store,FUN=mean,na.rm=TRUE)
t21

#wealthiest
t21[t21==max(t21)]
#poorest
t21[t21==min(t21)]

oj1=oj[oj$store == 62,]
oj2=oj[oj$store == 75,]
#rbind() function combines vector, matrix or data frame by rows. Here it combines oj1 and oj2 data frames
oj3=rbind(oj1,oj2)

xyplot(logmove~price|store,data=oj3, main="Scatter plot for sale of juices in \n poorest and richest neighbourhood")
xyplot(logmove~price|store,groups=feat,data=oj3, main="Scatter plot for sale of juices in \n poorest and richest neighbourhood \n with and without advertisement")

#mhigh for store in the wealthiest neighborhood, so oj1 used and mlow for poorest so oj2 used
#lm is used to fit linear models for statistical analysis such as regression, ANOVA, ANCOVA etc.
#summary provides descriptive stats

mhigh=lm(logmove~price,data=oj1)
summary(mhigh)
plot(logmove~price,data=oj1, main = "Plot showing effect of price on sales \n in rich neighbourhood" , xlim=c(0,4),ylim=c(5,13))
abline(mhigh)

mlow=lm(logmove~price,data=oj2)
summary(mlow)
plot(logmove~price,data=oj2, main = "Plot showing effect of price on sales \n in poor neighbourhood", xlim=c(0,4),ylim=c(5,13))
abline(mlow)

