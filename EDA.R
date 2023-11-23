
#EDA
library(rafalib)
data(father.son,package="UsingR") ##available from CRAN
x <- father.son$fheight
y <- father.son$sheight
plot(x,y,xlab="father's height in inches",ylab="son's height in inches")
##
boxplot(split(y,round(x)))
print(mean(y[round(x) == 72]))
##
#standardize the data
x=(x-mean(x))/ sd(x)
y=(x-mean(y))/ sd(y)
means=tapply(y,round(x*4)/4,mean)
fatherheights=as.numeric(names(means))
plot(fatherheights,means,ylab="avg of son heights", xlab="fatherlength")

##
#Scatterplot Exercises
data(nym.2002,package="UsingR")
library(dplyr)
males <- filter(nym.2002, gender=="Male")
females <- filter(nym.2002, gender=="Female")
#find corr between males age and time 
x=males$age
y=males$time
cor(males$age,males$time)
plot(x,y,xlab="Age",ylab="time",main=paste("correlation =",signif(cor(x,y),2)))
#Scatterplot Exercises #2
#For females, what is the Pearson correlation between age and time to finish?
cor(females$age,females$time)
#Scatterplot Exercises #3
num2 <-as.data.frame(nym.2002)
num2$age_grcut <- cut(num2$age, 
                      breaks = c(5, 25, 30, 35, 40, 50, 60, 70, 81), 
                      labels = c("5-25", "25-30", "30-35", "35-40", "40-50", "50-60", "60-70", "70-81"), right = FALSE)

boxplot(time~age_grcut, num2)

###
#Symmetry of Log Ratios Exercises
time=sort(nym.2002$time)
#What is the fastest time divided by the median time?
min(time) / median(time)
#What is the slowest time divided by the median time?
max(time) / median(time)


##Robust summary statistics
#Robust: doesn't affect measurement so much
#MAD: Robust estimation of standard deviation 

#Median, MAD, and Spearman Correlation Exercises
data(ChickWeight)
head(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"),timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
#what is the average weight of the day 4 chicks, including the outlier chick, divided by the average of the weight of the day 4 chicks without the outlier
chick_w4 <- chick[,'weight.4']
chick_w4_add <- append(chick_w4,3000)
chick_w4_add
# Difference between with and without outlie
mean(chick_w4_add) - mean(chick_w4)
# Ratio between with and without outlier
mean(chick_w4_add)/mean(chick_w4)
#median
median(chick_w4_add) - median(chick_w4)
median(chick_w4_add)/median(chick_w4)
#sd
sd(chick_w4_add) - sd(chick_w4)
sd(chick_w4_add)/ sd(chick_w4)
#MAD
mad(chick_w4_add) - mad(chick_w4)
mad(chick_w4_add)/ mad(chick_w4)
#Calculate the Pearson correlation of the weights of chicks from day 4 and day 21.
cor(c(chick$weight.4, 3000), c(chick$weight.21,3000))/cor(chick$weight.4, chick$weight.21)


#Mann-Whitney-Wilcoxon Test Exercises
#Exercises #1
x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
t.test(x,y)$p.value
wilcox.test(x,y)$p.value
x_add <- c(x,200)
t.test(x_add,y)$p.value

#Exercises #2
x = chick$weight.4[chick$Diet == 1]
y = chick$weight.4[chick$Diet == 4]
wilcox.test(c(x, 200), y, exact=FALSE)$p.value

#Exercises #3
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

#Quiz
#Q 2
wilcox.test(x,y+10)$p.value
wilcox.test(x,y+100)$p.value
wilcox.test(c(1,2,3),c(4,5,6))$p.value
#Q 3
wilcox.test(c(1,2,3),c(400,500,600))$p.value
#Q 4
data(nym.2002, package="UsingR")
time = sort(nym.2002$time)
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
