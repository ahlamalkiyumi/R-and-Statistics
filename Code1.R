dat <- read.csv("femaleMiceWeights.csv")
install.packages("downloader") 
library(downloader)
head(dat)
dat[12,2]#12 row, second column
col_11 <- (dat$Bodyweight)[11]
col_11 
head(dat)
nrow(dat)
max(dat$Bodyweight)
row37 <- dat[3:7,]
row37
mean(row37$Bodyweight)

set.seed(1)
# Generate a random sample of size 1 from the numbers 13 to 24
random_row <- sample(13:24, size = 1)
# Find the weight of the mouse represented by that row
mouse_weight <- dat$Bodyweight[random_row]
mouse_weight

install.packages("dplyr")
library(dplyr)
install.packages("utf8")
library(dplyr)

controls <- filter(dat, Diet=="chow")
controls <- select(controls, Bodyweight)
unlist(controls)
#select specific column
controls <- filter(dat,Diet=="chow") %>% 
  select(Bodyweight) %>% unlist


# Load the dplyr package (if not already loaded)
library(dplyr)

# Assuming "msleep" is your data frame
# Filter rows where "order" is equal to "primates"
filtered_msleep <- msleep %>%
  filter(order == "Primates")

# View the resulting data frame
View(filtered_msleep)
nrow(filtered_msleep)
class(filtered_msleep)


class(filtered_msleep$sleep_total)
mean(filtered_msleep$sleep_total)
mean_sleep_total <- filtered_msleep %>%
  summarize(mean_sleep = mean(sleep_total))


###
install.packages("UsingR")
library(UsingR)
install.packages("htmlTable")
library(UsingR)
x=father.son$fheight
length(x)
round(sample(x,20),1)#vector of 20 randomly values from x
##
#The data will be divided into bins based on the specified breaks
hist(x,breaks=seq(floor(min(x)),ceiling(max(x))),
     main="Height histogram",xlab="Height in inches")#main:title, xlab:xaxis
     
##
#seq:it generates values
#It takes three arguments: the start value, the end value, and the step size (increment)
#floor(min(x):largest integer less than or equal to the minimum value in the dataset 'x.'
#ceiling(max(x):  smallest integer greater than or equal to the maximum value in the dataset 'x.'
xs<-seq(floor(min(x)), ceiling(max(x)),0.1)
# ecdf:empirical cumulative probability distribution of 'x'
#(xs) is used to evaluate the ECDF at the specified sequence of 'xs.'
#l:line plot 
plot(xs,ecdf(x)(xs),type="l",
     xlab="Height in inches",ylab="F(x)")


## QQ Plot
mean(x)
sd(x)
##
#Normal distribution
# we can change the nuber and check if both have close values
#if both have close values: ND is a good approximation
mean(x>70) 
1-pnorm(70,mean(x),sd(x))
# use pnorm(70,mean(x),sd(x))

#plot QQ plot
ps <- seq(0.01,0.99,.01)
qs <- quantile(x,ps)
normalqs <-pnorm(ps,mean(x),sd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line

load("skew.RData")
dim(dat)#dimension
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[, i], main = paste("QQ Plot - Variable", i))
}


## Box Plot: when data is not normally distributed , QQ is not effective. Bocplot is good.  
data(exec.pay, package="UsingR")
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)

boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))

## exercises 
head(InsectSprays)
#Box Plot using split
boxplot(split(InsectSprays$count, InsectSprays$spray), main = "Boxplot by Spray", xlab = "Spray", ylab = "Count")
#Box Plot using formula
boxplot(count ~ spray, data = InsectSprays, main = "Boxplot by Spray", xlab = "Spray", ylab = "Count")

library(dplyr)
data(nym.2002, package="UsingR")
#Use boxplots and histograms to compare the finishing times of males and females
males <- nym.2002[nym.2002$gender == "male", "time"]
females <- nym.2002[nym.2002$gender == "female", "time"]
##histogram
# Check for missing values
is.na(males)
is.na(females)
# Plot histogram
# Filter out missing and non-finite values
head(males)

install.packages("dslabs")
library(dslabs)
library(dplyr)
data("heights")
h<-heights$height
#hist
hist(h, main ="Histogram of Height", xlab="Height",ylab="Frequency", col="skyblue")


### compare between two mice group (healthy and overweight)
dat <- read.csv("femaleMiceWeights.csv")
#Healthy group
control <- filter(dat,Diet=="chow") %>% 
  select(Bodyweight) %>% unlist
#overweight group
treatment <- filter(dat, Diet=="hf") %>%
  select(Bodyweight) %>% unlist
#compare mean
mean(treatment) #has higher mean
mean(control)
#those mean averages are normal variables 

population <-read.csv("femaleControlsPopulation.csv")
population <-unlist(population) #covert from data.frame to numeric
mean(sample(population,12)) #take mean of 12 random samples from population, you can do it many times 
#access to full population will help us answer if the difference happened by chance or not 

#Excercises
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist(read.csv(filename)) #entire population
RNGkind("Mersenne-Twister", "Inversion", "Rejection") #Make sure that you are using the correct random number generator (RNG)
mean(x)
set.seed(5)
x2 <- sample(population,5)
abs(mean(x2) - mean(x)) #difference

##Null hypothesis and p-value
obs <- mean(treatment) - mean(control)
n <- 10000
nulls <- vector("numeric",n)#empty vector
for (i in 1:n){
  control <- sample(population,12)
  treatment <-sample(population,12)
  nulls[i] <- mean(treatment) - mean(control)
}
hist(nulls)
mean(abs (nulls) > obs)#give us the p-value

#Use Central Limit Theorem
library(rafalib)
qqnorm(nulls)
qqline(nulls)
##

##Exercises
set.seed(1)
n <-1000
sampleavg <- vector("numeric",n)#empty vector
for (i in 1:n){
  sampledat <- sample (x, 5)
  sampleavg[i] <- mean(sampledat)
}
prop <- mean((abs(sampleavg) - mean(x)) > 1)


##Probability Distributions 
install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
#What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
lifex <- filter(gapminder, year=="1952") %>%
  select(lifeExp) %>% unlist
mean(lifex <=40)

##
prop = function(q) {
  mean(x<= q)
}
qs = seq(from=min(x), to=max(x), length=20)# range of qs that we can apply the function
props = sapply(qs, prop)#apply the prop function to each element of qs
plot(qs, props)
##
#we can write the above function in one line 
props = sapply(qs, function(q) mean(x<=q))
plot(ecdf(x))
##

#Normal Distribution
#mu and sigma are the most important values
#when we standardize (find z), will have have same units
x <= unlist (read.csv("femaleControlsPopulation.csv"))
set.seed(1)
n <- 1000
averages5 <- vector ("numeric",n)
for (i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
#
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for (i in 1:n) {
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages5)
hist(averages50)

# Calculate the proportion between 23 and 25 
proportion_between_23_and_25 <- mean(averages50 > 23 & averages50 < 25)
# Print the proportion
cat("Proportion of averages between 23 and 25:", proportion_between_23_and_25, "\n")

#What is the proportion of observations between 23 and 25 in a normal distribution 
#with average 23.9 and standard deviation 0.43?
mu <- 23.9
sigma <- 0.43
prop25 <- pnorm(25, mean=mu, sd= sigma)
prop23 <- pnorm(23, mean=mu, sd= sigma)
prop2325 <- prop25 - prop23 
cat("Proportion of averages between 23 and 25 in normal distribution  :", prop2325, "\n")

##
#Population and Samples CLT
dat <- read.csv("mice_pheno.csv")
#remove the lines that contain missing values
dat <- na.omit(dat)
library(dplyr)
x <- filter(dat, Diet=="chow" & Sex == "M") %>% select(Bodyweight) %>% unlist
mean(x)
sd(x)
#Set the seed at 1. Take a random sample  of size 25 from x
set.seed(1)
X = sample(x,25)
X_mean <-  mean(X)
#Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
#What is this population's average?
y = filter(dat,Diet=="hf" & Sex == "M" ) %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)
#Set the seed at 1. Take a random sample  of size 25 from y.
set.seed(1)
Y = sample(y,25)
Y_mean <- mean(Y)
abs(Y_mean - X_mean)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )

x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(2)
Y <- sample(y,25)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )
##Central Limit Theorem
#If a list of numbers has a distribution that is well approximated by the normal distribution,
#what proportion of these numbers are within one standard deviation away from the list's average?
list_of_numbers <- rnorm(100, mean = 0, sd = 1)
mean_val <- mean(list_of_numbers)
sd_val <- sd(list_of_numbers)
cum_prob_upper <- pnorm(mean_val + sd_val, mean = mean_val, sd = sd_val)
cum_prob_lower <- pnorm(mean_val - sd_val, mean = mean_val, sd = sd_val)
# Calculate the proportion within one standard deviation
proportion_within_one_sd <- cum_prob_upper - cum_prob_lower
cat("Proportion within one standard deviation:", proportion_within_one_sd, "\n")
# Calculate the proportion within 2 standard deviation
prob_upper <- pnorm(mean_val + 2*sd_val, mean = mean_val, sd = sd_val)
prob_lower <- pnorm(mean_val - 2*sd_val, mean = mean_val, sd = sd_val)
proportion <- prob_upper - prob_lower
# Calculate the proportion within 3 standard deviation
pnorm(3) - pnorm(-3)

#Define y to be the weights of males on the control diet.
#What proportion of the mice are within one standard deviation away from the average weight?
y <- filter(dat, Sex=="M" & Diet == 'chow') %>% select(Bodyweight) %>% unlist
library(rafalib)
mean_val <- mean(y)
sd_val <-popsd(y)
# Calculate the proportion within one standard deviation
z_score <- (y - mean(y))/popsd(y) # get t-statistic (i.e., z score)
mean(abs(z_score) <= 1)
#What proportion of these numbers are within two standard deviations away from the list's average?
mean(abs(z_score) <= 2)
mean(abs(z_score) <= 3)
#
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

##t-test
dat <- read.csv("femaleMiceWeights.csv")
#Healthy group
control <- filter(dat,Diet=="chow") %>% 
  select(Bodyweight) %>% unlist
#overweight group
treatment <- filter(dat, Diet=="hf") %>%
  select(Bodyweight) %>% unlist
N <- length(treatment)
obs <- mean(treatment) - mean(control)
#to do t-test we need to estimate the standard error
se <- sqrt(
  var(treatment)/N +
    var(control)/N
)
ttest <- obs/se
1 - pnorm(ttest)# p-value
2* (1-pnorm(ttest)) #p-value for two tail

##t-test, p-value, confidence interval 
ttest <- t.test(treatment, control)
ttest
qqnorm(control)
qqline(control)

##Exercises
set.seed(1)
n <- 100 # Number of dice rolls
n_simulations <- 10000
p <- 1/6
# Function to simulate and calculate z
simulate_z <- function() {
  # Simulate n random dice rolls
  x <- sample(1:6, size = n, replace = TRUE)
  proportion_6s <- mean(x == 6)  # Calculate the proportion of 6s
  z <- (proportion_6s - p) / sqrt(p * (1 - p) / n)
  return(z)
}

simulations <- replicate(n_simulations, simulate_z())
proportion_greater_than_2 <- mean(abs(simulations) > 2)
cat("Proportion of times |z| was larger than 2:", proportion_greater_than_2, "\n")
qqnorm(simulations)
qqline(simulations)

## to compare between 4 different ps and ns ans see which is best normal D
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

##
dat <- read.csv('femaleMiceWeights.csv')
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

##CLT
#Use the CLT to approximate the probability that our estimate X is off by more than 2 grams from mu .
sigma <- sd(X) #sample sd 
n <- length(X)
sample_avg <- mean(X)
se <- sigma/ sqrt(n) #sample mean standard error
# Calculate the z-scores for 2 grams above and below the mean
z_upper <- (sample_avg + 2 - mean(X)) / se
z_lower <- (sample_avg - 2 - mean(X)) / se
# Use the pnorm function to calculate the probabilities
prop_u <- 1- pnorm(z_upper)
prop_l <- pnorm(z_lower)
# Sum the probabilities for more than 2 grams above and below
sum <- prop_u + prop_l
cat("Probability that the estimate is off by more than 2 grams from the true mean:",sum, "\n")

##CLT, t-test, null hypothesis
M <- 12  # Sample size for group X
N <- 12  # Sample size for group Y
# Calculate sample standard deviations
s_X <- sd(X)
s_Y <- sd(Y)
# Estimate of SE(\bar{Y} - \bar{X})
se_diff_means <- sqrt((s_Y^2 / N) + (s_X^2 / M))
# Print the result
cat("Estimate of SE(\\bar{Y} - \\bar{X}):", se_diff_means, "\n")

# to find t value for Y bar - X bar 
#first method
ttest <- t.test(Y, X)
# second method
mean_X <- mean(X)
mean_Y <- mean(Y)
t_statistic <- (mean_Y - mean_X) / se_diff_means

##t-distribution is centered at 0 and has one parameter: the degrees of freedom, that control the size of the tails. 
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

#What is the probability of observing a quantity as large as what we computed in t_stat, when the null distribution is true?
#find p-value
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z)) 

# Use the t.test function
t_test_result <- t.test(Y, X, var.equal = FALSE)
# Access the p-value from the result
p_value_t_test <- t_test_result$p.value

#With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution, one that is larger. 
#These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, under the null, large values are more probable under the null distribution.

#Quiz week 2
##Question 1
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n <- 1000
res <- vector('double',n)
for (i in seq(n)) {
  avg_sample <- mean(sample(x,50))
  res[[i]] <- avg_sample
}
mean(abs(res-mean(x)) > 1)

#Question 2
#What is the proportion of countries in 1952 that have a life expectancy longer than 40 years but shorter than 60 years?
library(gapminder)
data(gapminder)
head(gapminder)
life_ex = filter(gapminder, year=="1952") %>% select(lifeExp) %>% unlist
life_x = sum(life_ex >40 & life_ex<60)
nnrow = nrow(filter(gapminder, year == 1952))
life_x/nnrow

##QQ-plot Exercises
load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  x <- dat[,i]
  qqnorm(x,  main=paste0("Q-Q plot for column V.",i,sep=""))
  qqline(x)
}
par(mfrow=c(1,1))
hist(dat[,4]) 
hist(dat[,9])