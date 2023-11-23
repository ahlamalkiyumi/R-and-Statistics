
##Week 3
babies <- read.table("babies.txt", header=TRUE)
#We will study the differences in birth weight between babies born to smoking and non-smoking mothers.
#let's split this into two birth weight datasets: one of birth weights to non-smoking mothers and the other of birth weights to smoking mothers.
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies,smoke==1) %>% select(bwt) %>% unlist
##look for difference in means
library(rafalib)
mean(bwt.nonsmoke) - mean(bwt.somke)
popsd(bwt.nonsmoke)
popsd(bwt.somke)

##T-test Exercises #1
set.seed(1)
N = 25
sample_non = sample(bwt.nonsmoke ,N)
sample_smoke = sample(bwt.smoke ,N)
tval = t.test(sample_non,sample_smoke)
tval
#another way
N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)
X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)
X.s <- mean(dat.s)
sd.s <- sd(dat.s)
sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.s - X.ns)/sd.diff
abs(tval)

##T-test Exercises #2
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval 


##Confidence interval
set.seed(1)
chowPopulation <- read.csv("femaleControlsPopulation.csv")
chowPopulation <- unlist(chowPopulation)
mu_chow <- mean(chowPopulation)
mu_chow

#take sample
N <- 30
chow <- sample(chowPopulation, N)
print(mean(chow))
se <- sd(chow)/sqrt(N) # error
se
Q <- qnorm(1- 0.05/2)
# -Q < (mean(chow) - mean(chowPopulation)) / se <Q

interval <- c(mean(chow) - Q*se, mean(chow)+Q*se)
interval
#check if it's true or false 
interval[1] < mu_chow & interval[2] > mu_chow


##
library(rafalib)
B <- 250
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- 
    mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}

##small sample size and the CLT
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}

## Confidence Intervals Exercises
babies <- read.table("babies.txt", header=TRUE)
library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies,smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
## Confidence Intervals Exercises #1
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.n <- sample(bwt.smoke, N)
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )

##Confidence Intervals Exercises #2
set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.n <- sample(bwt.smoke, N)
t.test(dat.ns,dat.n)$p.value

##Power Calculations
library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex=="F" & Diet=="chow") %>%
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat, Sex=="F" & Diet=="hf") %>%
  select(Bodyweight) %>% unlist

mu_hf <-mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print(mu_hf - mu_control)/mu_control *100
# the percent of difference is 9.9 which is huge 

set.seed(1)
N <- 5
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
t.test(hf,control)$p.value
#p= 0.5 which means there is no difference 
##this is type II error: there is a difference but we fail to reject
#because we have small sample size


##Power calculations exercises
babies <- read.table("babies.txt", header=TRUE)
library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies,smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

##Exercises #2
#What proportion of the time do we reject at the 0.05 level?
B <- 10000
alpha <- 0.05
N <- 5
set.seed(1)
reject <- function(N, alpha=0.05){
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  pval <- t.test(dat.s, dat.ns)$p.value
  pval < alpha
}
rejections <- replicate(B,reject(N))
mean(rejections)

# Exercises #3
##Repeat the exercise above for samples sizes of 30, 60, 90 and 120. Which of those four gives you power of about 80%?
Ns <- seq(30, 120, 30)
set.seed(1)
powers <- sapply(Ns, function(N) {
  rejections <- replicate(B, reject(N))
  mean(rejections)
})
names(powers) <- Ns
powers

# Exercises #4
Ns <- seq(30, 120, 30)
set.seed(1)
powers <- sapply(Ns, function(N) {
  rejections <- replicate(B, reject(N, alpha =0.01))
  mean(rejections)
})
names(powers) <- Ns
powers


