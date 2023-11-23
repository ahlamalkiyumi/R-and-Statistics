
##
set.seed(1)
library(rafalib)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex=="F" & Diet=="chow") %>%
  select(Bodyweight) %>% unlist

ttestgenerator <- function(n) {
  #note that here we have a false "high fat" group where we actually
  #sample from the chow or control population
  #This is because we are modeling the null.
  cases <- sample(controlPopulation, n)
  cnotrols <- sample(controlPopulation, n)
  tstat <- (mean(cases) - mean(controls)) /
    sqrt(var(cases)- var(controls)/n)
  return(tstat)
}
#generate 10000 t-stat with sample size of 10
ttests <- replicate(1000,ttestgenerator(10) )
hist(ttests)
qqnorm(ttests)
abline(0,1)
# we can try less sample size 
# it works better with large sample size 

#with Monte Carlo simulation when n=3 t-dist works better than CLT
ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)

qqnorm(controlPopulation)
qqline(controlPopulation)

#when we don't have access to data we can generate using same mean and sd using monte carlo
controls <- rnorm(5000, mean=24, sd=3.5)
#use rnorm instead of sample 
ttestgenerator <- function(n, mean=24, sd=3.5) {
  cases <- rnorm(n,mean,sd)
  controls <- rnorm(n,mean,sd)
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt( var(cases)/n + var(controls)/n ) 
  return(tstat)
}
ttests <- replicate(1000,ttestgenerator(10) )
hist(ttests)
qqnorm(ttests)
abline(0,1)
  
## Monte Carlo Exercises
#create sample with size of 5  then compute the t-statistic with t= sqrt(5)*mean/sd
set.seed(1)
N <- 5
sample <- rnorm(N)
sqrt(N)*mean(sample)/ sd(sample)
# we can also use: t.test(sample)$statistic

#Monte Carlo Exercises #2
#generate B=1000  t-statistics as done in exercise 1. What proportion is larger than 2?
set.seed(1)
rep <- replicate(1000,t.test(rnorm(n=5))$statistic)
length(which(rep > 2))/ 1000

#Monte Carlo Exercises #3
#create different sample sizes. 
#For which sample sizes does the approximation best work?
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#Monte Carlo Exercises #4
#same as #3 but using t.test
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  

#Monte Carlo Exercises #5
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

#Monte Carlo Exercises #5
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

#Monte Carlo Exercises #7
#The sample median is approximately normal with mean 0 and SD larger than 1/sqrt(N)

##Permutations Exercises
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)
#
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample(dat)
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)- mean(nonsmokersstar)
})
( sum(abs(null) >= abs(obs)) + 1 )/ (length(null)+1)
##we add the 1s to avoid p-values=0 but we also accept:
(sum (abs(null) >= abs(obs)) )/ (length(null))

#Repeat the above exercise, but instead of the differences in mean, consider the differences in median
set.seed(1)
obs <- median(smokers) - median(nonsmokers)
null <- replicate(1000, {
  shuffle <- sample(dat)
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar) - median(nonsmokersstar)
})
( sum(abs(null) >= abs(obs)) + 1 )/ (length(null)+1)
##we add the 1s to avoid p-values=0 but we also accept:
(sum (abs(null) >= abs(obs)) )/ (length(null))

##Association Tests
##Chi-Square test is used to determine if there's a relationship between two categorical variables. 
#Example: if genotype and disease are related 
d <- read.csv("assoctest.csv")
#Compute the Chi-square test for the association of genotype with case/control status
tab <- table(d$allele,d$case)
chisq.test(tab)
#Exercises #2
fisher.test(tab)

##Quiz
#Question #1
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))




