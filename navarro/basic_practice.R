library(lsr)  #stats package from textbook

# get a sample from normal distr
normal.a <- rnorm(n=1000,mean=0,sd=1) 
hist(normal.a)
mean(normal.a)
sd(normal.a)  # calculates sigma bar, estimate of pop standard dev

# create chisq distr from normal samples!
normal.b <- rnorm(n=1000)
normal.c <-  rnorm(n=1000)
chi.sq.3 <- (normal.a)^2 + (normal.b)^2 + (normal.c)^2 

#calculate 97.5th percentile of a t distribution with N-1 degrees of freedom
n=500
qt(p=.975,df=n-1)

#calculate 95% confidence interval for mean of normal.a
ciMean(normal.a, conf=.95)   #lsr function


#hypothesis test that probability of random variable !=0.5
binom.test(x=62,n=100,p=.5)

# chi-sq tests

load("data/randomness.Rdata") #load data from textbook
observed <- table(cards$choice_1) #create frequency table of suit first observed

chisq.test(x=observed) #GOF test on null hyp. of all suits equally likely
chisq.test(x=observed,p=c(.2,.3,.3,.2)) #GOF test on null hyp. of different probs


load("data/chapek9.Rdata") #load two-variable data from textbook
freq_chapek <- xtabs(~choice+species,data=chapek9) #create frequency table

#beginner function for independence test
associationTest(formula = ~choice+species,data=chapek9)

#base function for independence test
chisq.test(freq_chapek)


### t tests ###
load("data/zeppo.Rdata")
oneSampleTTest(grades,mu=67.5) #beginner function for t test
t.test(x=grades,mu=67.5) #base function for one-sample t test

load("data/harpo.Rdata")

#beginner func independent t test
independentSamplesTTest(formula=grade~tutor,data=harpo,var.equal=TRUE)

t.test(formula=grade~tutor,data=harpo,var.equal=TRUE) #base independent t test
t.test(formula=grade~tutor,data=harpo) #base independent Welch test


### linear regression ###

load("data/parenthood.Rdata")

#simple OLS regression on sleep vs grumpiness
lm(formula=dan.grump~dan.sleep,data=parenthood )
regression <- lm(formula=dan.grump~dan.sleep+baby.sleep,data=parenthood)

# hypothesis test regression model (F-statistic) and each predictor
# coefficient (using t statistic)
summary(regression)


confint(object=regression,level=.95) #calculate confidence interval for each coefficient

standardCoefs(regression) #standardized regression coefs, i.e. same unit for comp


