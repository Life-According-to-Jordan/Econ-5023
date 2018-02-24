#Question 1 Probability of failure for A & B 

FA <- Failure due to condition A 
FB <- Failure due to condition B 
FAB <- Falure due to conditions A & B 

#Probabilities for each failure condition
FA <- 0.02
FB <- 0.03
FAB <- 0.01

#Success defined as 1.00
PSuccess <- 1

#Queston 1.1 Probability of Failure = (FA + FB - FAB), we subtract FAB as we do not double count these values in our equation

PFailure <- (FA+FB-FAB)

#Question 1.2

PSF <- PSuccess - PFailure 

#Numeric value for our equation above 
list(PSF)

#A randomly chosen chip has a 0.04 percent chance of failing. The alternative is that a 
#randomly chosen chip has a 96% chance of not failing. Knowing these statistics, the company 
#can corretctly price its warranty package correctly. 

#PRACTICE WORKING WITH STOCK MARKET DATA 

#Question 2.1 To begin our analysis, we will pull up the data for the stock 
#prices of Google through quantmod and referencing Yahoo.com's data. 

library("quantmod")
getSymbols('GOOG', src='yahoo')
head(GOOG)


##Question 2.2 Now, we will construct a new variable called daily returns, defined as difference in log. 

#Algebraically: return<-log(S[i]/S[i-1])

dailyreturns<-diff(GOOG$GOOG.Adjusted)/lag(GOOG$GOOG.Adjusted)


#Queston 2.3 Now to calculate the VaR (value at risk) for daily returns at 
#95 percent confidence level based on the distribution for the daily returns.

#the distribution of daily returns 
#ret is our variable we are examining
#0.05 is our alpha level in relation to our confidence interval of 95% 
#type = 1 refereces the inverse of empircal distribution function, y=0 if g=0, and 1 otherwise. Q[i](p) 
#is a discontinuous function of p, with m=0 when i=1 and i=2, and m=-1/2 when i=3

VaRdailyreturns.05<-quantile(dailyreturns, 0.05, type=1, na.rm=TRUE)

#Question 2.4 VaR for $10,000 invested at a 95% confidence interval

#VaR = Amount Invested * VaR for Daily Returns

VaR.05 <- 10000*VaRdailyreturns.05

#the below code gives us the amount that we could lose upon investing

list((VaR.05))

#the below code gives us the absolute value of the amount that we could lose upon investing

list(abs(VaR.05))

#Thus, there is a 5% chance that we will lose more than $259.6059 on our $10,000 investment in the next day. 

##Question 2.5 
```{r}
#the distribution of daily returns 
#ret is our variable we are examining
#0.01 is our alpha level in relation to our confidence interval of 99%
#type = 1 refereces the inverse of empircal distribution function, y=0 if g=0, and 1 otherwise. Q[i](p) is a discontinuous function of p, with m=0 when i=1 and i=2, and m=-1/2 when i=3
VaRdailyreturns.01<-quantile(dailyreturns, 0.01, type=1, na.rm=TRUE)

```

```{r}
#VaR = Amount Invested * VaR for Daily Returns
VaR.01 <- 10000*VaRdailyreturns.01
#the below code gives us the amount that we could lose upon investing
list((VaR.01))
#the below code gives us the absolute value of the amount that we could lose upon investing
list(abs(VaR.01))
```
Thus, there is a 1% chance that we will lose more than $518.9665 on our $10,000 investment in the next day. 

##Question 2.6 
Upon our investigation we come across a stimulus, a change in CEO's, which effects the daily returns and we would like to analyze the change on daily returns. We will focus on the data of Google's stock price from yahoo post 2012.
```{r}
#The below code specifies which year of data we want to stored in the variable GOOG2012
dailyreturns2<-subset(dailyreturns, index(dailyreturns)>="2012-01-01")
#using head(GOOG2012) we pull the most recent 6 values for Googles stock prices of 2012
head(dailyreturns2)
```

The below code, tells R to calulate the VaR for Daily Returns in for the opening day of the NYSE in 2012. 
```{r}
VaRdailyreturns2.05<-quantile(dailyreturns2, 0.05, type=1, na.rm=TRUE)
list(VaRdailyreturns2.05)

#VaR = Amount Invested * VaR for Daily Return, 2012.
VaR2.05 <- 10000*VaRdailyreturns2.05
#the below code gives us the amount that we could lose upon investing in 2012 at an alpha of 5%.
list((VaR2.05))
#the below code gives us the absolute value of the amount that we could lose upon investing in 2012 at an alpha of 5%.
list(abs(VaR2.05))

```
Thus, there is a 5% probability that we will lose more than $200.9413 off of our $10,000 investment in the next after the CEO change at Google in 2012. 

```{r}
list(VaR.05, VaR2.05)
```
Additionally, the above code reveals the loss that we can expect at an alpha of 0.05 prior to the CEO and post CEO switch. This shows that there is less variability in the stocks trading price and that there is greater confidence in the earnings of the stock. Thus, in our preliminary analysis, I conclude that the switch in CEO's was a good decision based on the assumption that no other stimuli were present during this time which could be cause for the decrease in variablity. Thus, the CEO will likely be held responsible for the company's success and be awarded a nice bonus. 

##Question 2.7
We then want to calculate the VaR for the next week, thus we reset our variables. Since, 2012-01-01 and 2012-01-02 were observed US Holidays, we started our analysis on 12-01-03 a Tuesday. Therefore, when we look look a week out, our observation, 12-01-10, will have be on a Tuesday as well.
```{r}

GOOG.week<-weeklyReturn(GOOG$GOOG.Adjusted)
GOOGW12<-subset(GOOG.week, index(GOOG.week)>="2012-01-01")

weeklyReturn.05<-quantile(GOOGW12, 0.05, type=1, na.rm=TRUE)

list(weeklyReturn.05)

#VaR = Amount Invested * VaR for Daily Return, 2012.
VaRW.05 <- 10000*weeklyReturn.05
#the below code gives us the amount that we could lose upon investing in 2012 at an alpha of 5%.
list((VaRW.05))
#the below code gives us the absolute value of the amount that we could lose upon investing in 2012 at an alpha of 5%.
list(abs(VaRW.05))
```
Thus, there is a 5% chance that we will lose more than $442.31 on our $10,000 investment in the next week. 

##Question 3.1 

Below, we set variables for the case that n firms will join the market. Where Ci represents the number of competitors for i competitors + 1. 
```{r}
c(C1<-2, C2<-3, C3<-4, C4<-5, C5<-6)
```

We'll express revenue as a funciton of competitors in the market: F(R)=100/Ci, for i firms in the market.
```{r}
M<-100 #in Millions of Dollars  
#This below code shows our expected revenue given the number of firms in the market.
Revenue<-data.frame(M*(1/c(C1,C2,C3,C4,C5)))
#revenue is visualized through the list command 
list(Revenue)
```

We can then set a data frame to reference our probabilities for each additional competitor.
```{r}
PP<-data.frame(c(P.C1<-0.1, P.C2<-0.25, P.C3<-0.3, P.C4<-0.25, P.C5<-0.1))
```

Thus, we multiply our tables Revenue and PP togeher to generate our expected revenue at for each scenario. 
```{r}
ExpectedValue<-sum(Revenue*PP)
list(ExpectedValue)
```
Since our Expected Value is $27.5 Million and our fixed costs are $26 Million, I would recommend that we enter the new market.

##Question 3.2 

Suppose that you are averse to risk. Then calculate your expected utility using the following utility function, where r is our risk-tolerance constat. 
U(x) = −exp(−x/r)

In this problem, we will assign r a value of 50. 

```{r}
EU <- (-exp(-ExpectedValue/50))
list(EU)
```
