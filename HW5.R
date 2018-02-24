#Question 1

#Load Library
library(moments)

#Ensure reproducable results 
set.seed(123456)

#simulate sample 
x<-rchisq(100,5)

#statistics overview 
#Set up variables to calculate variance, skewness, mean, median later
N<-length(x)
Summation<-sum(x)
mu<-Summation/N
list(mu)


#1. What is the mean? 5.153984
#2. What is the variance? 9.090508
#3. What is the skewness? From this number, is the mean greater or smaller
than the median? Why? Skewness is 0.7777164 & median is 4.531642
#4. What is the kurtosis? Is it a fat- or thin-tail distribution? Kurtosis is 3.142032, 
#if kurtosis is greater than 3, the distribution is assumed to have a fat tail. 

#Calculate Variance 
Variance<-sd(x)^2
list(Variance)

#List Skewness 
skewness(x)

#List Median
median(x)

#List Kurtosis
kurtosis(x)

#2a. Generate the 6th moment vars 
six.moment.var<-(x^6)-(mu^6)
list(six.moment.var)

#2b. Mean of the new variable six.moment.var 
numerator<-sum(six.moment.var)/length(six.moment.var)
list(numerator)

#Calculate Variance
variance<-(sd(x)^2)/length(x)
list(variance)

#Calculate Standard Deviation
sd<-sqrt(variance)
list(sd)

#Set denominator for 6th moment 
denominator<-sd^6
list(denominator)

#sss = sixth moment 
sss<-numerator/denominator
list(sss)




