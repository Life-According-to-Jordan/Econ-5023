# Question 1 
#Super-Craps, 2 dice are rolled and you are need to guess the absolute value of difference in the dice rolled

#To choose an ideal outcome for the dice it would be 6.5 as it would be the mean of the expected value for 
#all outcomes from the dice. Thus, since the expected value for each dice is the same, the absolute value in 
#the difference between roles would be zero. 

#Variable for 2, 12-sided dice 
x_1<-c(1:12)
x_2<-c(1:12)

#expected value of the each dice roll dice 
mu_1<-mean(x_1) 
mu_2<-mean(x_2)
list(mu_1, mu_2)



#Question 2 The variable of interest is the failure rate of chips in our manufactoring process. 
#Given that the probability of failure is 0.005, we would expect that less than one out of every 
#50 chips fail to meet specifications. 

#I would advise that we enter into a contract with the firm. The probability of failing to meet their 
#requirements is less than 0.005%. More to it, in order to make a better decision, we need information 
#on the resulting penalty and profit margins for each chip. If we expect a few chips to be defective, 
#we could send an extra box to ensure that they have 10,000 chips that work properly and could reduce 
#the number of shipments that our firm sends their firm. 

#set seed to allow for replicability  
set.seed(123456)

#Failure as the probability that less than 2 chips fail 
Success<-dbinom(48, size=50, prob=.995)+dbinom(49, size=50, prob=.995)+dbinom(50, size=50, prob=.995)

#the probability that we will have more than 2 chips fail in a shipment 
Success 
1-Success

#Question 3 I don't think our student's strategy worked out very well for him. The student should 
#have studied more for his quiz. Given set.seed(123456), our student should only get one answer right 
#out of the 20 questions.

#Duplicate to ensure reliability 
#set seed to allow for replicability  
set.seed(123456)

#generate 20 observations
observations_3<-runif(20)

#Binomial comparison 
sample_3<-sample(0:1, 20, replace = TRUE, prob=c(0.9,0.1))
sample_3



#Question 4 
#Duplicate...again for safety 
set.seed(123456)

#create our function 
U<-runif(10000)
U_4<-1+2*U
gx<-U_4^2+U_4
mean(gx)
2*mean(gx)


#Question 5
#sample and plot density 
set.seed(123456)
mu_5<-vector(,10000)
for(i in 1:10000){
  sample_5<-sample(0:1, 10000, replace = TRUE)
mu_5[i] <- mean(sample_5)}
plot(density(mu_5))


#Question 6 
#used to control output
#lists the range from 1 - 5 

for (i in 1:5)
  print(i)

#Question 7
#The below code prints the numbers 1-10, with n-1 features per loop
for(n in 11:21)
for(i in 10:0){
  if(n-i>0)
    if(n-i<11)
  print (n-i)}


