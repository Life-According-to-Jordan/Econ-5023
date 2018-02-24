#To first be able to work with the data it is essential that we know what our working 
#directory is set to. From there, we set the working directory to location that holds the csv. 
#file that we would like to work with. Below, we set the working directory to a folder that was 
#created to host the data cnames.csv.

getwd()
setwd("/Users/jhoehne/Desktop/HW3")

#Q1.1 We load the seasonally adjusted male labor force participation from St. Louis Fed’s 
#website referencing LNS11300001.  

library("quantmod")
getSymbols("LNS11300001", src="FRED")

#Q1.2 Altough plot() is included in the R package. I find that using chartSeries 
#presents our data better.

chart_Series(LNS11300001)

#Q1.3 The share of men between the ages of 25 and 54 either working or actively seeking 
#work has been falling for more than 60 years. The decline in participation has been roughly constant over much of this time horizon. As Figure 1 shows, participation among prime-age men peaked in 1954, declined only slightly until the mid-1960s, but then began to decline in earnest in the decade between 1965 and 1975, when the share in the labor force fell from 96.70 percent to 94.30 percent. Since then, participation has fallen persistently, with sharper declines in recessionary periods, such as the early 1990s, that were not fully reversed in the subsequent expansion periods. Since 1965, the prime-age male labor force participation rate has fallen by an average of 0.1614 percentage point each year, totaling an 8.3 percentage-point decline as of May 2016.

getSymbols("LRAC25MAUSM156S", src="FRED")
LRAC <- LRAC25MAUSM156S
LRAC[c(61, 181, 677)]

MT <- 96.7-88.4
MT/51.41666667

#Q1.4 After examing the graph below, the trend variable is the most distinct. 

#Q1.5 We decompose the model showing it's various components. This shows the user the different 
#components that make up the model.

attr(LNS11300001, 'frequency') <- 12
time <- (as.ts(LNS11300001))
results <- plot(decompose(time))

#or plot(decompose(as.ts(LNS11300001)))

#Q1.6 Using the R command tail() and referencing the variable time, R provides us with the 
#last 6 elements for the variable time. Using the last 6 cell references, I predict that the value 
#for the next period will by 69%. 

tail(time)

#Q1.7 We replicate the series of questions from before and use the FRED's database 
#for women in order to recompute the quetions. 

#Q1.1 - Female We load the seasonally adjusted female labor force participation from 
#St. Louis Fed’s website referencing LNS11300002.  

getSymbols("LNS11300002", src="FRED")
LNSF <- LNS11300002
chart_Series(LNSF)


#Q1.2 - Female 

chart_Series(LNS11300002)

#Q1.3 - Female The share of women between the ages of 25 and 54 either working or actively seeking 
#work has been rising for more than 60 years. The incline in participation has been roughly constant 
#over much of this time horizon. As Figure Q1.2 shows, participation among prime-age women hit a relative 
#minimum in 1948, increased slightly until the early-1950s, but then began to incline in the decade 
#between 1965 and 1975, when the share in the labor force increased from 44.9 percent to 54.8 percent. 
#Since then, participation has increased consistently, with slower increases in recessionary periods, 
#such as the early 1990s, that were not fully reversed in the subsequent expansion periods. Since 1965, 
#the prime-age female labor force participation rate has increased by an average of 0.1925 percentage 
#point each year, totaling an 9.9 percentage-point incline as of May 2016.

getSymbols("LRAC25FEUSM156S", src="FRED")
QF1.7 <- LRAC25FEUSM156S
QF1.7[c(61, 181, 677)]
FT <- 54.8-44.9
FT/51.41666667


#Q1.4 - Female Again we find that the trend variable is the most distinct.

#Q1.5 - Female Again, we decompose the model showing it's various components. This shows the user the 
#different components that make up the model.

attr(LNSF, 'frequency') <- 12
time2 <- (as.ts(LNSF))
results <- plot(decompose(time2))


#Q1.6 - Female Using tail to view the last 6 elements of time2 which we set above, we can safely 
#predict that the next value will approximate 57.3 percent. 

tail(time2)


#Q2.1 Here we obtain the unemployment rates without sesonality adjustments for both men and women. 

library(quantmod)
getSymbols("UNRATENSA", src="FRED")


#Q2.2 Then we use a mulitiplicative model to decompose this variable, and plot the results.

attr(UNRATENSA, 'frequency') <- 12
time2.2<-as.ts(UNRATENSA)
results2.2<-decompose(time2.2, type="multiplicative")
ls(results2.2)
plot(results2.2)

#Q2.3 After that, we plot the results again. This time free of the random component. 

Trendseasonobserved<-results2.2$seasonal+results2.2$trend
Trend<-as.ts(Trendseasonobserved)
plot(Trend)

#Q3 For Q3, we read in the csv. file cnames.csv.
cnames <- read.csv("cnames.csv")


#Q3.1 The percentage of white people in the file cnames.csv is 0.692878042.
sum(cnames$white)

#Q3.2 The amount of people in the data set cnames with the last name Smith is 0.0098 percent.

rowSums(head(cnames)[c("white", "black", "api", "asian", "others", "hispanic")], n = 1)

#head shows row sum values for the first 6 rows, however we only need to return results for the 
#first value, Smith, in this sequence.

#Q4.1 In order to create an increasing sequence, we start with our initial value, add our desired 
#end value, and specify that our increment is positive.

year <- seq(from = 1950, to = 2010, by = 10)
list(year)

#Q4.2 In order to create a decreasing sequence, we start with our initial value, add our desired end value, and specify that our increment is negative.
year.decreament <- seq(from = 2010, to = 1950, by = -10)
list(year.decreament)

