#I manually entered the data for the OU Football Teams Scores for the 2016 Season and 2017 Season. The data came from soonerstats.com and I created vecrtors in the global environment to reference the data. 

Opponent.Name <- c("Houston", "Louisiana Monroe", "Ohio State", "TCU",
                  "Texas", "Kansas State", "Texas Tech", "Kansas", "Iowa State", "Baylor", "West Virginia",
                  "Oklahoma State", "Auburn", "UTEP", "Ohio State", "Tulane", "Baylor")
OU.Score <- c(23, 59, 24, 52, 45, 38, 66, 56, 34, 45, 56, 38, 35, 56, 31, 56, 49)
Opponent.Score <- c(33, 17, 45, 46, 40, 17, 59, 3, 24, 24, 28, 20, 19, 7, 16, 14, 41)


#2. I use the list command to show all of the data for Opponent.Name. 

list(Opponent.Name)

#3. In order to see the data that was input into our vectors above, we build a data frame so that we can easily visualize the data. 

OUdata <- data.frame(Opponent.Name, OU.Score, Opponent.Score)

#4. OU.Score[c(2,4)] helps the user reference specific data and is the result of the combine values function. This provides the user the value of the datum element in the Ou.Score referenceing row's 2 and 4.  

OU.Score[c(2,4)]

#5. 

OU.Score2 <- OU.Score/10
list(OU.Score2)


OU.Score3 <- OU.Score/OU.Score[c(1)]
list(OU.Score3)

#7. The spread of each game is listed below. Negative values indicate a loss and positive values indicate a win.


Spread <- (c(OU.Score) - c(Opponent.Score))
list(Spread)

#8. Using the length() function in R, you are able to count the number of elements used for a given vector. length() is used over count() as count will not list duplicate a value in it's count rather it will increase its frequency associated with the given variable. 

length(OU.Score)

#9. Total points scored from the beginning of the 2016 season to now.

sum(OU.Score)

#10. The calculation for the average score per game and it's result, the average points per game.

sum(OU.Score) / length(OU.Score)


##################Part 2


#1. In order to import our csv. files, it is vital that we set our working directory first.
getwd()
setwd("/Users/jhoehne/Desktop/Econ 5023/HW/HW2/HW2Q2")
list.files()
Kenya <- read.csv("Kenya.csv")
Sweden <- read.csv("Sweden.csv")
World <- read.csv("world.csv")

#2. For Question 2, I will calculate the CBR or Crude Birth Rate. In order to calculate CBR, the user divides "Total number of births during the period" by "Total number of person-years lived during the period". Through the list() command, the user is able to see the rates for each country for any given time period. Through the analysis, the user can see that the birthrates decline between 1950-1955 and 2005-2010. This could be due to a number of factors as people living longer, strain on resources, a rise in disposible income, and preferences.


Kenya$totalpy1950and1955 <- sum(Kenya$py.men[1:15] + Kenya$py.women[1:15])
Kenya$totalpy2005and2010 <- sum(Kenya$py.men[16:30] + Kenya$py.women[16:30])
Kenya$totalbirths1950and1955 <- sum(Kenya$births[1:15])
Kenya$totalbirths2005and2010 <- sum(Kenya$births[16:30])

Sweden$totalpy1950and1955 <- sum(Sweden$py.men[1:15] + Sweden$py.women[1:15])
Sweden$totalpy2005and2010 <- sum(Sweden$py.men[16:30] + Sweden$py.women[16:30])
Sweden$totalbirths1950and1955 <- sum(Sweden$births[1:15])
Sweden$totalbirths2005and2010 <- sum(Sweden$births[16:30])

World$totalpy1950and1955 <- sum(World$py.men[1:15] + World$py.women[1:15])
World$totalpy2005and2010 <- sum(World$py.men[16:30] + World$py.women[16:30])
World$totalbirths1950and1955 <- sum(World$births[1:15])
World$totalbirths2005and2010 <- sum(World$births[16:30])

CBRK1950thru1955 <- Kenya$totalbirths1950and1955/Kenya$totalpy1950and1955
CBRS1950thru1955 <- Sweden$totalbirths1950and1955/Sweden$totalpy1950and1955
CBRW1950thru1955 <- World$totalbirths1950and1955/World$totalpy1950and1955

CBRK2005thru2010 <- Kenya$totalbirths2005and2010/Kenya$totalpy2005and2010
CBRS2005thru2010 <- Sweden$totalbirths2005and2010/Sweden$totalpy2005and2010
CBRW2005thru2010 <- World$totalbirths2005and2010/World$totalpy2005and2010

list(CBRK1950thru1955, CBRK2005thru2010, CBRS1950thru1955, CBRS2005thru2010, CBRW1950thru1955, CBRW2005thru2010)


#3. Below are the calculations for the Age-Specific Birth Rate. The data suggests that Kenya had an ASFR 4x greater than Sweden in period 1950-1955 and a 3x greater ASFR in the period 2005-2010. Perhaps this is becase 


ASFRK1950and1955 <- (sum(Kenya$births[4:10])) / (sum(Kenya$py.women[4:10]))
ASFRK2005and2010 <- (sum(Kenya$births[19:25])) / (sum(Kenya$py.women[19:25]))
ASFRS1950and1955 <- (sum(Sweden$births[4:10])) / (sum(Sweden$py.women[4:10]))
ASFRS2005and2010 <- (sum(Sweden$births[19:25])) / (sum(Sweden$py.women[19:25]))
ASFRW1950and1955 <- (sum(World$births[4:10])) / (sum(World$py.women[4:10]))
ASFRW2005and2010 <- (sum(World$births[19:25])) / (sum(World$py.women[19:25]))

ASFR = data.frame(ASFRK1950and1955, ASFRK2005and2010, ASFRS1950and1955, ASFRS2005and2010, ASFRW1950and1955, ASFRW2005and2010)



#4. We calculate total fertility rate as the age-specific births for a given age range divided by the age-specific person years then multiplying by the number of years in that period. Thus, we see from the formulae below that our Total Fertility Rate designated in the format "TFR.first letter of each country.period of interest".

The data suggests that women are living longer and that there are more births occuring in 2000 since 1950. []

TFR.K.1950and1955 <- sum(5*(sum(Kenya$births[4])) / (sum(Kenya$py.women[4])) + 
  (5*(sum(Kenya$births[5])) / (sum(Kenya$py.women[5])))
+ (5*(sum(Kenya$births[6])) / (sum(Kenya$py.women[6])))
+ (5*(sum(Kenya$births[7])) / (sum(Kenya$py.women[7])))
+ (5*(sum(Kenya$births[8])) / (sum(Kenya$py.women[8])))
+ (5*(sum(Kenya$births[9])) / (sum(Kenya$py.women[9])))
+ (5*(sum(Kenya$births[10])) / (sum(Kenya$py.women[10]))))

TFR.K.2005and2010 <- sum(5*(sum(Kenya$births[19])) / (sum(Kenya$py.women[19])) + 
  (5*(sum(Kenya$births[20])) / (sum(Kenya$py.women[20])))
+ (5*(sum(Kenya$births[21])) / (sum(Kenya$py.women[21])))
+ (5*(sum(Kenya$births[22])) / (sum(Kenya$py.women[22])))
+ (5*(sum(Kenya$births[23])) / (sum(Kenya$py.women[23])))
+ (5*(sum(Kenya$births[24])) / (sum(Kenya$py.women[24])))
+ (5*(sum(Kenya$births[25])) / (sum(Kenya$py.women[25]))))

TFR.S.1950and1955 <- sum(5*(sum(Sweden$births[4])) / (sum(Sweden$py.women[4])) + 
  (5*(sum(Sweden$births[5])) / (sum(Sweden$py.women[5])))
+ (5*(sum(Sweden$births[6])) / (sum(Sweden$py.women[6])))
+ (5*(sum(Sweden$births[7])) / (sum(Sweden$py.women[7])))
+ (5*(sum(Sweden$births[8])) / (sum(Sweden$py.women[8])))
+ (5*(sum(Sweden$births[9])) / (sum(Sweden$py.women[9])))
+ (5*(sum(Sweden$births[10])) / (sum(Sweden$py.women[10]))))

TFR.S.2005and2010 <- sum(5*(sum(Sweden$births[19])) / (sum(Sweden$py.women[19])) + 
  (5*(sum(Sweden$births[20])) / (sum(Sweden$py.women[20])))
+ (5*(sum(Sweden$births[21])) / (sum(Sweden$py.women[21])))
+ (5*(sum(Sweden$births[22])) / (sum(Sweden$py.women[22])))
+ (5*(sum(Sweden$births[23])) / (sum(Sweden$py.women[23])))
+ (5*(sum(Sweden$births[24])) / (sum(Sweden$py.women[24])))
+ (5*(sum(Sweden$births[25])) / (sum(Sweden$py.women[25]))))

TFR.W.1950and1955 <- sum(5*(sum(World$births[4])) / (sum(World$py.women[4])) + 
  (5*(sum(World$births[5])) / (sum(World$py.women[5])))
+ (5*(sum(World$births[6])) / (sum(World$py.women[6])))
+ (5*(sum(World$births[7])) / (sum(World$py.women[7])))
+ (5*(sum(World$births[8])) / (sum(World$py.women[8])))
+ (5*(sum(World$births[9])) / (sum(World$py.women[9])))
+ (5*(sum(World$births[10])) / (sum(World$py.women[10]))))

TFR.W.2005and2010 <- sum(5*(sum(World$births[19])) / (sum(World$py.women[19])) + 
  (5*(sum(World$births[20])) / (sum(World$py.women[20])))
+ (5*(sum(World$births[21])) / (sum(World$py.women[21])))
+ (5*(sum(World$births[22])) / (sum(World$py.women[22])))
+ (5*(sum(World$births[23])) / (sum(World$py.women[23])))
+ (5*(sum(World$births[24])) / (sum(World$py.women[24])))
+ (5*(sum(World$births[25])) / (sum(World$py.women[25]))))

TFR <- data.frame(TFR.K.1950and1955, TFR.K.2005and2010, TFR.S.1950and1955, TFR.S.2005and2010, TFR.W.1950and1955, TFR.W.2005and2010)






