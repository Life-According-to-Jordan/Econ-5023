library("quantmod")

#The following statements were reported by Fox News. As we are well aware, Fox News is a known right-wing news channel that harshly critques liberal practices. Therefore in Problem Set #1, I will investigate these claims and report on my findings. My results were compiled using the FRED database equipped in the R software. Additional sources that are cited are referenced in their respective segments.

# Lowest Labor Force Participation Rate since 1970

getSymbols.FRED(Symbols = "CIVPART", env = .GlobalEnv, return.class = "xts")

chartSeries(CIVPART, theme = 'white')

#At this time, there are an estimated 323.1 million individuals in the United States and given the claim that 95 million are unemployed, that equates to approximately 29.4% of the population. This has considerable implications such as tax revenue, crime rates, life expectancy, etc. However, the complement to Labor Force Participation Rates is explained in the next section. 


# Almost 95 Million Americans out of the labor force 

getSymbols.FRED(Symbols = "LNS15000000",
                env = .GlobalEnv,
                return.class = "xts")
chartSeries(LNS15000000, theme = "white")


#The fact that the nearly 95 Millions are not in the labor force is not entirely problematic. It is neccessary to understand the the many factors that conribute to missing workers such as: 

#Population aging - as people are living longer past retirement, the number of individuals in the population not     participating increases thus shrinking our ratio. 
#Capital Gains - people don't have to work to make money
#Cotinued Education - many individuals are continuing their education as the US economy is demanding a greater       percentage of individuals with a higher skill set. 
#Change in consumer preferences - perhaps another explanation is that workers reservation wages are not being met   by companies and therefore individual's are preferring leisure over work.


# Worst Economic Recovery Since 1940's 

getSymbols.FRED(Symbols = "A191RL1Q225SBEA",
           env = .GlobalEnv,
           return.class = "xts")
chartSeries(A191RL1Q225SBEA,theme ="white")


#Considering a variety of factors such as inflation, employment rates, GDP, etc. may impact economic recovery standards. I will focus primarily on % change in GDP during the years after the recession in relation to Obama's presidency. 

#The Obama administration's primary concern was stabalizing the economy. After such a financial scandal, it was important to put all resources into ensuring a base line of stability rather than attempting to encourage performance. Although the economy didn't quite bounce back unlike other recessions in the past, the global economy had already redefined itself adding in new criteria which are hard to evaluate without more research. Perhaps the economy cannot handle the growth we have seen in the past as we may be experiencing decreasing marginal benefits of GDP growth as the economy cannot keep expanding with our current constraints.

# Lowest Home Ownership rate in 51 years 

getSymbols.FRED(Symbols = "RHORUSQ156N",
                env = .GlobalEnv,
                return.class = "xts")
                
chartSeries(RHORUSQ156N, theme = "white")


#The data gathered from FRED shows comparable home ownership rates as to those 50 years ago. Consumer confidence decreased and consumers became more aware of their spending after the mortgage crisis resuting in the late 2000's recession. Another explanation for lowering home ownership rates could be the inability to obtain credit, temporary housing resulting from college enrollment, and changes to consumer preferences such as remaining in one location. 

# Almost 13 Million More Americans on food stamps 

getSymbols.FRED(Symbols = "TRP6001A027NBEA",
                env = .GlobalEnv,
                return.class = "xts")            
chartSeries(TRP6001A027NBEA, subset='2004:01:01::2017:01:01', theme = "white")


#The social safety net program for hunger in the United States is titled the Supplemental Nutrition Assistance Program, SNAP. SNAP is designed to ensure that citizens of the US have access to affordable food. 

#As of Jan 1, 2007, the total expense of SNAP was 30.9 Billion USD 
#The cost of SNAP in 2007 was $98.18 per person per month or $1,178.16/year. 
#Dividing total snap expenditure by expenditure per person results in 26.23 million people enrolled in SNAP.


#As of Jan 1, 2017, the total expense of SNAP was 65.4 Billion USD 
#The cost of SNAP in 2017 was $125 per person per month or $1500/year 
#Dividing total snap expenditure by expenditure per person results in 43.6 million people enrolled in SNAP.

#The difference in persons enrolled in SNAP from 2007 thru 2017 is 17.37 million persons. Thus, the claim that 13 Million more Americans are enrolled in SNAP is not to far from my calculation, given that the claim makes no references to specifc dates. 

#source: http://www.kff.org/other/state-indicator/avg-monthly-snap-benefits/?currentTimeframe=8&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D


