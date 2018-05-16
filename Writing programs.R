setwd('~/./Writing programs') #changes file location
wnv <- read.csv("wnv.csv") #adds wnv.csv dataset to wnv
head(wnv) #check first 6 lines of dataset

library(ggplot2) #loads ggpplot library

ggplot(data=wnv) +
  geom_histogram(aes(x=Year, fill=State)) +
  labs(x="State", y="Total", title='WNV infection frequency in the USA', 
       caption="Data from: the interweb") #the outoput is not informative, facet by year

#Histogram
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(mapping = aes(fill=State)) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Year', y='Total;', title='WNV infection frequency in the USA, a state analysis', caption='Data from the interweb')

#Histogram with a log scale for x axis - cannot force log10 of x axis of a histogram
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(mapping = aes(fill=State)) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Year', y='Total;', title='WNV infection frequency in the USA, a state analysis', caption='Data from the interweb')
  scale_x_log10()

#FACETING HISTOGRAM WITH AXIS LIMITS
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(mapping = aes(fill=State)) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Year', y='Total;', title='WNV infection frequency in the USA, a state analysis', caption='Data from the interweb')

#### Calculate case fatality rate ####

#Calculate case fatilty rate (cfr): #calculated value will appear here, though will not be appended to original dataset
wnv$cfr <- wnv$Fatal/wnv$Total

head(wnv)  # look at first 10 lines of data including cfr

#Below plot has removed mapping statement = it still works in this example
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(aes(x=cfr, fill=State)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x='Case fatality rate', y='Total;', title='WNV infection frequency in the USA, a state analysis', caption='Data from the interweb')

#FACET THE CASE FATALITY RATE HISTOGRAM
#I HAVE ALSO SUBSEQUENTLY CHANGED Y AXIS MAX TO 25 AND 100 WAS TOO MUCH WHEN INITIALLY PLOTTED
ggplot(data=wnv, mapping=aes(x=Total)) +
  geom_histogram(aes(x=cfr, fill=State)) +
  facet_wrap(~ Year) +
  scale_y_continuous(limits = c(0,25)) +
  labs(x='Case fatality rate', y='Total;', title='WNV infection frequency in the USA, a state analysis', caption='Data from the interweb')

#CHANGE YEAR FROM NUMERICAL TO FACTOR
wnv$Year <- as.factor(wnv$Year)

#GRAPH TOTAL NUMBERS BY STATE AND ROTATE X AXIS TITLES (this is total numbers, not cfr)
ggplot(data=wnv, mapping=aes(x=State)) +
  geom_histogram(mapping = aes(x=State, y=Total, fill=Year), stat='identity') +
  labs(x='State', y='Total', title='WNV infection frequency in the USA by state', caption='Data from the interweb') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
#remove negative sign to swap around text - test & rerun code to see!

#GRAPH CASE FATALITY RATIO (CFR) BY STATE AND ROTATE X AXIS TITLES
ggplot(data=wnv, mapping=aes(x=State)) +
  geom_histogram(mapping = aes(x=State, y=cfr, fill=Year), stat='identity') +
  labs(x='State', y='Total', title='WNV case fatality ratio in the USA by state', caption='Data from the interweb') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#### calculate mean and sd ####


#Write a function to calculate the mean and standard error (standard deviation
#divided by the square root of the sample size) of the neuroinvasive disease rate for all the
#states in a given list and given set of years. Follow the Google R style and remember to place
#the function near the top of your script. Use your function to calculate the average severe
#disease rate in California, Colorado, and New York

EncephMen <- c(wnv$EncephMen)

mean <- function(x){
  s <- sum(x)
n <- length(x)
m <- s/n
return (m)} 

mean(EncephMen) #as I have stipulated x= EncephMen in wnv dataset 

sd(EncephMen)

standard.error <- function(x){
  sqrt(var(x)/length(x))
}

standard.error(EncephMen)

#NOW CALCULATE FOR CALIFORNIA, COLORADO, AND NEW YORK ONLY
#NEED TO WORK WITH SUBSET OF DATA

EncephMenCaliforniaSubset <- (data=subset(wnv$EncephMen, wnv$State=='California'))

mean(EncephMenCaliforniaSubset)
sd(EncephMenCaliforniaSubset)
standard.error(EncephMenCaliforniaSubset)

EncephMenColoradoSubset <- (data=subset(wnv$EncephMen, wnv$State=='Colorado'))

mean(EncephMenColoradoSubset)
sd(EncephMenColoradoSubset)
standard.error(EncephMenColoradoSubset)

EncephMenNewYorkSubset <- (data=subset(wnv$EncephMen, wnv$State=='New York'))

mean(EncephMenNewYorkSubset)
sd(EncephMenNewYorkSubset)
standard.error(EncephMenNewYorkSubset)
