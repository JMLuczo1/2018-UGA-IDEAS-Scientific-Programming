dir.create("mers")
setwd('~/./mers')
mers <- read.csv('cases.csv')
#INSTALL PACKAGES FIRST, THEN LOAD LIBRARY

mers$hospitalized[890] <- c('2015-02-20')
head(mers)
mers <- mers[-471,]
#install.packages("lubridate" and "ggplot2)
library(lubridate)
library(ggplot2)

mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
day0 <- min(na.omit(mers$onset2))
mers$epi.day <- as.numeric(mers$onset2 - day0)

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x="Epidemic day", y="Case count", title='Global count of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x="Epidemic day", y="Case count", title='Global count of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#CHANGED Y AXIS MAX TO 15
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  ylim (0, 15) +
  labs(x="Epidemic day", y="Case count", title='Global count of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#CHANGED COLOUR PALETTE
require(RColorBrewer)
myPlot <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x="Epidemic day", y="Case count", title='Global count of MERS cases by date of symptom onset', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
myPlot + scale_fill_brewer(palette="PRGn")

#CALCULATE THE ACTUAL  INFECTIOUS PERIOD (RAW DATA)
mers$infectious.period <- mers$hospitalized2-mers$onset2

#CHECK WHAT CLASS OF DATA THE INFECTIOUS PERIOD IS CLASSIFIED AS
class(mers$infectious.period)

result = "difftime", which means time intervals/differences

#NOW, SPECIFY THAT THE UNITS ARE DAYS. THIS COMMAND ALSO CHANGES A FACTOR INTO A NUMBERIC (as.numeric)
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x="Infectious period", y="Frequency", title="Distribution of calculated MERS infectious period", caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
 
#CHANGE X AXIS RANGE:
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  xlim (-100,150) +
  labs(x="Infectious period", y="Frequency", title="Distribution of calculated MERS infectious period", caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#IFELSE CAN BE USED TO CONSTRUCT A VECTOR WITH TRUE/FALSE WHEN PERIOD < 0
#calculated infectious period in the case where it is positive and zero otherwise
mers$infectious.period2 <-ifelse(mers$infectious.period<0,0,mers$infectious.period)

#THIS CODE WILL EXCLUDE VALUES <0
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period (positive values only)', caption='Data')

#CHANGE PLOT TYPE TO DENSITY PLOT
ggplot(data=mers) +
  geom_density(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#RATHER THAN DENSITY PLOT, CAN GRAPH THE PLOT AREA (SIMPLY, BOXES ARE FILLED IN)
#THE DATA NEEDS TO BE BINNED TO PLOT (DISCREET INTERVALS)
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#NOW CHANGE TO DOT PLOT
ggplot(data=mers) +
  geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#PLOT AREA, BUT RATHER THEN INFECTIOUS PERIOD AND FREQUENCY, PLOTTING INFECTIOUS PERIOD AND
#THE DATA NEEDS TO BE BINNED TO PLOT (DISCREET INTERVALS)
#I HAVE ALSO CHANGED THE Y AXIS MAX TO 40
#THIS CODE BUILDS UPON THE ABOVE, THOUGH SMOOTHING THE LINE
ggplot(data=mers) +
  geom_smooth(aes(x=epi.day, y=infectious.period2), method="loess") +
  labs(x='Day', y='Infectious Period', title='Probability density for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#PLOT AREA, BUT RATHER THEN INFECTIOUS PERIOD AND FREQUENCY, PLOTTING INFECTIOUS PERIOD AND
#THE DATA NEEDS TO BE BINNED TO PLOT (DISCREET INTERVALS)
#I HAVE ALSO CHANGED THE Y AXIS MAX TO 40
ggplot(data=mers) +
  geom_point(aes(x=epi.day, y=infectious.period2, colour=country)) +
  ylim(0,40) +
  labs(x='Day', y='Infectious Period', title='Probability density for MERS infectious period (positive values only)', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#FACETING THE ABOVE GRAPH
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(colour=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious Period', title='MERS infectious period (positive values only) over time', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#ONLY GRAPH CERTAIN COUNTRIES (FACETING OF THE GRAPHS ALSO)
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping = aes(x=epi.day, y=infectious.period2, colour=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious Period', title='MERS infectious period by gender and country', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')
  
#DATA EXPLORATION
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping = aes(x=epi.day, y=infectious.period2, colour=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious Period', title='MERS infectious period by gender and country', caption='Data from: https://github.com/rambaut/MERS???Cases/blob/gh???pages/data/cases.csv')

#CALCULATE CASE FATALITY IN MERS DATASET
#IFELSE CAN BE USED TO CONSTRUCT A VECTOR WITH TRUE/FALSE WHEN PERIOD < 0
#calculated infectious period in the case where it is positive and zero otherwise

class(outcome)

