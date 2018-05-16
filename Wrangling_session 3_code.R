####Lyme disease cases. Our scientific programming goals are to
#Import the data sets
#Convert data to the tidy data format
#Identify and manipulate text strings with the regexp language
#Merge data sets
#Visualize geographic variables as a map ####

dir.create("Wrangling") #creates a new folder in My Docs
setwd('~/./Wrangling') #changes file save location

#Import required libraries for this session
#Install and load libraries first as it contains commands to import a tibble-style dataframe
install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("stringr")
install.packages("GGally")
install.packages("maptools")
install.packages("ggmap")
install.packages("maps")
library('tidyverse')
library('magrittr')
library('dplyr')
library('stringr')
library('GGally')
library('maptools')
library('ggmap')
library('maps')

#import csv files as a tibble-style dataframe (df) (underscore rather than dot):
#The read_csv will create tibble versions of data frames, which retain all the good things about
#data frames, but not some of the less good things (more here on tibbles: https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html)
pop <- read_csv('pop.csv')
ld <- read_csv('lyme.csv')
prism <- read_csv('climate.csv')

#now checking first 6 lines of each tibble df
head(pop)
head(ld)
head(prism)

#### modify pop df ####

#convert pop data to tidy format (Currently, only the PRISM data conforms to the concept of tidy data)
#Each variable has its own column, Each observation has its own row, Each value has its own cell
head(pop)

pop %<>% select(fips,starts_with("pop2")) #looking at populations in the 2000s
pop #run this to see how table has been modified, first 10 lines will be displayed
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit #removes anything with NAs, gathers data from the 2000s, FIPS, pop size. Will transpose data and organise into columns
pop
pop %<>% mutate(year=str_replace_all(str_year,"pop","")) #Removing pop from str_year: current format is pop2000, adds additional column
pop
pop %<>% mutate(year=as.integer(year)) #change year from character to integer (number)
pop
pop %<>% mutate(fips=str_replace_all(fips,"^0","")) #will remove zero at beginning of FIPS codes
pop
pop %<>% mutate(fips=as.integer(fips))#change year from character to integer (number)
pop

#%<>% = will perform task/changes within df, then apply the modifications to the df

#### modify ld df ####
#You are going to want a column that has the full FIPS codes for each county, as explained above. You
#should write a function that takes two arguments: state code and county code. The function should
#return a 4 or 5 digit FIPS code (integer data type). For county codes that are 1 or 2 digits, you'll
#need to 'pad' them with the appropriate number of zeros (this will require an if-else flow control). You
#can determine how long a character string is using the str_length function of the stringr package
#(e.g. str_length("disease")=7). As you might expect, the command to paste two character strings
#together is paste (remember, you can get help with ?paste). To apply your function to every cell you
#can use the rowwise function which is part of the dplyr package (as used in the presentation example).

#Use 'rename' to rename the columns "STNAME" and "CTYNAME" with "state" and "county",
#respectively (useful for a later join-operation for mapping)
ld
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases") #selects data in str_year that begins with cases, and place into a column format rather than row)
ld
ld %<>% mutate(year=str_replace_all(str_year,"Cases","")) #Removing cases from str_year: current format is Cases2000, adds additional column
#IE - replace something with nothing

ld
ld %<>% mutate(year=as.integer(year))  #change year from character to integer (number)
ld
ld %<>% rename(state=STNAME,county=CTYNAME) #rename STNAME to state, and CTYNAME to county
ld

fips.builder<-function(st,ct){ #state (st) FIPS = 2 numbers, county (ct) = 3 numbers - this will combine STCODE and CTYCODE
  if (str_length(ct)==3){ #specifying that if county string is 3 numbers
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer #if county is 3 numbers, then paste state and county FIPS codes together as an integer, without spaces (sep=separator)
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer #if county is 2 numbers for county code, then append 0 to the start of the string of numbers for county
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer #if the county code is not 1 digits, append 00 to beginning of county string
  }
  return(fips)
} #above calculation has been performed, now add to tibble

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))
ld
ld %<>% select(-c(STCODE,CTYCODE,str_year)) #cleaning tibble by removing unwanted columns

?merge #? will provide information on a command

#Base R has a merge function that works well in combining data frames, and we encourage you to familiarize
#yourself with it (?merge). The dplyr package has a set of join functions that operate on tibbles. This is the
#method we are using in this workshop.

#### MERGING TIBBLES (DATAFRAMES) ####

ld.prism <- inner_join(ld,prism) #merging ld and prism tibbles #precip and avtemp from prism #year and fips are common columns
ld.prism #view joined tibble
ld.prism.pop <- inner_join(ld.prism,pop) #now merges above with the pop tibble
ld.prism.pop #view joined tibble
pop
ld
prism

#str_year should be removed from df, but it is NOT! Remove column from ld.prism.pop manually.
ld.prism.pop %<>% select(-c(str_year)) #cleaning tibble by removing unwanted columns
ld.prism.pop

#### OBTAINING SUMMARY INFORMATION ####
#Task 7: Write two lines of code that create two new data frames: (1) to determine how many cases of Lyme
#disease were reported each year, (2) the average number of cases in each state - averaged across county and
#year. What was the worst year? Which three states have been most impacted on average?

#After grouping data with group_by, there may be a need to return to a non-grouped form. Running ungroup() will drop any grouping. This can be reinstated again with regroup(). 

cases_by_year <- ld %>% ungroup %>% group_by(year) %>% #ungroups ld from the combined tibble (df), then will look at # of cases per year
  summarize(total=sum(cases)) %>% arrange(desc(total)) #summarises total # of cases, arranging them in descending order (of cases reported)

cases_by_year #view new tibble

total_number_cases_State <- ld %>% ungroup %>% group_by(state) %>% #total number of cases per state - though want average number of cases
  summarize(total=sum(cases)) %>% arrange(desc(total))
total_number_cases_State

average_number_cases_State <- ld %>% ungroup %>% group_by(state) %>% #want to code for number of year, not to manually input year
  summarize(total=sum(cases/16)) %>% arrange(desc(total))
average_number_cases_State

average_number_cases_State <- ld %>% ungroup %>% group_by(state) %>%
  summarize(mean=(cases)) %>% arrange(desc(total))
average_number_cases_State

average_number_cases_StateCY <- ld %>% ungroup %>% group_by(state, county, year) %>% #average cases by state, county and year
  summarize(total=mean(cases)) %>% arrange(desc(total))
average_number_cases_StateCY

#### SAVING DATA FRAMES/TIBBLES AS OBJECTS/FILES ####
#Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same
#(remember to try ?save and ?write_csv in the console if you're not sure how these functions work).

?save

?write_csv
#x	A data frame to write to disk
#path	Path or connection to write to.
#delim	Delimiter used to separate values. Defaults to " ". Must be a single character.
#na	String used for missing values. Defaults to NA. Missing values will never be quoted; strings with the same value as na will always be quoted.
#append	If FALSE, will overwrite existing file. If TRUE, will append to existing file. In both cases, if file does not exist a new file is created.
#col_names	Write columns names at the top of the file?

save(ld.prism.pop, file = "ld_prism_pop.RData") #saves ld.prism.pop file as an .RData file (R workspace)
write_csv(ld.prism.pop, "ld_prism_pop.csv") #saves ld.prism.pop file as a csv file

#### USING FIPS AND BUILT IN MAPPING TOOLS TO VISUALISE GEOGRAPHICAL DATA ####
#Task 9: Add annotations to the following lines of code with comment lines to explain what they are achieving.
#Note: in the code line with "group_by(ld.prism.pop)" you need to replace the data frame in parentheses with
#the name of your data frame in which you combined Lyme disease, climate and demography data (Task 6)

library(ggplot2)

county_map <- map_data("county")
state_map <- map_data("state")

ld.prism.pop #just checking tibble (ie - dataframe)
ag.fips <- group_by(ld.prism.pop,fips) #group information in ld.prism.pop by FLIPS numbering
ld.16y<-summarize(ag.fips,all.cases=sum(cases)) #summarises ag.flips datafile generated on above line of code = summarises sum of cases
ld.16y #look at ld.16y datafile
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y) #adds state and county columns to the tibble
ld.16y
ld.16y<-distinct(ld.16y) #I AM UNSURE WHAT THIS DOES!!!!
ld.16y
ld.16y %<>% rename(region=state,subregion=county) #rename state to region, and county to subregion
#ld %<>% rename(state=STNAME,county=CTYNAME) #rename STNAME to state, and CTYNAME to county - this is an example from above
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","") #removes the term country from subregion
ld.16y
ld.16y$region<-tolower(ld.16y$region) #add d.16y$region info to a new file, called ld.16y$region (data from region column in ld.16y file)
ld.16y
ld.16y$subregion<-tolower(ld.16y$subregion) #add d.16y$subregion info to a new file, called ld.16y$subregion (data from subregion column in ld.16y file)
ld.16y %<>% mutate(log10cases=log10(1+all.cases)) #transforms data into log10 scale
map.ld.16y<-left_join(county_map,ld.16y) #adds lat & long info to the tibble
map.ld.16y
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
