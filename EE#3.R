library(readxl)
library(igraph)
library(data.table)
library(randtests)
library(stringr)
library(Matrix)
library(expm)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library("PearsonDS")
library(plm)
library(pglm)

install.packages('hhi')
library(hhi)

# home wd
#setwd("C:\\Users\\Jent\\Documents\\College\\Emory\\Social Network Analytics\\EE#3")
# school wd
setwd("C:\\Users\\jentl\\Documents\\Emory\\Fall 2021\\Social Network Analytics\\EE#3")

"Loading Data"

border <- fread("border_information.csv")
election <- fread("election_results.csv")
rain <- fread("monthly_rainfall.csv")

# checking column names
head(border) #edge list of districts, 794 unique districts, 413 focal districts
head(election) #election results
head(rain) #rain by district

"1(A)"
"Calculating SPI"

# cleaning data
rain$time <- as.integer(rain$time)
rain <- rain[,list(rain=sum(rainfall)), by=.(district,time)]

# average rain

# calculating scale: yearly variance across districts/yearly mean across districts by year
rain[,scale:=var(rain)/mean(rain),by=time]

# calculating shape: mean(x)^2/yearly variance by year
rain[,shape:=mean(rain)^2/var(rain),by=time]

# calculating qpearsonIII quantile function
rain[,qpear3:=qpearsonIII(p=pnorm(rain, mean=mean(rain), sd=sd(rain)), shape=shape, scale=scale, location=0)]

# calculating SPI
rain[,avg_pear3:=mean(qpear3),by=district]
rain[,sd_pear3:=sd(qpear3),by=district]
rain[,SPI:=(qpear3-avg_pear3)/sd_pear3,by=.(district,time)]

# create election year flag

# joining tables by year, creating election year flag
rain[election, on=c(time='year'),elect_year:=year]

# replace elect_year NA with 0
rain[is.na(elect_year),elect_year:=0]

# creating elect_year_group
unique_years <- unique(rain$elect_year)

for (i in 1:(length(unique_years)-1)){
  rain[unique_years[i]<time & time<= unique_years[i+1],elect_group:=i]
}

rain[is.na(elect_group),elect_group:=0]

# calculate number of parties founded each election-interval

# get list of parties in district in each election, all previous elections

# call setdiff on these lists to find, count the number of different parties

new_party <- data.table(district=character(), year=numeric(), new_parties=numeric())

# need a year zero b/c parties are 'founded' in the first election
elect_years <- c(0,unique(election$year))

for (i in 1:(length(elect_years-1))){
  # list of new parties 
  temp1 <- setdiff(election[year==elect_years[i+1],party_name,by=district],
                   election[year<=elect_years[i],party_name,by=district])
  # number of new parties by district
  temp1[,new_parties:=length(unique(party_name)),by=district]
  # creating new party table
  temp2 <- election[year==unique(election$year)[i]][temp1, on='district', new_parties:=new_parties]
  new_party <- rbind(new_party,temp2[,.(district,year,new_parties)])
}

# joining new_party to rain by district and year
rain[new_party, new_parties:=i.new_parties, on=c(time='year',district='district')]

# fixing na's
rain[is.na(new_parties),new_parties:=0]

"1(A).
Create a scatter plot, showing relationship between 
average SPI in a district across each year from the prior election or first election up to the current election, and 
the number of political parties that are founded in a district. 
Consider the party to be 'founded' if it has not competed in an election in this district prior to this election. X axis is avg SPI for a district in an election-interval, y axis is num parties founded"

# calculate average SPI by district by election interval
rain[,avg_SPI:=mean(SPI),by=.(district,elect_group)]

# table for SPI 
rain_plot <- unique(rain[,.(avg_SPI,new_parties),by=.(district,elect_group)])

# using ggplot, adding geom_smooth trend line
ggplot(rain_plot, aes(avg_SPI,new_parties))+geom_point()+xlab("Average SPI")+ylab("Number of New Parties")+ggtitle("Number of New Indian Political Parties vs. SPI by District each Year ")+theme_classic()

"1(B).
Using the election-period level rainfall measures created above, show that the SPI is
not independent from on election period to the next within a district, as well as from
neighboring districts from one election period to the next.

It is possible to show this relationship by regressing a district's current level of the rainfall
variable on
(1) its lagged value and
(2) the lagged value of its neighbor's rainfall variable.
For computing the neighbors' value, you can use an average of each of the surrounding district's values.

Include a control in the regression for the number of years in the election period, and
use a fixed effects specification to control for the time-invariant features of a district
as well as a control for each election period.
This can be accomplished using the plm package, using a model specified in the form of
plm(outcome variable∼predictor variables, data, effect='twoways', model='within', index='district'), where 'twoways' and 'within' provide both sets of fixed effects."

# setting up data table
rain_short <- rain[time<=1999,.(district,time,elect_group,avg_SPI)]
rain_short <- unique(rain_short)

# creating neighbor SPI table to join to rain_reg afterwards
neighbors_SPI <- data.table(district=character(), V1=numeric(), elect_group=numeric())

for (d in unique(rain$district)){
  # getting district neighbors
  neighbors <- border[focal_district==d,district]
  # calculating mean SPI for each election cycle
  temp <- rain[district %in% neighbors, mean(SPI), by=.(elect_group)]
  temp$district <- d
  neighbors_SPI <- rbind(neighbors_SPI,temp)
}

# joining neighbors SPI with rain short table
rain_short[neighbors_SPI,on=c('district','elect_group'),n_SPI:=i.V1]

# getting lag values
rain_short[,lag_SPI:=avg_SPI-shift(avg_SPI),by=district]
rain_short[,lag_n_SPI:=n_SPI-shift(n_SPI),by=district]

# next task: get election-period number of years
years <- data.table(elect_years,elect_years-shift(elect_years))
years <- years[2:.N,]
setnames(years,'V2','year_gap')
years <- years[-1,]
years$elect_group <- 1:13

# joining with rain_short table
rain_short[years,on='elect_group',year_gap:=i.year_gap]
rain_short[is.na(year_gap),year_gap:=0]

# cleaning up non-election entries
rain_short <- rain_short[elect_group!=0]

# running the regression
plm(avg_SPI~lag_SPI+lag_n_SPI+year_gap, data=rain_short, effect='twoways', model='within', index='district')

"
1(C).
moderate droughts occur if SPI falls below -1,
moderate floods to occur if it rises above 1. 

Create a measure that sums the number of years a district experiences either moderate droughts
or floods during the interval starting from the year following the previous election up
until the year of the current election. 

Perform the same test as in (B), using this new transformed measure. 

Since this is a count outcome that is reported as a discrete number of years, use a regression adopted for data of this form—this can be accomplished with the pglm package, using a model specified
in the form of pglm(outcome variable ∼predictor variables, data, effect =
'twoways', model = 'within', index = 'district', family = 'poisson'). 
What differences do you see between the estimates?
"

# creating drought-flood flag
rain[SPI>1 | SPI<1,dr_fl:=1]
rain[is.na(dr_fl),dr_fl:=0]

# counting num years drought-flood per election cycle
rain[,sum_dr_fl:=sum(dr_fl), by=.(district,elect_group)]

# joining sum_dr_fl with rain_short
rain_short[rain,on=c(time='time',district='district'),sum_dr_fl:=i.sum_dr_fl]

# running into lots of errors, replacing NA's with 0
rain_zero <- rain_short
for (i in seq_along(rain_zero)) set(rain_zero, i=which(is.na(rain_zero[[i]])), j=i, value=0)

# running regression
pglm <- pglm(avg_SPI~lag_SPI+lag_n_SPI+year_gap+sum_dr_fl, data=rain_zero, effect='twoways', model='within', index='district', family='poisson')

# interesting results!
pglm$estimate

"2(A). 
Run a regression predicting the number of new political parties that are formed as a function of the number of years a district experiences droughts or flooding
in the interval starting from the year following the previous election up until the year of the current
election. 
The number of new political parties that enter a district is a discrete count outcome, 
so we should use a regression format adopted for counts, as in (1C). 
Include a control in the regression for the number of years in the election period, 
and a control for the time-invariant features of a district, as in Question 1. 

Are certain kinds of political parties, based on the issues they cater to, more likely to be formed when a district experiences extreme weather?"

# calculate number of years of dr_fl per district per election interval: sum_dr_fl
# number of new parties formed: rain$new_parties
# joining to 'rain zero'
rain_zero[rain,on=c(district='district',time='time'),new_parties:=i.new_parties]

# running regression
pglm <- pglm(new_parties~sum_dr_fl+year_gap, data=rain_zero, effect='twoways', model='within', index='district', family='poisson')

pglm$estimate

# reality check: level of dr_fl by new_parties
ggplot(rain_zero,aes(factor(sum_dr_fl),new_parties))+geom_point()

"it appears there are more new parties created after droughts/floods than when there are no droughts/floods"

# Are certain kinds of political parties, based on the issues they cater to, more likely to be formed
# when a district experiences extreme weather?"

unique(election$party_issue) # there are 7 party issues

# join party_issue, district, and dr_fl by year
issues <- rain[,.(district,time,elect_group,new_parties,dr_fl)]
issues[election, on=c(time='year',district='district'),party_issue:=i.party_issue]

# remove rows w/o new parties
issues <- issues[!new_parties==0,]

# remove rows with NA party issue
na.omit(issues,cols='party_issue')

# how many parties of each issue are formed?
issues_group <- issues[,sum(new_parties),by=party_issue]

# removing blank issue
issues_group <- issues_group[-2,]

# change col name
setnames(issues_group,'V1','count')

# proportion of each issue
issues_group[,proportion:=count/sum(count)]

"liberal and far left groups were most likely to be formed in the aftermath of floods & droughts"

"
2(B). 
look at how political activity stimulated by droughts or floods in one district might affect political activity in another district.

Use a similar regression to (A) to show that, 
even when taking into account a district’s own droughts and floods, 
that district’s degree of political foundings will also depend
on the number of years its neighboring districts experience years of droughts or flooding
in the interval starting from the year following two elections ago, up until the year of
the previous election—the election lead-up interval before the current one. 
Include a control in the regression for the number of years in the current election period, 
and a control for the time-invariant features of a district."

# get neighbors num_years of dr_fl (sum_dr_fl) 

# creating neighbor dr_fl table to join to rain_zero afterwards
neighbors_dr_fl <- data.table(district=character(), V1=numeric(), elect_group=numeric())

for (d in unique(rain_zero$district)){
  # getting district neighbors
  neighbors <- border[focal_district==d,district]
  # calculating mean SPI for each election cycle
  temp <- rain_zero[district %in% neighbors, sum(sum_dr_fl), by=.(elect_group)]
  temp$district <- d
  neighbors_dr_fl <- rbind(neighbors_dr_fl,temp)
}

# joining neighbors_dr_fl with rain_zero table
rain_zero[neighbors_dr_fl,on=c('district','elect_group'),n_sum_dr_fl:=i.V1]

# lag neighbors sum_df_fl from 2 elect_group ago to previous elect_group
rain_zero[,twolag_n_sum_dr_fl:=shift(n_sum_dr_fl,n=2),by=.(district,elect_group)]

# replacing NA with zeros in the shifted 
rain_zero[is.na(twolag_n_sum_dr_fl),twolag_n_sum_dr_fl:=0]

# running regression
pglm <- pglm(new_parties~sum_dr_fl+year_gap+twolag_n_sum_dr_fl, data=rain_zero, effect='twoways', model='within', index='district', family='poisson')

pglm$estimate

# checking vs. grouped dt
test <- rain_zero[,.(sum_dr_fl,year_gap,twolag_n_sum_dr_fl,new_parties),by=.(district,elect_group)]

pglmt <- pglm(new_parties~sum_dr_fl+year_gap+twolag_n_sum_dr_fl, data=test, effect='twoways', model='within', index='district', family='poisson')

pglmt$estimate # exact same results!

"3. 
Extreme weather events like droughts or floods can erode the stability of political systems and
wear away at the entrenched power bases of large, national-scale parties that have difficulty
responding to the needs of affected regions.

Perform a regression similar to Question 2(B) to determine whether experiencing droughts
or floods relates to political concentration. 

The Herfindahl Index, or HHI, is a measure of political concentration that measures the degree to which a few parties command the majority of the shares of votes in each election:

Herfindahl =n∑i (vote share_i)^2

where the vote share is 
the count of votes divided by the total number of votes received by all candidates 
in the election in this district in this election period. 

The HHI can be computed with the HHI package using the hhi() function.

What does this regression illustrate in terms of the HHI’s concentration or fragmentation of
political power in districts affected by extreme weather?
"
# calculate vote_share
# assumption: vote_share is by party

# first getting total votes
election[,total_votes:=sum(vote_count),by=.(district,year)]

# getting vote_share
election[,vote_share:=vote_count/total_votes,by=.(party_name,district,year)]

# grouping election by district, year, party_name
votes <- election[,sum(vote_share),by=.(district,year,party_name)]
setnames(votes,'V1','vote_share')

# making sure vote-share totals to 100
votes[,sum(vote_share),by=.(district,year)] # it does!

# what do I need to run hhi on?
test <- votes[district=='Sirohi Pali' & year==1951,.(party_name,vote_share)] #setting columns doesn't seem to matter
test$vote_share <- test$vote_share*100
hhi(test,'vote_share')# throws an error message even when shares sum to 100, 

# for each year, for each district, calculate hhi
# save this in a dt with district, year, hhi
# join with votes
# run regression
