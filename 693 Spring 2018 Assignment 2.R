##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights

#other install
#install.packages("dplyr")
library(dplyr)
flights <- as_tibble(flights)

############edit
#3. flights data: numer of flights canceled per day?
flights %>% 
  filter (is.na(dep_delay)) %>% 
  mutate (date=paste(year,month,day,sep="-")) %>% 
  group_by(date) %>% 
  summarize(ncanceled=n())
#is the proportion of canceled flights related to the average delay?
###########

#4. Which carrier has the worst delay?
flights %>% 
  group_by(carrier) %>%
  arrange(desc(dep_delay)) 
#carrier HA has the single worst delay.
flights %>% 
  group_by(carrier) %>% 
  summarize(meandep_delay=mean(dep_delay,na.rm=TRUE)) %>% 
  arrange(desc(meandep_delay))
#carrier F9 has the longest average departure delays

###################edit
#4. challenge
flights %>%
  group_by(carrier,dest) %>%
  summarize(n())
##################

#5. For each plane count the number of flights before the first delay of greater than 1 hr
#############should it be tailnum, date, dep_delay?
flights %>% 
  arrange(tailnum,dep_delay) %>% 
  filter(dep_delay<=60) %>% 
  group_by(tailnum) %>% 
  summarize(n())
  
#6. which plane has the worst ontime flight record?
flights %>% 
  group_by(tailnum) %>% 
  summarize(meanarr_delay=mean(arr_delay)) %>% 
  arrange(desc(meanarr_delay))
#on average, plane N844MH has the worst ontime flight record
flights %>% 
  group_by(tailnum) %>% 
  arrange(desc(arr_delay)) %>% 
  select(arr_delay,tailnum)
#flight N384HA has the single worst on time record

#7.What time of day should you fly if you want to avoid delays as much as possible?
flights %>% 
  group_by(hour) %>% 
  summarize(meandep_delay=mean(dep_delay, na.rm=TRUE)) %>% 
  arrange((meandep_delay))
#7pm through 9pm is the time of day with the most delays
#5am through 7am is the best time to fly for the least delays

#8. For each destination, total minutes of delay?
flights %>% 
  filter(dep_delay>=0) %>% 
  filter(arr_delay>=0) %>% 
  mutate (totaldelay=dep_delay+arr_delay) %>% 
  group_by(dest) %>% 
  summarize(sum(totaldelay))
##################edit
#For each flight, proportion of total delay for its destination?
flights %>% 
  filter(dep_delay>=0) %>% 
  filter(arr_delay>=0) %>% 
  mutate (totaldelay=dep_delay+arr_delay) %>% 
  group_by(dest) %>% 
  summarize(sum(totaldelay))  
  #mutate( summarize(sum(totaldelay)))
###################

###############edit
#9. Look at each destination. Can you find flights that are suspiciously fast?
flights %>% 
  group_by(dest) %>% 
  arrange((air_time))
##############


