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

#3. flights data: numer of flights canceled per day?
canceled<-(flights %>% 
  filter (is.na(dep_delay)) %>% 
  mutate (date=paste(year,month,day,sep="-")) %>% 
  group_by(date) %>% 
  summarize(ncanceled=n()))
#Is there a pattern?
##There doesn't appear to be a pattern based on day of the week.
#is the proportion of canceled flights related to the average delay?
avgdelay<-(flights %>% 
  mutate (date=paste(year,month,day,sep="-")) %>% 
  group_by(date) %>% 
  summarize(meandep_delay=mean(dep_delay,na.rm=TRUE)))
#Yes, The more flights canceled in a day the longer average delay.

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


#4. challenge: bad airports vs bad carriers
flights %>%
  group_by(carrier,dest) %>%
  summarize(n())
flights %>% 
  group_by(dest, origin, carrier) %>% 
  summarize(avgdep_delay=mean(dep_delay,na.rm=TRUE)) %>% 
  arrange(dest, origin, desc(avgdep_delay))
#yes, when looking at the average delay by carrier and destination,
#you can see that some carriers have much higher delays than others even when the
#destination and origin airport is the same. Carrier EV tends to have higher delays.


#5. For each plane count the number of flights before the first delay of greater than 1 hr
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
  filter(dep_delay>=0 & arr_delay>=0) %>% 
  mutate (totaldelay=dep_delay+arr_delay) %>% 
  group_by(dest) %>% 
  summarize(sum(totaldelay))
#For each flight, proportion of total delay for its destination?
flights %>% 
  filter(dep_delay>=0 & arr_delay>=0) %>% 
  mutate (totaldelay=dep_delay+arr_delay) %>% 
  group_by(dest) %>% 
  summarize(sum(totaldelay))


#9. Look at each destination. Can you find flights that are suspiciously fast?
flights %>% 
  filter(dest=="BDL") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="HNL") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="PHL") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="BOS") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="IAH") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="MIA") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="ATL") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="ORD") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="SFO") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="RSW") %>% 
  arrange((air_time))
flights %>% 
  filter(dest=="DFW") %>% 
  arrange((air_time))
#BDL,PHL,BOS all have very short air times of <30 minutes
#compute the air time of a flight relative to the shortest flight to that destination.
flights %>% 
  filter(dest=="BDL") %>% 
  arrange(desc(distance))
#which flights were most delayed in the air?
flights %>% 
  mutate(airdelay=arr_delay-dep_delay) %>% 
  arrange(desc(airdelay))
#flight 399 was delayed the most, 196 minutes

