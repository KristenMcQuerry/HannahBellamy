##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights

##arrival delay of 2 or more hours
flights %>% 
  filter(arr_delay >= 120.0)

##few to houstion 
flights %>% 
  filter (dest == "IAH" | dest== "HOU")

##operated by united, american, delta
flights %>% 
  filter (carrier == "UA"|carrier=="AA"|carrier=="DL")

##departed july august or september
flights %>% 
  filter(month>=7 & month<=9)

##arrived more than 2 hours late, but didn't leave late
flights %>% 
  filter(sched_arr_time>120 & dep_delay<=0)

##were delayed at least an hour, but made up over 30 minutes in flight
flights %>% 
  filter(dep_delay>=60) %>%
  mutate (timemadeup=dep_delay-arr_delay) %>% 
  filter(timemadeup>30)

##departed midnight through 6am
flights %>% 
  arrange(dep_time) %>% 
  filter(dep_time<=600)
  
##sort to find most delayed flights. find the flights that left the earliest
flights %>% 
  arrange(desc(dep_delay))
flights %>% 
  arrange(dep_delay)

##fastest flights
flights %>% 
  mutate(speed=distance/air_time) %>% 
  arrange(desc(speed))

##longest flights, shortest flights
flights %>% 
  arrange(desc(distance))
#longest flights were JFK to HNL
flights %>% 
  arrange (distance)
#shortest flights were EWR to PHL