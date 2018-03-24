##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)

library(nycflights13)

## dataset for assignment
flights

#other install
install.packages("dplyr")
library(dplyr)
flights <- as_tibble(nycflights13)

#3. flights data: numer of flights canceled per day?
flights %>% 
  filter (distance==0)

#4. dep delay
flights %>% 
  arrange(desc(dep_delay)) %>%
  group_by(carrier) 

#4. challenge
flights %>%
  group_by(carrier,dest) %>%
  summarize(n())

#6. which plane has the worst ontime flight record?
flights %>% 
  group_by(tailnum) %>% 
  summarize(meanarr_delay=mean(arr_delay)) %>% 
  arrange(desc(meanarr_delay))
#on average, plane N844MH has the worst ontime flight record
flights %>% 
  group_by(tailnum) %>% 
  arrange(desc(arr_delay))
#flight N384HA has the single worst on time record

