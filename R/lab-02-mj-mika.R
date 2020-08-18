

library(tidyverse)
library(readxl)
library(zoo)
library(knitr)

covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#Question 1

state.of.interest = "California"


## I removed summarize here, mutate is better to add a column
covid %>%
  filter(state == state.of.interest) %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() ->
  newCases2

#Part 3

newCases2 %>%
  filter(date == max(date)) %>%
  arrange(-cases) %>%
  head(5) %>%
  select(county, cases) %>%
  knitr::kable(caption = paste("Most Cumulative Cases", state.of.interest),
               col.names = c("County", "Cumulative Cases"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

newCases2 %>%
  filter(date == max(date)) %>%
  arrange(-newCases) %>%
  head(5) %>%
  select(county, newCases) %>%
  knitr::kable(caption = paste("Most New Cases", state.of.interest),
               col.names = c("County", "New Cases"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#Part 4, 5

## This looked great, but this is more streamline then the rm() call
pop <- read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select (fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019")


## No need to read COVID back in!

#part 7

# Join was Great!
covid_pop = left_join(newCases2, pop, by = "fips")

#part 8

covid_pop %>%
  filter(date == max(date)) %>%
  mutate(cases_pcap = cases/pop2019) %>%
  arrange(-cases_pcap) %>%
  head(5) %>%
  select(county, cases_pcap) %>%
  knitr::kable(caption = paste("Most Cumulative Cases Per Capita", state.of.interest),
               col.names = c("County", "Cumulative Cases Per Capita"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

covid_pop %>%
  filter(date == max(date)) %>%
  mutate(newcases_pcap = newCases/pop2019) %>%
  arrange(-newcases_pcap) %>%
  head(5) %>%
  select(county, newcases_pcap) %>%
  knitr::kable(caption = paste("Most New Cases Per Capita", state.of.interest),
               col.names = c("County", "New Cases Per Capita"),
               format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("bordered", full_width = F, font_size = 14)

#part 9


## Your duplicates were because you have 14 rows for each county. R read that as 14 possible divisions when you divide by pop. A solution is to use pop2019 as a grouping variable.

# If you do that then a summarize is a good call, and the pop2019 can be used following...

covid_pop %>%
  filter(date >= max(date)-13) %>%
  group_by(county, pop2019) %>%
  summarize(newCases_p100 =  sum(newCases)) %>%
  mutate(newCases_p100 = newCases_p100 / (pop2019/100000)) %>%
  filter(newCases_p100 < 100) ->
  final

nrow(final)





