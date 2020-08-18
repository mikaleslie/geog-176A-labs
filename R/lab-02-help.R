library(tidyverse)
library(readxl)
library(zoo)
library(knitr)

PopulationEstimates <- read_excel("data/PopulationEstimates.xls", skip = 2)
pop = PopulationEstimates %>%
  select (fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019")
rm(PopulationEstimates)

covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(state == state.of.interest) %>%
  select(fips, county)

inner_join(covid, newCases2, by = "county") ->
  covid2
rm(covid)

left_join(covid2, pop, by = "fips") %>%
  unique() ->
  covid_pop

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


