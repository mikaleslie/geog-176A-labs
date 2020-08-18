install.packages("readxl")
install.packages("kableExtra")

library(tidyverse)
library(readxl)
library(zoo)
library(knitr)
library(ggplot2)
library(ggthemes)

home = read_csv("data/landdata-states.csv")
covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

p2 = PopulationEstimates %>%
  select(fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019")%>%
  group_by(state) %>%
  slice_max(pop2019, n=1)


#Question 1

state.of.interest = "California"

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
pop <- read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select (fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019")


#part 7

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

covid_pop %>%
  filter(date >= max(date)-13) %>%
  group_by(county, pop2019) %>%
  summarize(newCases_p100 =  sum(newCases)) %>%
  mutate(newCases_p100 = newCases_p100 / (pop2019/100000)) %>%
  filter(newCases_p100 < 100) ->
  final

newCases2 %>%
  summarise(tot_cases = sum(cases)) -> x

newCases2 %>%
  summarise(tot_newcases = sum(newCases, na.rm = T)) -> y

paste("The total number of cases is", x,
      "the total number of new cases is", y,
      "and the total number of safe counties is", nrow(final))

#

#Question 2
covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#part 2
covid %>%
  filter(state == "California" | state == "New York" | state == "Florida" | state == "Louisiana") %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = rollmean(newCases, 7, fill = NA, align="right")) %>%
  ungroup() %>%
  ggplot(aes(x = date))+
  geom_col(aes(y = newCases, color = state)) +
  geom_line(aes(y = roll7), size = 1) +
  facet_wrap(~state)+
  theme_dark() +
  labs(title = paste("New Reported Cases by Day in States of Interest"),
       color = "",
       x = "Date",
       y = "") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold'))

#part 3
pop <- read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select (fips = "FIPStxt", state = "Area_Name", pop2019 = "POP_ESTIMATE_2019")

covid_pop = left_join(covid, pop, by = "state")

covid_pop %>%
  filter(state == "California" | state == "New York" | state == "Florida" | state == "Louisiana") %>%
  select(date, state, cases, pop2019) %>%
  group_by(state, date, pop2019) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases),
         newCases_pcap = newCases/pop2019,
         roll7 = rollmean(newCases_pcap, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date))+
  geom_col(aes(y = newCases_pcap, color = state)) +
  geom_line(aes(y = roll7), size = 1) +
  facet_wrap(~state)+
  theme_dark() +
  labs(title = paste("New Reported Cases Per Capita by Day in States of Interest"),
       color = "",
       x = "Date",
       y = "") +
  theme(plot.title = element_text(size = 10, face = 'bold'))

#By looking at the number of cases per capita, some states appear to do
#better than others compared to the raw data. The peak in the CA plot appears
#much smaller, while the peak in the LA plot appears much larger because
#CA's population is much larger. So while CA has more cases than LA, they have
#a smaller ratio to the total population.


