library(patchwork)
library(tidyverse)

# Fetch data (subtract 1 from date if #s for day aren't released yet)
dt <- Sys.Date()
dateFormat = "%m/%d/%y"
curDate <- gsub("^0", "", gsub("/0", "/", format(dt, dateFormat)))
tsCases <- read_csv(file="https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

seq(dt-7, by = "day", length.out = 7) %>%
  mapply()

# Cleanup col names, pivot to longer, and sort
dateCols <- grepl(pattern = "\\d", x = colnames(tsCases))
names(tsCases)[!dateCols] <- make.names(names(tsCases)[!dateCols])
tsCases <- 
  tsCases %>%
  select(-Lat, -Long) %>%
  pivot_longer(-c(Province.State, Country.Region), names_to = "Date", values_to = "Count") %>%
  arrange(Country.Region, Province.State, as.Date(Date, format = dateFormat))

# data for top 10 countries today
topCountries <- tsCases %>%
  filter(Country.Region != "China") %>%
  filter(Date == curDate) %>%
  group_by(Country.Region) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE) %>%
  arrange(-Count) %>%
  head(10)

# data for top states today
topStates <- tsCases %>%
  filter(Country.Region == "US") %>%
  filter(Date == curDate) %>%
  group_by(Province.State) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE) %>%
  arrange(-Count) %>%
  head(10)

# top countries today
topCountriesPlot <- topCountries %>%
  ggplot(aes(x = Count, y = reorder(Country.Region, Count))) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("") +
  theme(legend.position="left") +
  scale_y_discrete(position = "right")

# top states today
topStatesPlot <- topStates %>%
  ggplot(aes(x = Count, y = reorder(Province.State, Count))) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("") +
  theme(legend.position="none")

# top countries over time
countriesTimeseriesPlot <- tsCases %>%
  filter(Country.Region %in% topCountries$Country.Region) %>%
  group_by(Country.Region, Date) %>%
  summarize(Count = sum(Count)) %>%
  filter(Count > 100) %>%
  arrange(Country.Region, as.Date(Date, format = dateFormat)) %>%
  ggplot(aes(x = reorder(Date, Count), y = Count, group = Country.Region, color = Country.Region)) +
    geom_line() +
    theme(legend.position="left",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())

# top states over time
statesTimeseriesPlot <- tsCases %>%
  filter(Province.State %in% topStates$Province.State) %>%
  group_by(Province.State, Date) %>%
  summarize(Count = sum(Count)) %>%
  filter(Count > 100) %>%
  arrange(Province.State, as.Date(Date, format = dateFormat)) %>%
  ggplot(aes(x = reorder(Date, Count), y = Count, group = Province.State, color = Province.State)) +
    geom_line() +
    xlab("Date") +
    theme(legend.position="left",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# Data for growth rate
growth <- tsCases %>%
  filter(Country.Region %in% c("US", "Italy")) %>%
  group_by(Country.Region, Date) %>%
  summarize(Count = sum(Count)) %>%
  filter(as.Date(Date, format = dateFormat) > as.Date("2020-02-29")) %>%
  arrange(Country.Region, as.Date(Date, format = dateFormat)) %>%
  mutate(Growth = Count/lag(Count) - 1)

growthPlot <- growth %>%
  ggplot(aes(x = reorder(Date, Count), y = Growth, group = Country.Region, color = Country.Region)) +
  geom_line() +
  stat_smooth(method="lm") +
  theme(legend.position="none",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())

# layout graphs
(topCountriesPlot | growthPlot) /
  countriesTimeseriesPlot
