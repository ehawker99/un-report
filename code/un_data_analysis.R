library(tidyverse)

#Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

#What is the mean life expectancy? 
#using the gapminder_data data set, make a new variable called averagelifeExp
#that is equal to the mean of the variable lifeExp
summarize(gapminder_data, averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

#What us the mean population in the gapminder data set?
mean(gapminder_data$pop)

gapminder_data %>% 
  summarize(averagePop = mean(pop))

#What is the mean population and the mean life expectency
gapminder_data %>% 
  summarize(averagePop = mean(pop), 
            averagelifeExp = mean(lifeExp))

#What is the mean life expectancy for the most recent year? 
#Find the most recent year:
gapminder_data %>% summarize(maxYear = max(year))
#Filter for 2007 and take the mean of only obs from 2007
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#to do it all in one step
#filter for the most recent year and then take the mean of only obs from the max year
gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#What is the mean GDP per capita for the earliest year?
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(meanGDP = mean(gdpPercap))

#What is the mean life expectancy for each year?
#group_by()

gapminder_data %>% 
  group_by(year) %>% #group_by() does not change the data frame, it just adds metadata that R knows it is grouped by year
  summarize(meanLifeExp = mean(lifeExp)) #ran mean on each year group from the metadata rather than whole dataset

#What is the mean lufe expectancy for each continent?

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp)) #ran mean on each continent group from the metadata rather than whole dataset

#What is the mean life expectancy and mean GDP for each continent in a single result tibble
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp), 
            meanGDP = mean(gdpPercap))

#What is the GDP NOT per capita?
#mutate() makes a new column in the dataset

gapminder_data %>% 
  mutate(gdp = gdpPercap*pop) #for each row in the dataset, it makes a variable called gdp that is gdpPercap*pop for that row

#Make a new column for population in millions
gapminder_data %>% 
  mutate(popInmillions = pop/1000000)

#Save these new variables to thew data set
gapminder <- gapminder_data %>% 
  mutate(popInmillions = pop/1000000,
         gdp = gdpPercap*pop)

#select() chooses a subset of columns from a dataset
gapminder_data %>% 
  select(year, pop)

#Select every column that is NOT continent
gapminder_data %>% 
  select(-continent)

#Create a tibble with only country, continent, year, lifeExp
gapminder_data %>% 
  select(-gdpPercap, -pop)

#select helper function: starts_with()
gapminder_data %>% 
  select(year, starts_with("c")) #will select columns with year, and every column where the varible name starts with c


#Vectors: a vector is a list of values that are all of the same type
#c()
char_vec <- c("dog", "cat", "horse")
num_vec <- c(1, 2, 3, 4)

proof <- gapminder_data %>% 
  pull(year) #pulls a column out of a dataframe, 
#will pull that column into the values section, proving each column in a dataframe is a vector

your_data %>% 
  filter(id %in% c("id1", "id2", "id3") #Filter for ids that are in this vector
         
         
#pivot_longer() and pivot_wider()
#long data = one observation per row
#wide data = NOT one observation per row, rows are values of another variable


data_wider %>% 
  pivot_wider(
    names_from = year,
    values_from = cases
  )

data_longer %>% 
  pivot_longer(
    cols = 1999:2002
    names_to = year,
    values_to = cases
  )


gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(
    names_from = year, #make a column for each year, pivot wider on year
    values_from = lifeExp #populate it from values of lifeExp
  )

#pivot_wider, populate values with gdpPercap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(
    names_from = year, #make a column for each year, pivot wider on year
    values_from = gdpPercap
  )

#pivot_longer
gapminder_data %>% 
  pivot_longer(
    cols = c(pop, lifeExp, gdpPercap),
    names_to = "measurement_type",
    values_to = "measurement"
  )

#Is there a relationship between GDP and CO2 emissions?
#filter fr=or year 2007 and continent Americas
#remove the year and continent columns

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

#Read in the CO2 data, which is present in the data directory in the RProject we are working in
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series",
                      "value", "footnotes", "source")) 
#use skip = 1 to indicate to skip row 1 because column headers are in row 2
#use col_names = c() to name the columns of the dataset you're importing

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series,
              values_from = value) %>%
  filter(year == 2005) %>% 
  select(-year)
#re-name the names for the series variable
#pivoting the dataset to have each value of the series variable as its own column, populated with that series' value data
#so you go from having each row having its own series and value of that series
#to having a column for series that is populated with the values
#filter for year == 2005 and then remove year column after we have filtered


#inner_join
inner_join(gapminder_data_2007, co2_emissions, by = "country")



#Is there a relationship between GDP and CO2 emissions?

  




