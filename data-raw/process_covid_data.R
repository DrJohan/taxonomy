## Script to process COVID data 

## Data obtained from: https://ourworldindata.org/coronavirus-source-data

## Date: 17/11/20

library(readxl)
library(dplyr)
library(openxlsx)
covid_data <- read_excel("data-raw/owid-covid-data.xlsx")

reduced_data <- covid_data %>%
                    select(location, date, new_cases)%>%
                    filter(location == "Malaysia" | location == "Australia")

write.xlsx(reduced_data, "./data/covid_data.xlsx")

reduced_data <- tsibble::as_tsibble(reduced_data, key = "location", index = "date")

