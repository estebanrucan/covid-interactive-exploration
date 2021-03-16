require(tidyverse)
require(shiny)
require(flexdashboard)
require(magrittr)
require(lubridate)
require(purrr)
require(janitor)
require(leaflet)
require(ggrepel)
require(slider)
require(plotly)
require(scales)
require(rvest)
require(httr)



# load data ---------------------------------------------------------------

page <- GET('https://github.com/owid/covid-19-data/commits/master/public/data/owid-covid-data.csv',
            user_agent('Esteban :)'))



dates <- page %>% 
    content() %>% 
    html_nodes('relative-time.no-wrap') %>% 
    html_attr('datetime') %>%
    as_datetime()

last_commit <- dates[1]

last_update <- read_csv(file.path('datasets', 'date.csv')) %>% pull(date)

current_time <- Sys.time() %>% with_tz('UTC') 

time_since_last_commit <- interval(last_commit, current_time) %/% hours(1)

if (last_update < last_commit) {
    covid_data <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
    write.csv(covid_data, file = file.path('datasets', 'owid-covid-data.csv'))
    write.csv(last_commit %>% tibble(date = .), file = file.path('datasets', 'date.csv'))
} else {
    covid_data <- read_csv(file.path('datasets', 'owid-covid-data.csv'))
}

coordinates <- read_csv(file.path('datasets', 'spatial.csv'))

coordinates %<>% 
    clean_names() %>% 
    select(iso_code = alpha_3_code,
           longitude = longitude_average,
           latitude = latitude_average)

covid_data %<>%
    filter(!(location %in% c('World', unique(continent), 'European Union')))

covid_data %<>% 
    replace_na(list(new_tests = 0, new_cases = 0, positive_rate = 0,
                    new_deaths = 0, total_deaths = 0,
                    total_cases = 0, total_tests = 0)) %>% 
    group_by(location) %>% 
    summarise(total_active = slide_dbl(new_cases, sum, .before = 14, .after = 0, .complete = FALSE)) %>% 
    replace_na(list(total_active = 0, total_cases = 0, total_deaths = 0)) %>% 
    ungroup() %>% 
    select(total_active) %>% 
    bind_cols(covid_data) %>% 
    mutate(new_tests = abs(new_tests), new_cases = abs(new_cases), positive_rate = abs(positive_rate),
           new_deaths = abs(new_deaths), total_deaths = abs(total_deaths),
           total_cases = abs(total_cases), total_tests = abs(total_tests)) %>% 
    select(everything(), total_active) %>% 
    mutate(total_recovered = total_cases - total_active - total_deaths)

# others ------------------------------------------------------------------


continents <- covid_data %>% 
    pull(continent) %>% 
    unique() %>% 
    sort()

countries_per_continent <- covid_data %>% 
    arrange(continent) %>% 
    group_by(continent) %>% 
    nest() %>% 
    mutate(countries = map(data, ~.x %>% 
                               pull(location) %>%
                               unique() %>% 
                               sort())) %>% 
    pull(countries) %>% 
    set_names(continents) 


    


# each country cases ------------------------------------------------------



country_cases <- function(start, end, top) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases = 0)) %>%
        left_join(coordinates, by = 'iso_code') %>% 
        group_by(continent, location) %>% 
        summarise(cases = sum(new_cases),
                  mean_cases = mean(new_cases)) %>% 
        arrange(desc(cases)) %>% 
        head(top) %>% 
        rename('Total Cases' = cases,
               'Country' = location,
               Continent = continent,
               'Mean Cases' = mean_cases)
        
}


country_cases_pm <- function(start, end, top) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases = 0)) %>%
        left_join(coordinates, by = 'iso_code') %>% 
        group_by(continent, location) %>% 
        summarise(cases = sum(new_cases),
                  cases_per_million = round(cases / max(population) * 1e+6)) %>% 
        arrange(desc(cases_per_million)) %>% 
        head(top) %>% 
        rename('Total Cases' = cases,
               'Country' = location,
               Continent = continent,
               'Total Cases per Million' = cases_per_million)
    
}


global_total_cases <- function(start, end, top) {
    tab1 <- covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        filter(!is.na(new_cases)) %>% 
        group_by(location) %>% 
        summarise(cases = sum(new_cases)) %>% 
        arrange(desc(cases)) %>% 
        head(top) %>% 
        pull(location)
    
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        filter(!is.na(new_cases)) %>% 
        filter(location %in% tab1) %>% 
        group_by(date) %>% 
        summarise(cases = sum(new_cases)) %>% 
        ungroup() %>% 
        mutate(cum_cases = cumsum(cases)) %>% 
        arrange(date) %>% 
        rename('Date' = date,
               'New Cases' = cases,
               'Accumulated Cases' = cum_cases)
}



# map ---------------------------------------------------------------------


global_summary <- function(start, end) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases = 0, new_deaths = 0, new_tests = 0)) %>%
        group_by(location) %>% 
        summarise(total_cases = sum(new_cases),
                  total_deaths = sum(new_deaths),
                  positivity = round(sum(new_cases) / sum(new_tests) * 100)) %>% 
        left_join(covid_data %>% 
                      select(location, iso_code) %>% 
                      distinct(), by = 'location')  %>% 
        select(iso_code, location, total_cases, total_deaths, positivity) %>% 
        left_join(coordinates, by = "iso_code")
}


country_summary <- function(start, end, country) {

    interval_covid_data <- covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        filter(location == country) %>% 
        replace_na(list(new_cases = 0, new_deaths = 0, new_tests = 0))
    
    new_cases <- interval_covid_data %>% 
        filter(date == as.Date(end)) %>% 
        pull(new_cases)
    
    active_cases <- interval_covid_data %>% 
        filter(date == as.Date(end)) %>% 
        pull(total_active)
    
    total_recovered <- interval_covid_data %>% 
        filter(date == as.Date(end)) %>% 
        pull(total_recovered) 
    
    total_cases <- interval_covid_data %>% 
        filter(date == as.Date(end)) %>% 
        pull(total_cases) 
    
    last_day_deaths <- interval_covid_data %>% 
        replace_na(list(new_deaths = 0)) %>% 
        filter(date == as.Date(end)) %>% 
        pull(new_deaths) 
    
    total_deaths <- interval_covid_data %>% 
        replace_na(list(new_deaths = 0)) %>% 
        pull(new_deaths) %>% 
        sum()
    
    
    total_icu <- interval_covid_data %>% 
        replace_na(list(icu_patients = 0)) %>% 
        pull(icu_patients) %>% 
        sum()
        
    total_positivity <- interval_covid_data %>% 
        replace_na(list(new_cases = 0, new_test = 0)) %>% 
        mutate(total_pos = round(sum(new_cases) / sum(new_tests) * 100, 1)) %>% 
        pull(total_pos) %>% 
        max()
    
    positivity_last_day <- interval_covid_data %>% 
        replace_na(list(new_tests = 0, new_cases = 0)) %>% 
        filter(date == as.Date(end)) %>% 
        mutate(positive_rate = round(new_cases / new_tests * 100, 1)) %>% 
        pull(positive_rate)
    
    total_test <- interval_covid_data %>% 
        replace_na(list(new_test = 0)) %>% 
        pull(new_tests) %>% 
        sum()
    
    test_last_day <- interval_covid_data %>% 
        filter(date == as.Date(end)) %>%
        replace_na(list(new_test = 0)) %>% 
        pull(new_tests)
    
    cases_pm <- total_test <- interval_covid_data %>% 
        replace_na(list(new_cases = 0)) %>% 
        summarise(ncases_pm = round(sum(new_cases) / max(population) * 1e+6)) %>% 
        pull(ncases_pm)
        
        
    list(interval_covid_data, new_cases,active_cases, total_recovered, 
         total_cases, last_day_deaths,
         total_deaths, total_icu, total_positivity, positivity_last_day, 
         total_test, test_last_day, cases_pm) %>% 
        set_names(c(
            'interval_covid_data', 'new_cases', 'active_cases', 'total_recovered', 
            'total_cases', 'last_day_deaths', 'total_deaths',
            'total_icu', 'total_positivity', 'positivity_last_day',
            'total_test', 'test_last_day', 'cases_pm'
        ))
    
}




