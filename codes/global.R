suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(flexdashboard))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(lubridate))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(janitor))
suppressPackageStartupMessages(require(leaflet))
suppressPackageStartupMessages(require(ggrepel))
suppressPackageStartupMessages(require(slider))
suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(scales))
suppressPackageStartupMessages(require(rvest))
suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(leaflet.extras))


# Checking commits -------------------------------------------------------------

commits_page <- GET('https://github.com/owid/covid-19-data/commits/master/public/data/owid-covid-data.csv',
            user_agent('Esteban :)'))

dates <- commits_page %>% 
    content() %>% 
    html_nodes('relative-time.no-wrap') %>% 
    html_attr('datetime') %>%
    as_datetime()

last_commit  <- dates[1]
last_update  <- read_rds(file.path('datasets', 'date.RDS')) %>% pull(date)
current_time <- Sys.time() %>% with_tz('UTC') 

time_since_last_commit <- interval(last_commit, current_time) %/% hours(1)

if (last_update < last_commit) {
    
    covid_data <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
    
    saveRDS(covid_data, file = file.path('datasets', 'owid-covid-data.RDS'))
    saveRDS(last_commit %>% tibble(date = .), file = file.path('datasets', 'date.RDS'))
    
} else {
    
    covid_data <- read_rds(file.path('datasets', 'owid-covid-data.RDS'))
}

coordinates <- read_rds(file.path('datasets', 'coordinates.RDS'))

# Changes ----------------------------------------------------------------------

covid_data %<>%
    filter(!(location %in% c('World', unique(continent), 'European Union')))

covid_data %<>% 
    replace_na(list(new_tests     = 0,
                    new_cases     = 0, 
                    positive_rate = 0,
                    new_deaths    = 0, 
                    total_deaths  = 0,
                    total_cases   = 0, 
                    total_tests   = 0)) %>% 
    group_by(location) %>% 
    summarise(total_active = slide_dbl(new_cases, sum, .before = 11, .after = 0, .complete = FALSE),
              .groups = 'drop') %>% 
    replace_na(list(total_active = 0, 
                    total_cases  = 0, 
                    total_deaths = 0)) %>% 
    ungroup() %>% 
    select(total_active) %>% 
    bind_cols(covid_data) %>% 
    mutate(new_tests     = abs(new_tests), 
           new_cases     = abs(new_cases), 
           positive_rate = abs(positive_rate),
           new_deaths    = abs(new_deaths),
           total_deaths  = abs(total_deaths),
           total_cases   = abs(total_cases), 
           total_tests   = abs(total_tests)) %>% 
    select(everything(), total_active) %>% 
    mutate(total_recovered = total_cases - total_active - total_deaths)

covid_data %<>% 
    left_join(coordinates, by = 'iso_code')

continents <- covid_data %>% 
    pull(continent) %>% 
    unique() %>% 
    sort()
