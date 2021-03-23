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

country_cases <- function(start, end) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases = 0)) %>%
        group_by(continent, location) %>% 
        summarise(cases = sum(new_cases)) %>% 
        arrange(desc(cases)) %>% 
        rename('Total Cases' = cases,
               'Country' = location,
               Continent = continent)
    
}


country_cases_pm <- function(start, end) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases = 0)) %>%
        group_by(continent, location) %>% 
        summarise(cases = sum(new_cases),
                  cases_per_million = round(cases / max(population) * 1e+6)) %>% 
        arrange(desc(cases_per_million)) %>% 
        rename('Total Cases' = cases,
               'Country' = location,
               Continent = continent,
               'Total Cases per Million' = cases_per_million)
    
}


global_total_cases <- function(start, end) {
    covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        filter(!is.na(new_cases)) %>% 
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


global_summary <- function(start, end, maps) {
    gs <- covid_data %>%
        filter(between(date, as.Date(start), as.Date(end))) %>% 
        replace_na(list(new_cases  = 0,
                        new_deaths = 0, 
                        new_tests  = 0)) %>%
        group_by(location) %>% 
        summarise(total_cases  = sum(new_cases),
                  total_deaths = sum(new_deaths),
                  positivity   = round(sum(new_cases) / sum(new_tests) * 100, 2),
                  cases_pm     = round(total_cases / max(population) * 1e+6),
                  deaths_pm    = round(total_deaths / max(population) * 1e+6),
                  .groups = 'drop') %>% 
        ungroup() %>% 
        left_join(covid_data %>% select(iso_code, location, population) %>% distinct(), 
                  by = 'location')
    
    maps@data  %<>% 
        left_join(gs, by = c('ISO_A3' = 'iso_code')) %>% 
        select(-ADMIN) %>% 
        rename(iso_code = ISO_A3) %>% 
        replace_na(list(total_cases  = 0,
                        total_deaths = 0,
                        positivity   = 0,
                        cases_pm     = 0,
                        deaths_pm    = 0)) %>% 
        mutate(total_cases = if_else(total_cases == 0, 1, total_cases))
    
    maps
}




# Country ----------------------------------------------------------------------



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

country_coord <- function(country) {
    covid_data %>% 
        filter(location == country) %>% 
        select(iso_code, location) %>% 
        slice(1) %>% 
        left_join(coordinates, by = 'iso_code') %>% 
        select(longitude, latitude)
}

integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}

bound_limits <- function(name) {
    
    covid_data %>%
        select(location, min_lon, min_lat, max_lon, max_lat) %>%
        bind_rows(
            tibble(
                location = continents,
                min_lon = c(-23.906250, 18.808594, -13.535156, -169.453125, 101.865234, -100.019531),
                min_lat = c(-36.456636, 1.406109, 34.452218, 15.961329, -51.563412, -57.421294),
                max_lon = c(53.964844, 190.898438, 42.890625, -52.734375, 180, -26.015625),
                max_lat = c(38.134557, 76.800739, 70.080562, 73.627789, -4.565474, 13.239945)
            )
        ) %>%
        bind_rows(
            tibble(
                location = 'Worldwide',
                min_lon  = -175,
                max_lon  = 175,
                min_lat  = -90,
                max_lat  = 90
            )
        ) %>% 
        filter(location == name) %>% 
        distinct() %>% 
        mutate(latitude = mean(c(min_lat, max_lat)),
               longitude = mean(c(min_lon, max_lon)))
        
}

evolution_of_cases <- function(start, end) {
    
    start <- as.Date(start)
    end   <- as.Date(end)
    
    dates <- start + 0:(interval(start, end) %/% days(1))
    
    dates_location <- tibble(
        date = rep(dates, rep(length(continents), length(dates))),
        continent = rep(continents, length(dates))) %>% 
        filter(between(date, start, end)) %>% 
        mutate(week = interval(start, date) %/% weeks(1) + 1) %>%
        mutate(paste_loc = paste0(week, continent)) %>%
        group_by(week, continent, paste_loc) %>% 
        summarise(date = max(date),
                  .groups = 'drop')
    
    covid_data %>% 
        filter(between(date, start, end)) %>% 
        select(date, continent, location, new_cases, new_deaths) %>%
        arrange(date, continent) %>% 
        mutate(week = interval(start, date) %/% weeks(1)) %>%
        group_by(week, continent) %>% 
        summarise(total_cases  = sum(new_cases),
                  total_deaths = sum(new_deaths),
                  .groups = 'drop') %>%
        ungroup() %>% 
        group_by(continent) %>% 
        arrange(total_cases, total_deaths, .by_group = TRUE) %>% 
        ungroup() %>% 
        mutate(paste_loc = paste0(week, continent)) %>%
        right_join(dates_location, by = 'paste_loc') %>% 
        select(week = week.y, 
               continent = continent.y, 
               total_cases,
               total_deaths,
               date) %>% 
        arrange(week, continent) %>%
        replace_na(list(total_cases = 1, 
                        total_deaths = 1)) %>% 
        group_by(week) %>% 
        mutate(ranking = row_number(desc(total_cases))) %>%
        ungroup() %>%
        mutate(group = factor(ranking,
                              labels = c('1th', '2nd', '3rd', '4th', '5th', '6th'))) %>% 
        arrange(week, continent)
}

load(file.path('datasets', 'countries.RData'))