datos <- country_summary('2020-01-01', today, 'Chile')$interval_covid_data

datos %>%
    rename('New Cases' = new_cases) %>% 
    e_charts(x = date) %>% 
    e_bar_('New Cases') %>% 
    e_tooltip(trigger = 'axis') %>% 
    e_axis_labels(x = 'Date',
                  y = 'New Cases') %>% 
    e_legend(show = FALSE) %>% 
    e_toolbox_feature("dataZoom") %>%
    e_toolbox_feature(feature = "reset") %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage") %>% 
    e_datazoom(
        type = "slider", 
        toolbox = FALSE,
        bottom = 12
    ) %>% 
    e_x_axis(nameLocation = "center", 
             splitArea = list(show = FALSE),
             axisLabel = list(margin = 0)) %>% 
    e_y_axis(nameLocation = "center", 
             position = 'right',
             splitArea = list(show = FALSE),
             axisLabel = list(margin = 0)) %>% 
    e_theme_custom('codes//walden.project.json')
    

