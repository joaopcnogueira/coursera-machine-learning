library(tidyverse)

fifa_19_players_data <- read_csv("../notebooks/data/fifa19.csv") %>% 
    select(-c(X1, ID, Photo, Flag, 'Club Logo'))


# Creating a group_by object ----
team <- fifa_19_players_data %>% group_by(Club)

team

team %>% 
    drop_na(Club) %>% 
    count(sort = TRUE) %>% 
    head(5)

# Plotting inside aggregated data ----
arsenal <- team %>% filter(Club == 'Arsenal')

arsenal %>% 
    mutate(Wage = Wage %>% str_replace_all('[€K]', '') %>% as.integer()) %>%
    arrange(Wage %>% desc()) %>% 
    head(10) %>% 
    mutate(Name = Name %>% fct_reorder(Wage)) %>% 
    ggplot(aes(Name, Wage)) +
    geom_col(fill = "#61D199") +
    coord_flip() +
    labs(title = "Top 10 Earners at Arsenal Football Club 2019") %>% 
    scale_y_continuous(labels = scales::dollar_format())
    

# Creating a DataFrame with the Best Player for each team ----
team %>% 
    filter(Overall == max(Overall))
