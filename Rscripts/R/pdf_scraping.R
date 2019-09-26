library(rJava)      # Needed for tabulizer
library(tabulizer)  # Handy tool for PDF Scraping
library(tidyverse)  # Core data manipulation and visualization libraries


# PDF Scrape Tables ----
endangered_species_scrape <- extract_tables(
    file = "data/endangered_species.pdf",
    method = "decide",
    output = "data.frame"
)

# Pluck the first table in the list
endangered_species_raw_tbl <- endangered_species_scrape %>% 
    pluck(1) %>% 
    as_tibble()

# Show first 6 rows
endangered_species_raw_tbl %>% head() %>% knitr::kable()

# Get column names from Row 1
col_names <- endangered_species_raw_tbl %>% 
    slice(1) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(value = ifelse(is.na(value), "Missing", value)) %>% 
    pull(value)

# Overwrite names and remove Row 1
endangered_species_renamed_tbl <- endangered_species_raw_tbl %>% 
    set_names(col_names) %>% 
    slice(-1)

# Show the first 6 rows
endangered_species_renamed_tbl %>% head() %>% knitr::kable()


# Data Wrangling ----

endangered_species_final_tbl <- endangered_species_renamed_tbl %>% 
    
    # 1. Remove columns with NAs
    select_if(~ !all(is.na(.))) %>% 
    
    # 2. Fix columns that were combined
    separate(col = "Amphibians Fishes Insects",
             into = c("Amphibians", "Fishes", "Insects"),
             sep = " ") %>% 
    
    # 3. Convert to (Tidy) Long Format for visualization
    pivot_longer(cols = -Year, names_to = "species", values_to = "number") %>% 
    
    # 4. Fix numeric data stored as character
    mutate(number = str_remove_all(number, ",")) %>% 
    mutate(number = as.numeric(number)) %>% 
    
    # 5. Convert Character Year & species to Factor
    mutate(Year = as_factor(Year),
           species = as_factor(species)) %>% 
    
    # 6. Percents by year
    group_by(Year) %>% 
    mutate(percent = number / sum(number)) %>% 
    mutate(label = scales::percent(percent)) %>% 
    ungroup()

# Show first 6 rows
endangered_species_final_tbl %>% View()


# Visualization ----
endangered_species_final_tbl %>% 
    mutate(Year = fct_rev(Year)) %>% 
    
    ggplot(aes(Year, number, fill = species)) +
    
    # Geoms
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2) +
    coord_flip() +
    
    # Theme
    labs(
        title = "Critically Endangered Species",
        y = "Number of Species Added to critically Endangered List",
        x = "Year"
    ) +
    theme_minimal()


endangered_species_final_tbl %>% 
    mutate(Year = fct_rev(Year)) %>% 
    
    # Geom
    ggplot(aes(Year, number, color = species, group = species)) +
    geom_point() +
    geom_smooth(method = "loess") +
    facet_wrap(~ species, scales = "free_y", ncol = 3) +
    
    # Theme
    expand_limits(y = 0) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
        title = "Critically Endangered Species",
        subtitle = "Trends Not Improving",
        x = "",
        y = "Changes in Number of Species in Threatened Category"
    )
    

# Reference ----
# https://www.business-science.io/code-tools/2019/09/23/tabulizer-pdf-scraping.html?utm_source=Business+Science+-+Combined+List&utm_campaign=bd66043115-NEW_ARTICLE_BIG_DATA_DTPLYR_COPY_01&utm_medium=email&utm_term=0_a4e5b7c52f-bd66043115-307866347&mc_cid=bd66043115&mc_eid=fac0fc34d7


