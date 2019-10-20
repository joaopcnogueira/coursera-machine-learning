# 1.0 LOAD LIBRARIES ----
library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(tidyverse) # Data Manipulation
library(tidyquant) # ggplot2 theme
library(xopen)     # Opens URL in Browser
library(knitr)     # Pretty HTML Tables


# 2.0 WEB SCRAPING ----

# URL to View all Bikes
url <- "https://www.specialized.com/us/en/shop/bikes/c/bikes?q=%3Aprice-desc%3Aarchived%3Afalse&show=All"
xopen(url)

# 2.1 Read HTML from URL ----
html <- read_html(url)
html

json <- html %>% 
    
    # Filter HTML to isolate nodes
    html_nodes(".product-list__item-wrapper") %>% 
    
    # Extract the attribute data
    html_attr("data-product-ic")


json[1]


# 3.0 DATA WRANGLING ----

# 3.1 Function to convert JSON to Tibble
from_json_to_tibble <- function(json) {
    json %>% 
        fromJSON() %>% 
        as_tibble()
}

# running on the first element to test
json[1] %>% 
    from_json_to_tibble() %>% 
    knitr::kable()

# 3.2 Iterate over all JSON objects ----
bike_data_list <- json %>% 
    map(safely(from_json_to_tibble))


# 3.3 Inspect first converted element ----
bike_data_list[1]


# 3.4 Inspect for Errors ----
error_tbl <- bike_data_list %>% 
    # Grab just the $error elements
    map(~ pluck(., "error")) %>% 
    # Convert from list to tibble
    enframe(name = "row") %>% 
    # Return TRUE if element has error
    mutate(is_error = map_lgl(value, function(x) !is.null(x))) %>% 
    # Filter where error == TRUE
    filter(is_error)

error_tbl

error_tbl %>% pluck("value", 1)

json[218] %>% 
    str_replace('\\"BMX / Dirt Jump\\"', 'BMX / Dirt Jump') %>% 
    str_replace('22.5\\" TT', '22.5 TT') %>%
    from_json_to_tibble()


# Fix errors, re-run
bike_features_tbl <- json %>%
    str_replace('\\"BMX / Dirt Jump\\"', 'BMX / Dirt Jump') %>%
    str_replace('22.5\\" TT', '22.5 TT') %>%
    map_dfr(from_json_to_tibble)

# Show first 6 rows
bike_features_tbl %>%
    head() %>%
    kable()


# 4.0 EXPLORE BIKE MODELS ----
bike_features_tbl %>% 
    select(dimension3, price) %>% 
    mutate(dimension3 = as_factor(dimension3) %>% fct_reorder(price, .fun = median)) %>% 
    
    # Plot
    ggplot(aes(dimension3, price)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title = "Specialized Bike Models by Price")


bike_features_tbl %>% 
    select(name, price, dimension3) %>% 
    mutate(s_works = ifelse(str_detect(name, "S-Works"), "S-Works", "Not-S-Works")) %>% 
    mutate(dimension3 = as_factor(dimension3) %>% fct_reorder(price, .fun = median)) %>% 
    
    # Plot
    ggplot(aes(dimension3, price, color = s_works)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~ s_works, ncol = 1, scales = "free_y") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(title = "S-Works Effect on Price by Model")


# 5.0 REFERENCE ----
# https://www.business-science.io/code-tools/2019/10/07/rvest-web-scraping.html