# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)

# 1.0 LOADING FILES ----
bikes_tbl <- read_excel(path = "data/bike_sales/bikes.xlsx")
bikes_tbl

bikeshops_tbl <- read_excel(path = "data/bike_sales/bikeshops.xlsx")
bikeshops_tbl

orderlines_tbl <- read_excel(path = "data/bike_sales/orderlines.xlsx")
orderlines_tbl


# 2.0 JOINING DATA ----
bike_orderlines_joined_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()


# 3.0 DATA WRANGLING ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
    
    # Separate description field into category 1, category 2, and frame.material
    separate(description,
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ",
             remove = TRUE) %>% 
    
    # Separate location field into city and state
    separate(location,
             into = c("city", "state"),
             sep = ", ", 
             remove = FALSE) %>% 
    
    # Price extended
    mutate(total.price = price * quantity) %>% 
    
    # Reorganize
    select(-c(...1, location)) %>% 
    select(-ends_with(".id")) %>% 
    
    bind_cols(bikes_orderlines_joined_tbl %>% select(order.id)) %>% 
    
    # Reorder columns
    select(contains("date"), contains("id"), contains("order"), quantity, price, total.price, everything()) %>% 
    
    # Renaming columns
    rename(order_date = order.date) %>% 
    set_names(names(.) %>% str_replace("\\.", "_"))

bike_orderlines_wrangled_tbl %>% glimpse()


# 4.0 BUSINESS INSIGHTS ----
# 4.1 Sales by Year ----
# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Selecting columns to focus on and adding an year column
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    
    # Grouping by year, and summarizing by sales
    group_by(year) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # $ Format text
    mutate(sales_text = scales::dollar(sales))

# Step 2 - Visualize
sales_by_year_tbl %>% 
    
    # Setup canvas with year (x-axis) and sales (y-axis)
    ggplot(aes(x = year, y = sales)) +
    
    # Geometries
    geom_col(fill = "#2c3e50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Revenue by Year",
         subtitle = "Upward trend",
         x = "",
         y = "Revenue")

# 4.2 Sales by Year and Category 2 ----
# Step 1 - Manipulate
sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # Selecting columns and add an year column
    select(order_date, total_price, category_2) %>% 
    mutate(year = year(order_date)) %>% 
    
    # Groupby and summarize by year and category 2
    group_by(year, category_2) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # $ Format text
    mutate(sales_text = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(sales))

# Step 2 - Visualize
BRL_format = scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")

sales_by_year_cat_2_tbl %>% 
    
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    
    geom_col() +
    geom_smooth(method = "lm", se = FALSE) +
    
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = BRL_format) +
    labs(title = "Revenue by Year and Category 2",
         subtitle = "Each product category has an upward trend",
         x = "",
         y = "Revenue",
         fill = "Product Secondary Category")

