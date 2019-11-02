library(tidyverse)

# 1.0 READING A BUNCH OF FILES ----
files <- list.files("data/bike_sales", pattern = ".xlsx", full.names = TRUE)
files

# 1.1 Read the files into a list using a for loop
all_files <- list()
for (i in seq_along(files)) {
    all_files[[i]] <- readxl::read_excel(files[[i]])
}
length(all_files)
all_files[[1]]
all_files[[2]]
all_files[[3]]

# 1.2 read the files into a list using the {purrr} package
library(purrr)
files

all_files <- map(files, readxl::read_excel)
length(all_files)
all_files[[1]]
all_files[[2]]
all_files[[3]]


# 2.0 PAIRWISE OPERATIONS OVER ELEMENTS FROM TWO LISTS ----

# 2.1 First example: multiplying elements ----
list1 <- list(1, 2, 3)
list2 <- list(1, 2, 3)

map2(list1, list2, ~ .x*.y)

# 2.2 Second example: create a dataframe ----
means <- list(1, 2, 3)
sites <- list("north", "west", "east")

list_of_datasets <- map2(sites, means, ~ data.frame(sites = .x, 
                                                    a = rnorm(mean = .y, n = 200, sd = 5/2)))
map(list_of_datasets, names)
map(list_of_datasets, dim)
map(list_of_datasets, ~ .x %>% count(sites))


# 3.0 PAIRWISE OPERATIONS OVER ELEMENTS FROM MORE THAN TWO LISTS ----
list1 <- list(1, 2, 3)
list2 <- list(1, 2, 3)
list3 <- list(1, 2, 3)

input_list <- list(v1 = list1, 
                   v2 = list2,
                   v3 = list3)

pmap(input_list, function(v1, v2, v3){v1*v2*v3} )


# 4.0 UNPACKING OPERATOR ----
library(zeallot)    # provides the unpacking operator %<-%

modelos <- c("lm", "svm")
feature_sets <- c("X1", "X2")
funcoes <- c("sqrt", "log1p")

# 4.1 Create a list of all possible combinations from three lists ----
cross3(modelos, feature_sets, funcoes)

# 4.2 Print the first combination
cross3(modelos, feature_sets, funcoes)[[1]]  

# 4.3 Unpacking values
c(modelo, feature_set, funcao) %<-% cross3(modelos, feature_sets, funcoes)[[1]]
modelo
feature_set
funcao

# 4.4 Iterating over the combined list ----
for (combination in cross3(modelos, feature_sets, funcoes)){
    c(modelo, feature_set, funcao) %<-% combination
    print(paste(modelo, feature_set, funcao, sep = ", "))
}

# 4.5 Unpacking values from more than 3 lists ----
ids <- c("id1", "id2")

input_list <- list(ids, modelos, feature_sets, funcoes)

# combination of all elements
cross(input_list)

for (combination in cross(input_list)){
    c(id, modelo, feature_set, funcao) %<-% combination
    print(paste(id, modelo, feature_set, funcao, sep = ", "))
}


# 5.0 SAFELY ITERATION OVER LISTS ----
# 5.1 First example ----
b <- list(-10, "unknown", 10)

# will produce an error due to the value "unknown"
b %>% map(function(x) x*10)

# we can iterate safely() over the list
b %>% map(safely(function(x) x*10, 
                 otherwise = NA_real_)) #%>% transpose()

# if we don't want to see the error message,
# we iterate possibly()
b %>% map(possibly(function(x) x*10,
                   otherwise = NA_real_))

# 5.2 Second example ----
a <- list(-10, 1, 10, 0)

a %>% map(possibly(function(x) log(x), 
                   otherwise = NA_real_))


# 6.0 WALK() FUNCTION ----
short_list <- list(-10, 1, 10)
short_list

# 6.2 nice printing formatting with walk() ----
walk(short_list, print)

# 6.1 printing several ggplot graphs with walk() ----
library(repurrrsive)
data("gap_split")

# map over the first 10 elements of gap_split
plots <- map2(gap_split[1:10], 
              names(gap_split[1:10]),
              ~ ggplot(.x, aes(year, lifeExp)) +
                  geom_line() +
                  labs(title = .y)
              )

walk(plots, print)

