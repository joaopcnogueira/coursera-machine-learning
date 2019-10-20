library(tidyverse)

df <- readxl::read_excel("data/PLANILHA PARA AGENDAMENTO ALIMENTOS E BEBIDAS - FINAL.xlsx", 
                         skip = 1)

df_wrangled <- df %>% 
    select(PRODUTOS, COMPRADORES, PRIORIDADE) %>% 
    separate_rows(c(COMPRADORES, PRIORIDADE), sep = ",") %>% 
    arrange(PRODUTOS) %>% 
    pivot_wider(names_from = COMPRADORES, values_from = PRIORIDADE) %>% 
    set_names(names(.) %>% str_trim()) %>% 
    select(sort(names(.))) %>%
    select(PRODUTOS, everything()) %>% 
    set_names(names(.) %>% str_to_upper())
