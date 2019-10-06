# 1.0 LOADING PACKAGES AND DATA ----
library(tidyverse)

netflix <- read_csv("data/NetflixViewingHistory.csv")

netflix


# 2.0 DATA WRANGLING ----
netflix <- netflix %>% 
    mutate(Date = lubridate::dmy(Date),
           mes = lubridate::month(Date, label = TRUE),
           ano = lubridate::year(Date),
           dia_semana = lubridate::wday(Date, label = TRUE),
           categoria = str_detect(Title, ": "),
           categoria = if_else(categoria, "Séries", "Filmes/Docs."),
           mes_ano = Date,
           programa = str_remove(Title, ":.*"))

lubridate::day(netflix$mes_ano) <- 1

netflix %>% glimpse()


# 3.0 VISUALIZATIONS ----

netflix %>% 
    count(mes_ano) %>% 
    ggplot(aes(mes_ano, n)) +
    geom_line() +
    geom_smooth(se = FALSE, color = "blue") +
    geom_vline(aes(xintercept = as.numeric(mes_ano[mes_ano == "2019/01/01"])),
               linetype = 2,
               color = "blue") +
    labs(title = "Títulos assistidos por mês ao longo da história", 
         x = "Ano", 
         y = "Frequência")


# 3.1 Programas assistidos por mês ----
netflix %>% 
    count(mes, ano, categoria) %>% 
    group_by(mes, categoria) %>% 
    summarise(freq_media = mean(n)) %>% 
    mutate(freq_media = round(freq_media)) %>% 
    ungroup() %>% 
    ggplot(aes(mes, freq_media, fill = categoria)) +
    geom_col() +
    labs(title = "Programas assistidos por mês", 
         x = "Mês", 
         y = "Títulos assistidos")

# 3.2 Programas assistidos por ano ----
netflix %>% 
    count(ano, categoria) %>% 
    ggplot(aes(x = ano, y = n, fill = categoria)) +
    geom_col() +
    scale_x_continuous(breaks = min(netflix$ano):max(netflix$ano)) +
    labs(title = "Programas assistidos por ano", 
         x = "Ano", 
         y = "Títulos assistidos", fill = "") 

# 3.3 Programas assistidos por dia da semana ----
netflix %>% 
    count(dia_semana, categoria) %>% 
    ggplot(aes(x = dia_semana, y = n, fill = categoria)) +
    geom_col() +
    labs(x = "Dia da semana", y = "Titulos assistidos", fill = "")


# 3.4 Top 10 séries mais assistidas ----
netflix %>% 
    filter(categoria == "Séries") %>% 
    count(programa) %>% 
    top_n(10, n) %>% 
    mutate(programa = fct_reorder(programa, n)) %>% 
    ggplot(aes(x = programa, y = n)) +
    geom_col(fill = "#e50914", color = "black") +
    geom_text(aes(label = n, y = n / 2)) +
    labs(title = "Top 10 títulos mais assistidos", 
         x = "Série", 
         y = "Visualizações") +
    coord_flip()
