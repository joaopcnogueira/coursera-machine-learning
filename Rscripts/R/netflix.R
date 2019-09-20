# 01. Loading packages ----
library(tidyverse)


# 02. Loading data ----
netflix <- read_csv("data/NetflixViewingHistory.csv")


# 03. Data Wrangling ----
glimpse(netflix)

netflix <- netflix %>% 
    mutate(
        Date = lubridate::dmy(Date), 
        mes = lubridate::month(Date, label = TRUE, locale = "pt_BR.UTF-8"),
        ano = lubridate::year(Date),
        dia_semana = lubridate::wday(Date, label = TRUE, locale = "pt_BR.UTF-8"), 
        categoria = str_detect(Title, ": "),
        categoria = if_else(categoria, "Séries", "Filmes/Docs."),
        mes_ano = Date,
        programa = str_remove(Title, ":.*")
    )

lubridate::day(netflix$mes_ano) <- 1

# A coluna mes_ano é a coluna Date com o dia do mês sempre igual a 1.
# Isso facilita a agregação da base por mês

netflix


# 04. Results ----
netflix %>% 
    count(mes_ano) %>% 
    ggplot(aes(mes_ano, n)) +
    geom_line() +
    geom_smooth(se = FALSE, color = "blue") +
    geom_vline(
        aes(xintercept = as.numeric(mes_ano[mes_ano == "2019/01/01"])),
        linetype = 2,
        color = "blue"
    ) +
    labs(x = "Ano", y = "Frequência")

# Programas assistidos por mês
netflix %>% 
    count(mes, ano, categoria) %>% 
    group_by(mes, categoria) %>% 
    summarise(freq_media = mean(n)) %>% 
    mutate(freq_media = round(freq_media)) %>% 
    ggplot(aes(mes, freq_media, fill = categoria)) +
    geom_col() +
    labs(x = "Mês", y = "Títulos assistidos")

# Programas assistidos por ano
netflix %>% 
    count(ano, categoria) %>% 
    ggplot(aes(x = ano, y = n, fill = categoria)) +
    geom_col() +
    labs(x = "Ano", y = "Títulos assistidos", fill = "") +
    scale_x_continuous(breaks = min(netflix$ano):max(netflix$ano))


# Qual dia da semana mais assisto?
netflix %>% 
    count(dia_semana, categoria) %>% 
    ggplot(aes(x = dia_semana, y = n, fill = categoria)) +
    geom_col() +
    labs(x = "Dia da semana", y = "Titulos assistidos", fill = "")


# Top 10 séries que mais assisti
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


# 05. Reference ----
# https://www.curso-r.com/blog/2019-06-05-netflix/

