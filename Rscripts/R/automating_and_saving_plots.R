# 01. LOADING PACKAGES ----
library(tidyverse)
library(purrr)


# 02. CREATING SOME DATA ----
set.seed(16)
df <- tibble(elev = round( runif(20, 100, 500), 1),
             resp = round( runif(20, 0, 10), 1),
             grad = round( runif(20, 0, 1), 2),
             slp = round( runif(20, 0, 35),1),
             lat = runif(20, 44.5, 45),
             long = runif(20, 122.5, 123.1),
             nt = rpois(20, lambda = 25) )


df %>% 
    head()

# getting the names of the response and the expl variables
response <- names(df)[1:3] # response variables
predictors <- names(df)[4:7]     # explanatory variables

response <- set_names(response)
predictors <- set_names(predictors)


# 03. SCATTER PLOT FUNCTION ----
scatter_plot <- function(data, x, y) {
    ggplot(data, aes_string(x, y)) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE, color = "grey74") +
        theme_bw()
}

scatter_plot(df, "lat", "elev")

# 03.1 Looping through one vector of variables ----
elev_plots <- map(predictors, ~scatter_plot(data = df, x = .x, y = "elev"))

elev_plots$slp
elev_plots$lat
elev_plots$long
elev_plots$nt


# 03.2 looping through both vectors ----
all_plots <- map(response,
                 ~map(predictors, scatter_plot, data = df, y = .x))

# plots for elev
all_plots$elev$slp
all_plots$elev$lat
all_plots$elev$long
all_plots$elev$nt

# plots for resp
all_plots$resp$slp
all_plots$resp$lat
all_plots$resp$long
all_plots$resp$nt

# plots for grad
all_plots$grad$slp
all_plots$grad$lat
all_plots$grad$long
all_plots$grad$nt


# 04. SAVING PLOTS ----

# 04.1 saving all plots to one PDF ----
# starting graphical device to save the plots with the pdf() function
pdf("all_scatterplots.pdf")

# saving all plot into the pdf "all_scatterplots.pdf"
all_plots

# turning off the graphical device
dev.off()


# 04.2 saving groups of plots together ----
iwalk(all_plots, 
      ~{
          pdf(paste(.y, "_scatterplots.pdf"))
          print(.x)
          dev.off()
      })


# 04.3 saving all plots separately ----
plotnames <- imap(all_plots, ~paste(.y,"_",names(.x),".png", sep="")) %>% 
    flatten()
plotnames

walk2(plotnames, 
      flatten(all_plots), 
      ~ggsave(filename = .x, plot = .y, height = 7, width = 7))


# 05. COMBINING PLOTS ----
cowplot::plot_grid(plotlist = all_plots[[1]])

response_plots <- map(all_plots, ~cowplot::plot_grid(plotlist = .x))

response_plots$elev

response_plots$resp

response_plots$grad


# 06. REFERENCE ----
# https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
# https://aosmith.rbind.io/2019/09/27/more-exploratory-plots/

