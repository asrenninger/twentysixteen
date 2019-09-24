########################################################
## Section 1: Global Moran's I
## ## Create a weight matrix
## ## Run a Monte Carlo simulation
########################################################

library(tidyverse)
library(sf)

##

counties <- st_read("data-out/counties.geojson", crs = 102003)
left <- read_csv("data-out/left.csv")

##

autocorrelating <- 
  counties %>%
  left_join(left) %>%
  st_as_sf() %>%
  drop_na()

##

library(spdep)

##

coords <- 
  autocorrelating %>%
  st_centroid() %>%
  st_coordinates()

nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")

##

moranstest <- moran.test(autocorrelating$change_2012, weights)
montecarlo <- moran.mc(autocorrelating$change_2012, weights, nsim = 999)

moranstest

##

plot_montecarlo <- 
  ggplot(as.data.frame(montecarlo$res), aes(montecarlo$res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = 0.6554498240), colour = "grey70",size = 1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "observed and permuted Moran's I",
       subtitle = "I = 0.655 | P < 0.01",
       x = "results",
       y = "count") +
  theme_ver() +
  ggsave("morantest.png", height = 4, width = 6, dpi = 300)

########################################################
## Section 1: Local Moran's I
## ## Plot variables
## ## Find clusters
########################################################


moransi <- localmoran(autocorrelating$change_2012, weights) %>% as_tibble()

##

autocorrelating <- 
  autocorrelating %>%
  bind_cols(moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)

##

theme_map_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5),
          legend.position = 'bottom'
    )
  
}

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keywidth = unit(10, units = "mm"),
               keyheight = unit(2, units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

##

fun <- colorRampPalette(pal)

##

map_moran_difference <- 
  ggplot() +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(change_2012, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(fun(5)),
                    labels = str_sub(as.character(quantile(autocorrelating$change_2012,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)),
                                     1, 5),
                    name = "change",
                    guide = guide_discrete) +
  labs(title = "difference") +
  theme_map_legend() +
  ggsave("change.png", height = 6, width = 8, dpi = 300)

##

map_moran_i <- 
  ggplot() +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(fun(5)),
                    labels = str_sub(as.character(quantile(autocorrelating$locali,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 
                                     1, 5),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map_legend() +
  ggsave("localmoran.png", height = 6, width = 8, dpi = 300)

##

map_moran_i <- 
  ggplot() +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = fun(5),
                    labels = str_sub(as.character(quantile(autocorrelating$locali,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 
                                     1, 5),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map_legend() +
  ggsave("localmoran.png", height = 6, width = 8, dpi = 300)

##

map_moran_p <- 
  ggplot() +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(p_value, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = rev(fun(5)),
                    labels = str_sub(as.character(quantile(autocorrelating$p_value,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 
                                     1, 5),
                    name = "p value",
                    guide = guide_discrete) +
  labs(title = "statistical significance") +
  theme_map_legend() +
  ggsave("pvalue.png", height = 6, width = 8, dpi = 300)

##

autocorrelating <- 
  autocorrelating %>%
  mutate(scaled_difference = scale(change_2012)) %>%
  select(change_2012, scaled_difference, locali, expectation, variance, deviation, p_value) %>%
  mutate(lagged_difference = lag.listw(weights, scaled_difference),
         quad_sig = NA)

autocorrelating <-
  autocorrelating %>%
  mutate(quad_sig = 
           case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 1,
                     scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 2,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 3,
                     scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 4,
                     scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 5)) %>%
  st_as_sf()

##

boundaries <- 
  regression %>%
  mutate(rallied = if_else(trump_rallies_dma_post_convention > 0, 1, 0)) %>%
  filter(rallied == 1) %>%
  left_join(counties) %>%
  st_as_sf() %>%
  group_by(rallied) %>%
  summarise()

##

points <- read_csv("data-out/rallies.csv") %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE) %>%
  st_set_crs(4236) %>%
  st_transform(102003)

##

map_quads <- 
  ggplot() + 
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig)), size = 0, colour = NA) +
  geom_sf(data = boundary %>%
            st_set_crs(102003),
          aes(), alpha = 0.5) +
  geom_sf(data = boundaries %>%
            st_set_crs(102003),
          aes(), alpha = 0.5) +
  geom_sf(data = 
            points %>%
            filter(candidate == "clinton"), 
          colour = pal[1], size = 2, alpha = 0.5) +
  geom_sf(data = 
               points %>%
               filter(candidate == "trump"), 
          colour = pal[9], size = 2, alpha = 0.5) + 
  scale_fill_manual(values = fun(2),
                    name = "quadrants",
                    labels = c("low-low", "high-high"),
                    guide = guide_discrete,
                    na.translate = FALSE) +
  coord_sf(crs = 102003) +
  theme_map_legend() +
  ggsave("clusters.png", height = 6, width = 8, dpi = 300)

