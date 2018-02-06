library(dplyr)
library(stringr)
library(readr)
library(rvest)
library(viridis)
library(ggmap)
library(tidyr)
library(forcats)
library(ggpubr)
library(hrbrthemes)
##########
# GET DATA
##########

html.ports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_container_ports")
df.ports <- html.ports %>%
  html_node("table") %>%
  html_table() %>%
  tbl_df()

# inspect
df.ports

################
# RENAME COLUMNS
################

# set colnames to lowercase
names(df.ports)[2:5] <- names(df.ports)[2:5] %>%
  tolower()
# inspect
df.ports

# rename year columns
names(df.ports)[6:18] <- seq(2016, 2004)
# inspect
df.ports

#########
# RESHAPE
#########

# Gather all years and create volume column
df.ports <- df.ports %>%
  gather(year, volume, 6:18)
# inspect
df.ports

# remove brackets from volume
df.ports <- df.ports %>%
  mutate(volume = str_replace_all(volume, "\\(.*\\)","") %>%
           parse_number())
# inspect
df.ports  

# drop rank column and rank each year
df.ports <- df.ports[, -1]
# inspect
df.ports 
# re-rank
df.ports <- df.ports %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(volume))) %>%
  ungroup() %>%
  select(rank, everything())
# inspect
df.ports

#########
# GEOCODE
#########
# get ports as character vector
ports <- df.ports %>%
  distinct(port) %>% 
  pull()
geocodes.ports <- geocode(ports)
# inspect
geocodes.ports

# return list of ports with NA coordinates
geocodes.ports %>%
  filter(is.na(lon))

# manually fill in the 3 NAs using https://www.latlong.net/convert-address-to-lat-long.html
geocodes.ports <- geocodes.ports %>%
  mutate(lon = case_when(.$port == "Ningbo-Zhoushan" ~ 121.987836,
                         TRUE ~ .$lon),
         lat = case_when(.$port == "Ningbo-Zhoushan" ~ 29.901952,
                         TRUE ~ .$lat)
         
  )

# merge geocodes to port lits
df.ports <- cbind(df.ports, geocodes.ports) %>%
  tbl_df()
# inspect
df.ports

# return list of ports with NA coordinates
df.ports %>%
  filter(is.na(lon))

# check
df.ports %>%
  filter(port == "Ningbo-Zhoushan") %>%
  select(lat, lon)

###########
# RE-FACTOR
###########

# region & country as factors
df.ports <- df.ports %>%
  mutate(region = as.factor(region),
         country = as.factor(jurisdiction)) %>%
  select(rank, port, country, region, year, volume, lon, lat)
# inspect
df.ports
# get region levels 
levels(df.ports$region)
nlevels(df.ports$region)

# create continent variable
df.ports <- df.ports %>%
  mutate(continent = fct_collapse(region,
                                  `South America` = "South America",
                                  `North America` = c("Central America", "North America"),
                                  Asia = c("East Asia", "South Asia", "Southeast Asia", "Western Asia"),
                                  Europe = c("Northern Europe", "Southern Europe"),
                                  Africa = c("North Africa"))) %>%
  select(rank, port, country, region, continent, year, volume, lon, lat)
# inspect
df.ports
levels(df.ports$continent)

#############
# MAKE GRAPHS
#############

# ---------------------
# Graph top 25 ports
# ---------------------
df.ports %>%
  filter(year == 2016, rank <= 25) %>%
  ggplot(aes(x = reorder(port, volume), y = volume)) + 
  geom_bar(stat = "identity", fill = "dark red") + 
  geom_text(aes(label = volume), hjust = 1.1, color = "#FFFFFF") +
  scale_y_continuous(labels = scales::comma_format()) + 
  coord_flip() +
  labs(title = "China, Singapore and South Korea had the\nhighest-volume ports of 2016") +
  labs(x = "Port", y = "Shipping Volume\n(1000 TEUs") +
  theme(axis.title = element_text(size = 20)) +
  theme_ipsum_rc(grid="X") 
  
ggsave("shipping-flows-top25-ports.png", width = 7, height = 10)

# ------------------------
# Highlight ports in China
# ------------------------

# How many of the ports were in China?
df.ports %>%
  count(country == "China") # 19 / 51 ~ 40%

# plot
df.ports %>%
  filter(year == 2016) %>%
  mutate(asia_flag = ifelse(country == "China", "China", "Not China")) %>%
  ggplot(aes(x = reorder(port, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", aes(fill = asia_flag)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_fill_manual(values = c("dark red", "#999999")) + 
  labs(title = "In 2016, 40% of the world's busiest ports were in China") +
  labs(x = "Port", y = "Shipping Volume\n(1000 TEUs") + 
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) + 
  theme(legend.title = element_blank()) 
ggsave("shipping-flows-most-ports-in-china.png", width = 10, height = 7, units = "in")

# -----------------------
# Highlight ports in Asia
# -----------------------
# How many ports were in Asia?
df.ports %>%
  count(continent == "Asia") # 34 / 51 = 2/3

# plot
df.ports %>%
  filter(year == 2016) %>%
  mutate(asia_flag = ifelse(continent == "Asia", "Asia", "Not Asia")) %>%
  ggplot(aes(x = reorder(port, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", aes(fill = asia_flag)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  scale_fill_manual(values = c("dark red", "#999999")) + 
  labs(title = "In 2016, exactly 2/3 the world's busiest ports were in Asia") +
  labs(x = "Port", y = "Shipping Volume\n(1000 TEUs") + 
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4)) + 
  theme(legend.title = element_blank()) 
ggsave("shipping-flows-biggest-ports-in-east-asia.png", width = 10, height = 7, units = "in")

# --------------------------------------------
# Graph Growth Rates, Highlighting Singapore's
# --------------------------------------------
df.ports %>%
  mutate(port_highlight = ifelse(port == "Shanghai", "Shanghai","Other High-Volume Port")) %>%
  ggplot(aes(x = year, y = volume, group = port)) +
  geom_line(aes(color = port_highlight, alpha = port_highlight), size = 1.5, na.rm = T) +
  scale_color_manual(values = c("#999999","dark red")) + 
  scale_alpha_manual(values = c(.3,1)) +
  labs(title = "Shanghai's shipping volume has increased the most\nfrom 2004 to 2016") +
  labs(x = "Year", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme_ipsum_rc() +
  theme(legend.title = element_blank()) 
ggsave("shipping-flows-highest-growth-in-singapore.png")


# ---------------------------
# Compare Port Growth by Port
# ---------------------------

# small multiples: growth of ports over time
df.ports %>%
  ggplot(aes(x = year, y = volume, group = port, color = region)) +
  geom_line(size = 1, na.rm = T) +
  scale_x_discrete(breaks = unique(df.ports$year)[seq(1,13,3)]) + 
  facet_wrap(~ port) +
  labs(title = "Strongest growth in East Asia and Singapore") +
  labs(subtitle = "2004 to 2016") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme_minimal() + 
  theme(axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) 
ggsave("shipping-flows-comparing-port-growth.png", width = 10, height = 8.5)

# -----------------------------
# Compare Port Growth by Region
# -----------------------------
df.ports %>%
  group_by(year, region) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_volume, group = region, color = region)) +
  geom_line(size = 1, na.rm = T) +
  scale_x_discrete(breaks = unique(df.ports$year)[seq(1,13,3)]) + 
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Growth in shipping volume, by Region") +
  labs(subtitle = "2004 to 2016") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme_ipsum_rc() +
  theme(axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) 

ggsave("shipping-flows-port-growth-by-region.png")


# ---------------------------------
# Port Growth by Continent
# ---------------------------------
df.ports %>%
  group_by(year, continent) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_volume, group = continent, color = continent)) +
  geom_line(size = 1, na.rm = T) +
  scale_x_discrete(breaks = unique(df.ports$year)[seq(1,13,3)]) + 
  scale_color_ipsum() +
  scale_fill_ipsum() +
  facet_wrap(~ continent, scales = "free_y") +
  labs(title = "Growth in shipping volume, by Continent") +
  labs(subtitle = "2004 to 2016") +
  labs(x = "Port", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme_ipsum_rc() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank()) 
ggsave("shipping-flows-port-growth-by-continent.png")

#============================
# BUMP CHART: CHINA
# here, we'll highlight China
#============================
df.ports %>%
  distinct(port)

param.rank_n = 15
df.ports
df.ports %>%
  filter(rank <= param.rank_n) %>%
  mutate(china_flag = ifelse(country == "China", T,F)) %>%
  mutate(china_labels = ifelse(china_flag == T, port,"other")) %>%
  ggplot(aes(x = year, y = rank, group = port)) +
  geom_line(aes(color = china_labels, alpha = china_flag), size = 2) +
  geom_point(aes(color = china_labels, alpha = china_flag), size = 2.3) +
  geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
  geom_text(data = df.ports %>% filter(year == "2014", rank <= param.rank_n), aes(label = port, x = '2014') , hjust = -.05, color = "#888888", size = 4) +
  geom_text(data = df.ports %>% filter(year == "2004", rank <= param.rank_n), aes(label = port, x = '2004') , hjust = 1.05, color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.3, .3)) +
  scale_y_reverse(breaks = c(1,5,10,15)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  labs(title = "Top Chinese ports increased rank\nsubstantially from 2004 to 2016") +
  labs(subtitle = "(Port ranks, by volume)") +
  labs(x = "Year", y = "Rank") +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +  
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#4e79a5","#f18f3b", "#ffffff", "#999999","#e0585b","#5aa155","#edc958","#77b7b2","#BBBBBB"))
ggsave("shipping-flows-bump-chart-port-growth.png")

##########
# PLOT MAP
##########

# install libraries
library(swatches)
library(nord)
library(hrbrthemes)
library(tidyverse)
library(ggalt)

# view Nord palettes to design color palette 
par(mfrow=c(8, 2), lheight = 2, mar=rep(1, 4), adj = 0)
walk(names(nord_palettes), nord_show_palette)

# get map data
map.world <- map_data("world")
map.world <- map.world[map.world$region != "Antarctica", ]
head(map.world)

# plot 
df.ports %>% 
  filter(year == '2016') %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#2E3440",color = "#3B4252", size = .125) +
  geom_point(aes(size = volume), color = "#EBCB8B", alpha = .67) +
  scale_size_continuous(range = c(.2,10), breaks = c(5000, 15000, 30000), name = "Shipping Volume\n(1000 TEUs)",
                        labels = scales::comma) + 
  coord_proj("+proj=wintri") +
  labs(x = NULL, y = NULL,
       title = "High-volume ports were highly clustered in\nChina and Asia in 2016",
       caption = "Source:") +
  theme_ipsum_rc(plot_title_size = 24, subtitle_size = 12) + 
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "#4C566A"),
        plot.title = element_text(color = "#8FBCBB", hjust = 0.5),
        legend.direction = "horizontal",
        legend.title = element_text(color = "#FFFFFF"),
        legend.text = element_text(color = "#FFFFFF"),
        legend.position = c(0.5, 0.05))
ggsave("shipping-flows-map.png")

