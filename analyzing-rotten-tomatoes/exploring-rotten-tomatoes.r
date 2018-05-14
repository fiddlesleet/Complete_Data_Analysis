# credit to https://www.r-bloggers.com/ranking-brad-pitts-movies-in-r/ for the idea

library(tidyverse)
library(rvest)
library(lubridate)
library(dplyr)

# find her personal page on Rotten Tomatoes
url <- "https://www.rottentomatoes.com/celebrity/meryl_streep"
page <- read_html(url)

# read in list of movies
filmography_selector <- html_nodes(page, css = "#filmographyTbl")
filmography_table <- html_table(filmography_selector)
movies <- filmography_table[[1]]
str(movies)

'%ni%' <- Negate('%in%')
movies <- movies %>%
  mutate(CREDIT = gsub("\\r\\n", "", CREDIT)) %>%
  filter(RATING != "No Score Yet",
         CREDIT %ni% c("Producer", "Executive Producer"),
         TITLE %ni% c("Selma", "The Time Traveler's Wife")) %>% # had no acting role
  mutate(RATING = gsub("%", "", RATING),
         RATING = as.numeric(RATING),
         RATING = RATING/100)

movies$RATING <- as.numeric(movies$RATING)

# get stats
mean(movies$RATING, na.rm = TRUE)
median(movies$RATING, na.rm = TRUE)
# worst film
movies %>% arrange(RATING) %>% head(n = 1)
# best film 
movies %>% 
  arrange(desc(RATING)) %>% 
  select(RATING, TITLE) %>% 
  head(n = 1)


## graph the results
library(grid)
library(png)


# download rotten tomatoes logo here:
#   http://static.tvtropes.org/pmwiki/pub/images/rotten_tomatoes_8290.jpg

# convert to png and strip out most of the white using ImageMagick:
#  convert rotten_tomatoes_8290.jpg -transparent white rotten_tomatoes_logo.png
tomato <- readPNG("/Users/hannahsmythe/Downloads/rotten_tomatoes_logo.png")
g <- rasterGrob(tomato, interpolate=TRUE)

# graph
ggplot(movies, aes(x = RATING)) +
  geom_histogram(bins = 20, fill = "#EE4000", colour = "yellow") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#3A9425")) +
  annotation_custom(g, xmin=0, xmax=0.25, ymin=4, ymax=6)

# ratings over time
movies <- movies %>%
  arrange(YEAR) %>%
  mutate(difference = c(0, diff(RATING, lag = 1)),
         date_counter = 1:nrow(.))

ggplot(movies, aes(x = date_counter, y = difference)) +
  geom_line(colour = "yellow") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "#3A9425"),
        axis.line = element_line(colour = "#EE4000")) +
  annotate("label", label = "1989", x = 2, y = -0.7, colour = "#EE4000") +
  annotate("label", label = "2016", x = 47, y = -0.7, colour = "#EE4000") +
  geom_hline(yintercept = 0, linetype = 2, colour = "#EE4000") +
  annotation_custom(g, xmin=0, xmax=7, ymin=0.5, ymax=0.95)

ggplot(movies, aes(x = YEAR, y = RATING)) +
  geom_point(colour = "yellow") +
  geom_smooth(method = "lm", colour = "#EE4000") +
  theme(panel.background = element_rect(fill = "#3A9425"),
        axis.line = element_line(colour = "#EE4000")) +
  annotation_custom(g, xmin=2013, xmax=2016, ymin=0.01, ymax=0.25)




#### multiple actors
library(reshape)

actors <- c("george_clooney", "leonardo_di_caprio", "daniel_daylewis",
            "jamie_foxx", "tom_cruise", "michael_fassbender")

base_url <- "https://www.rottentomatoes.com/celebrity/"
movie_list <- list()

for(actor in actors){
  url <- paste0(base_url, actor, "/")
  page <- read_html(url)
  tb_x <- html_nodes(page, css = "#filmographyTbl")
  tb <- html_table(tb_x)
  movie <- tb[[1]]
  movie_list[[actor]] <- movie
  movie_list[[actor]]$ACTOR <- actor
  names(movie_list[actor]) <- actor
}

all_actors <- merge_all(movie_list)

all_actors <- all_actors %>%
  mutate(CREDIT = gsub("\\r\\n", "", CREDIT)) %>%
  filter(RATING != "No Score Yet",
         CREDIT %ni% c("Producer", "Executive Producer",
                       "Director Producer")) %>%
  mutate(RATING = gsub("%", "", RATING),
         RATING = as.numeric(RATING),
         RATING = RATING/100,
         ACTOR = gsub("_", " ", ACTOR))

movies <- movies %>%
  mutate(ACTOR = "brad pitt")

movies$RATING <- as.numeric(movies$RATING)

str(all_actors)
all_actors$RATING <- as.numeric(all_actors$RATING)

all_actors <- full_join(all_actors, movies) %>%
  select(-c(difference, date_counter))

ggplot(all_actors, aes(y = RATING, x = ACTOR)) +
  geom_boxplot(fill = "#EE4000", colour = "yellow") +
  theme(panel.background = element_rect(fill = "#3A9425"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#EE4000"),
        axis.text.x = element_text(angle = 15, hjust = 1, colour = "#EE4000"),
        axis.text.y = element_text(colour = "#EE4000"),
        axis.ticks = element_line(colour = "#EE4000"),
        axis.title = element_text(colour = "#EE4000")) 
