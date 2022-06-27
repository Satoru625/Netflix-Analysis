library(tidyverse)
library(wordcloud2)
library(countrycode)
credit <- read.csv("./input/credits.csv")
titles <- read.csv("./input/titles.csv")

# country wrangling
country_list <- str_extract_all(titles$production_countries,"[A-Z]+")
n_country <- data.frame(id=titles$id,num_countries=map_dbl(country_list,~length(.)))
country_names <- countrycode(unlist(country_list),origin = "genc2c", destination = "country.name")
df_country <- data.frame(country=unlist(country_names)) %>% filter(is.na(country)==F) %>% 
  group_by(country) %>% summarise(count=n()) %>% arrange(desc(count))

# casts wrangling
stakeholders_used <- credit %>% group_by(id) %>% summarise(stakeholders=n_distinct(person_id))
stakeholders_count <- credit %>% group_by(person_id,name) %>% summarise(count=n()) %>% arrange(desc(count))

# genres wrangling
genre_list <- str_extract_all(titles$genres,"[a-z]+")
n_genre <- data.frame(id=titles$id,num_genres=map_dbl(genre_list,~length(.)))
df_genre <- data.frame(genre=unlist(genre_list)) %>% 
  group_by(genre) %>% summarise(count=n()) %>% arrange(desc(count))
wordcloud2(df_genre)
