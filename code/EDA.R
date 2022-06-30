library(tidyverse)
library(wordcloud2)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
credit <- read.csv("./input/credits.csv")
titles <- read.csv("./input/titles.csv")

# country wrangling
country_list <- str_extract_all(titles$production_countries,"[A-Z]+")
n_country <- data.frame(id=titles$id,num_countries=map_dbl(country_list,~length(.)))
country_names <- countrycode(unlist(country_list),origin = "genc2c", destination = "country.name")
df_country <- data.frame(name=unlist(country_names)) %>% filter(is.na(name)==F) %>% 
  group_by(name) %>% summarise(count=n()) %>% arrange(desc(count))

world <- ne_countries(returnclass = "sf") %>% merge(df_country,ny="name")
map <- ggplot(data = world) + geom_sf(aes(fill = count)) + theme_classic() + 
  coord_sf(datum = NA) + xlab("") + ylab("") + 
  theme(panel.grid.major = element_line(color = grey(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"), legend.background = element_rect(fill="transparent"))+
  scale_fill_gradient(low = "#f4f0f0", high = "red", na.value="white")
  
  
# casts wrangling
stakeholders_used <- credit %>% group_by(id) %>% summarise(stakeholders=n_distinct(person_id))
stakeholders_count <- credit %>% group_by(person_id,name) %>% summarise(count=n()) %>% arrange(desc(count))
head(stakeholders_count,10)

# genres wrangling
genre_list <- str_extract_all(titles$genres,"[a-z]+")
n_genre <- data.frame(id=titles$id,num_genres=map_dbl(genre_list,~length(.)))
df_genre <- data.frame(genre=unlist(genre_list)) %>% 
  group_by(genre) %>% summarise(count=n()) %>% arrange(desc(count))
wordcloud2(df_genre)

# imdb score
imdb_raw <- titles$imdb_score %>% as.numeric()
par(mfcol=c(1,2))
boxplot(imdb_raw)
hist(imdb_raw)

par(mfcol=c(1,2))
imdb_treat <- titles[as.numeric(titles$imdb_score)<=10,]$imdb_score %>% as.numeric()
boxplot(imdb_treat)
hist(imdb_treat)
summary(imdb_treat)
