
#Loading necessary libraries

library(readr)
library(ggridges)
library(viridis)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(spotifyr)
library(tuber)
library(plotly)
library(knitr)
library(gghighlight)
library(gridExtra)
library(kableExtra)
library(tuber)


# Spotify

# Get artist info
# Using Spotify web player, search for your favorite artist. You can find their ID in the web address
# For instance: `https://open.spotify.com/artist/...` the ID is after the backflash

# Taylor Swift Spotify ID
taylor_id <- "06HL4z0CvFAxyc27GXpf02"

# Discography
taylorswift_discography <- get_artist_audio_features(
  artist = "06HL4z0CvFAxyc27GXpf02",
  include_groups = "album")

# Singles
taylorswift_singles <- get_artist_audio_features(
  artist = "06HL4z0CvFAxyc27GXpf02",
  include_groups = "single")
  
# Album ID
"1KVKqWeRuXsJDLTW0VuD29"				
"1NAmidJlEaVgA3MpcPFYGq"
"2fenSS68JI1h4Fo296JfGr"
"2gP2LMVcIFgVczSJqn340t"
"2Xoteh7uEpea4TohMxjtaq"			
"4hDok0OAJd57SGIT8xuWJH"
"5eyZZoQEFQWRHkV2xgAeBw"
"5fy0X0JmZRZnVa2UEicIOl"
"6DEjYFkNZh67HP7R9PSZvv"
"6S6JQWzUrJVcJLK4fi74Fw"


# Mean valence
albums_mean_scores <- official_albums %>% 
  group_by(album_name, album_release_year) %>% 
  summarize(mean_dance= round(mean(danceability),3), mean_energy= round(mean(energy),3), mean_valence= round(mean(valence),3)) %>% 
  arrange(album_release_year)

# Using this to lock the album name order as release year 
albums_mean_scores$album_name <- factor(albums_mean_scores$album_name, levels = albums_mean_scores$album_name)

ggplot(albums_mean_scores, aes(x= album_name, y= mean_valence, color= album_name)) +
  geom_segment(aes(x= album_name, xend= album_name, y= 0, yend= mean_valence)) +
  geom_point(size= 2, color= "cyan3") +
  scale_color_viridis(discrete= TRUE, guide= FALSE, option="D") +
  theme_light() +
  theme (
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
  ) +
  labs(title = "Average valence per album") +
  xlab("") +
  ylab("Valence") +
  coord_flip()

  

# Valence scale
official_albums %>% 
  group_by(album_name, album_release_year) %>% 
  arrange(album_release_year)

# Using this to lock the album name order as release year  
official_albums$album_name <- factor(official_albums$album_name, levels = rev(unique(official_albums$album_name)))

ggplot(official_albums, aes(y = album_name, x = valence, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.6) + 
  scale_fill_gradient(low= "white", high= "deepskyblue4") +
  theme_light() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  ylab("") +
  theme(legend.position = "none") 


# Most cheerful songs
official_albums %>% 
  select(track_name, album_name, valence) %>% 
  top_n(10) %>% 
  arrange(-valence) %>% 
  kbl() %>% 
  kable_paper(full_width = F) %>% 
  row_spec(row= 1:10, background = "#BF1363", color = "white")


#Energy and danceability

# Using this to lock the album name order as release year  
official_albums$album_name <- factor(official_albums$album_name, levels = rev(unique(official_albums$album_name)))

ggplot(official_albums, aes(x= energy, y= danceability, color= album_name)) +
  geom_jitter() +
  scale_color_viridis(discrete= TRUE, option="C") +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks= seq(0, 1, 0.1)) +
  scale_y_continuous(breaks= seq(0, 1, 0.1)) +
  labs(title= "How energetic and danceable are Taylor's songs?") +
  theme_light()


# Popularity per album
official_albums %>% 
  group_by(album_name, album_release_year) %>% 
  arrange(album_release_year)

# Using this to lock the album name order as release year  
official_albums$album_name <- factor(official_albums$album_name, levels = rev(unique(official_albums$album_name)))

ggplot(official_albums, aes(y = album_name, x = popularity, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.6) + 
  scale_fill_gradient(low= "white", high= "darkorange") +
  theme_light() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  ylab("") +
  theme(legend.position = "none")


# Top popular tracks
official_albums %>% 
  select(track_name, album_name, popularity) %>% 
  arrange(-popularity) %>%
  top_n(10) %>% 
  kbl() %>% 
  kable_paper(full_width = F) %>% 
  row_spec(row= 1:12, background = "#BF1363", color = "white")


#Loading Spotify top 200
spotify.csv

### Songs in top 10

#Taylor in top 10
spotify %>% 
  select(Song_Name, Artist, Highest_Charting_Position, Streams) %>% 
  filter(Artist== "Taylor Swift", Highest_Charting_Position <= 10) %>% 
  arrange(Highest_Charting_Position) %>% 
  kbl() %>% 
  kable_paper(full_width= F) %>% 
  row_spec(row= 1:8, background= "#00A19D", color= "#FFF8E5")


#Taylor in top 200
spotify %>% 
  group_by(Artist, Artist_Followers) %>% 
  filter(Artist== "Taylor Swift") %>% 
  summarize(Song_Count= n()) %>% 
  kbl() %>% 
  kable_paper(full_width= F) %>% 
  row_spec(row= 1, background= "#00A19D", color= "#FFF8E5")


### Songs and followers comparison

spotify %>% 
  group_by(Artist, Artist_Followers) %>%
  summarize(Songs = n(), Streams = sum(Streams)/1000000, Followers = mean(Artist_Followers)/ 1000000) %>% 
  arrange(desc(Songs)) %>% 
  filter(Songs >= 10) %>% 
  ggplot(aes(y = Songs, x = Followers, color = Artist, size = Streams)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(.1, 8), name="Streams in millions") +
  scale_color_viridis(discrete = TRUE, option= "D") +
  labs(title = "Total songs versus followers and streams", caption = "Data: Spotify Top 200 2020-2021") +
  annotate("text", x = 48, y = 49, label = "Taylor Swift", color="maroon", fontface = "italic") +
  ylab("Total songs") +
  xlab("Followers in millions") +
  theme_light()


### Streams

# Top 10 streams
spotify %>% 
  filter(Artist== "Taylor Swift") %>% 
  select(Song_Name, Artist, Streams) %>% 
  arrange(-Streams) %>% 
  top_n(10) %>% 
  kbl() %>% 
  kable_paper(full_width= F) %>% 
  row_spec(row= 1: 10, background= "#00A19D", color= "#FFF8E5")


# Youtube
taylor_channel <- get_all_channel_video_stats(channel_id = "UCANLZYMidaCbLQFWXBC95Jg")

# Taylor vevo filtered

taylor_vevo <- taylor_channel %>% 
  subset(!(id %in% c("J21E6MecXTg", "X6kGV2mTbj8", "FMDVWSqALRs")))

taylor_vevo$viewCount <- as.numeric(taylor_vevo$viewCount)
taylor_vevo$likeCount <- as.numeric(taylor_vevo$likeCount)
taylor_vevo$dislikeCount <- as.numeric(taylor_vevo$dislikeCount)
taylor_vevo$commentCount <- as.numeric(taylor_vevo$commentCount)
taylor_vevo$publication_date <-as.Date(taylor_vevo$publication_date)


## Views/ likes/ dislikes

# Videos per year
taylor_vevo %>% 
  group_by(publication_year= as.numeric(format(publication_date, "%Y"))) %>% 
  summarize(videoCount= n()) %>% 
  kbl %>% 
  kable_paper(full_width = F) %>% 
  row_spec(row= 1:13, background = "#90AACB", color = "#FFF8E5")


## Top views

# Top 10 views
taylor_vevo %>% 
  select(title, viewCount, likeCount) %>% 
  arrange(-viewCount) %>% 
  top_n(10) %>% 
  kbl %>% 
  kable_paper(full_width = F) %>% 
  row_spec(row= 1:10, background = "#6D9886", color = "#FFF8E5")


# Views vs Likes
taylor_vevo %>% 
  group_by(title) %>% 
  summarize(likes= likeCount/1000, dislikes= dislikeCount/1000, views= viewCount/100000) %>% 
  ggplot(aes(x= views, y= likes, color= title)) +
  geom_jitter(show.legend = FALSE) +
  scale_color_viridis(discrete= TRUE, option="C") +
  labs(title= "Likes versus Views") +
  xlab("Views (x100 thousand)") +
  ylab("Likes in thousand") +
  theme_light()


#LikesvsDislikescsView
taylor_vevo %>% 
  group_by(title) %>% 
  summarize(likes= likeCount/1000, dislikes= dislikeCount/1000, views= viewCount/1000) %>% 
  ggplot(aes(x= dislikes, y= likes, color= title, size= views)) +
  geom_jitter(show.legend = FALSE, alpha= 0.5 ) +
  scale_color_viridis(discrete= TRUE, option="D") +
  labs(title= "Likes versus Dislikes versus Views") +
  xlab("Dislikes in thousands") +
  ylab("Likes in thousands") +
  theme_light()


# Most hate vids
taylor_vevo %>% 
  select(title, dislikeCount, likeCount) %>% 
  arrange(-dislikeCount) %>% 
  top_n(10) %>% 
  mutate(dislikesPercent = round(dislikeCount/likeCount, 3)*100) %>%
  kbl %>% 
  kable_paper(full_width = F) %>% 
  row_spec(row= 1:10, background = "#6D9886", color = "#FFF8E5")
