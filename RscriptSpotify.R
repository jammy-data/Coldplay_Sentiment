library(devtools)
library(spotifyr)
library(tidyr)
library(lubridate)
library(dplyr)
library(purrr)
library(genius)
library(stringr)
library(tidytext)
library(wesanderson)
library(knitr)
library(radarchart)
library(tm)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(yarrr)
library(tidyselect)
library(textdata)
library(syuzhet)
library(viridis)
library(proxy)


#Retrieve spotify and genius tokens for use with the API
Sys.setenv(SPOTIFY_CLIENT_ID = '*YOUR CLIENT ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR CLIENT SECRET')
access_token <- get_spotify_access_token()

get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                               scope = spotifyr::scopes
)
#genius token
Sys.setenv(GENIUS_API_TOKEN = "YOUR GENIUS TOKEN")


#Get the coldplay discography
coldplay_disc <- get_discography('coldplay')


#Select the albums we are using within the discography (excl features, EPs etc.)
selected_albums <- c('Parachutes',
                     'A Rush of Blood to the Head',
                     'X&Y',
                     'Viva La Vida or Death and All His Friends',
                     'Mylo Xyloto',
                     'Ghost Stories',
                     'A Head Full of Dreams')
coldplay_disc <- coldplay_disc %>% 
  filter(album_name %in% selected_albums)

#Fix names to match Genius tracks
coldplay_disc$track_name[50] <- 'God Put a Smile Upon Your Face'
coldplay_disc$track_name[26] <- 'Up With the Birds'
coldplay_disc$track_name[31] <- 'Viva la Vida'



#Use genius API to obtain tracklist for each album
Ptracklist <- genius_tracklist(artist = 'Coldplay', album='parachutes')
RBHtracklist <- genius_tracklist(artist = 'Coldplay', album='A rush of blood to the head')
XYtracklist <- genius_tracklist(artist = 'coldplay', album = 'x-y')
VLVtracklist <- genius_tracklist(artist = 'Coldplay', album='viva la vida or death and all his friends')
MXtracklist <- genius_tracklist(artist = 'Coldplay', album='mylo xyloto')
ghosttracklist <- genius_tracklist(artist = 'Coldplay', album='ghost stories')
HFDtracklist <- genius_tracklist(artist = 'Coldplay', album='a head full of dreams')

#merge into one df
tracks_df <- rbind(Ptracklist, RBHtracklist, XYtracklist, VLVtracklist, 
                   MXtracklist, ghosttracklist, HFDtracklist)



#For automatic scraping (sometimes does not work) uncomment below

#lyrics <- map_df(tracks_df$track_url, genius_url)

#For manual
parachutes <- read.csv('Parachutes.csv')
RBH <- read.csv('RBH.csv')
XY <- read.csv('XY.csv')
VLV <- read.csv('Vivalavida.csv')
MX <- read.csv('MyloXyloto.csv')
G <- read.csv('ghost.csv') %>% 
  select(-track_n)
HFD <- read.csv('HFD.csv')

#Rbind together
lyrics <- rbind(parachutes,RBH,XY,VLV,MX,G,HFD)

#merge with the tracklist df
tracks_df <- merge(tracks_df,lyrics)

#There are some NAs - may be instrumental. Remove NAs
tracks_df <- na.omit(tracks_df)


#create a df of non-words or lyrics that are repeated that mean very little
non_words <- data.frame(word = c('oo','ooo','ooh','ah','la','yeah','para','whisper','gonna','wanna','na','wa','hoo','oooooh','woo'))

#Create a tidy version of the lyrics.
tracks_tidy <- tracks_df %>% 
  select(lyric, track_title, track_n, album_name) %>% 
  unnest_tokens(word, lyric) %>% 
  anti_join(non_words) %>% 
  anti_join(stop_words)

album_order_levels <- c('Parachutes',
                        'A Rush of Blood to the Head',
                        'X&Y',
                        'Viva La Vida or Death and All His Friends',
                        'Mylo Xyloto',
                        'Ghost Stories',
                        'A Head Full of Dreams')

#rder the albums for visualisation in order of release
tracks_tidy$album_name <- 
  factor(tracks_tidy$album_name,
         levels= album_order_levels)
cbbPalette <- c("#F4BE2C", "#A0A0A0", "#086391", "#f5f5dc", "#FF00EE", "#79FAF6", "#F14528")

## Lyrical analysis

#Create a df with a distinct word count
word_summary <- tracks_tidy %>%
  group_by(album_name, track_title) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(track_title, album_name, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup() 

#Change the names so that they can fit in the pirateplot. Cannot swap axes.
word_summary$album_name <- str_replace_all(word_summary$album_name,'A Rush of Blood to the Head','A Rush of\nBlood to the Head')
word_summary$album_name <- str_replace_all(word_summary$album_name,'A Head Full of Dreams','A Head\nFull of Dreams')
word_summary$album_name <- str_replace_all(word_summary$album_name,'Viva La Vida or Death and All His Friends','Viva La Vida\nor Death and All\nHis Friends')

#Order the albums by release
word_summary$album_name <- 
  factor(word_summary$album_name,
         levels= c('Parachutes',
                   'A Rush of\nBlood to the Head',
                   'X&Y',
                   'Viva La Vida\nor Death and All\nHis Friends',
                   'Mylo Xyloto',
                   'Ghost Stories',
                   'A Head\nFull of Dreams'))


### Term diversity and frequency

#Create a pirateplot
pirateplot(formula =  word_count ~ album_name,
           data = word_summary, 
           xlab = "",ylab = "Song Distinct Word Count",
           point.o = .2, 
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0,
           point.pch = 16,
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .8, cex.names = .5) #Axis label size


#Assign an IDF value for each word in tidytracks
tracksTFIDF <- 
  tracks_tidy %>%
  count(album_name, word, sort=TRUE) %>% 
  bind_tf_idf(word, album_name, n) %>% 
  arrange(desc(tf_idf))


#Facet plot for IDF for each word/album
tracksTFIDF %>%
  group_by(album_name) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = album_name)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ album_name, scales = "free_y") + 
  ylab("tf-idf score") +
  coord_flip()



### Sentiment analysis

#Bing sentiments
coldplay_bing <- tracks_tidy %>% 
  merge(get_sentiments('bing'))

#Reorder the albums by release
coldplay_bing$album_name <- 
  factor(coldplay_bing$album_name,
         levels= album_order_levels)

#afinn sentiments
coldplay_afinn <- tracks_tidy %>% 
  inner_join(get_sentiments('afinn'))



tracks <- unique(coldplay_bing$track_title)
perc_sent=data.frame(tracks)
perc_sent$perc_neg <- 0
perc_sent$perc_pos <- 0
rownames(perc_sent) <- tracks

for(i in tracks) {
    n_sent <- as.numeric(coldplay_bing %>% 
                          filter(track_title == i) %>% 
                          summarise(count = n()) )
    perc_list <- coldplay_bing %>% 
    filter(track_title == i) %>% 
    group_by(sentiment) %>% 
    summarise(count = n()) %>% 
    mutate(perc = (count/n_sent)*100)
    
    perc_sent[i,2] <- perc_list$perc[1]
    perc_sent[i,3] <- perc_list$perc[2]
}


#Sentiment contribution for each album Figure 3.
coldplay_bing %>%
  group_by(album_name) %>% 
  count(word, sentiment, sort = TRUE) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=album_name, y=n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  ylim(-100,100) +
  labs(y = "Contribution to sentiment",
       x = '')

#Quantify the sentiment into + - figures.
quant_sentiment <- coldplay_bing %>%
  group_by(track_title) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% 
  summarise(quant_n = sum(n))


#Get the nrc sentiments for each word
nrc_data <- get_nrc_sentiment(tracks_tidy$word)

#deselect positive and negative as sentiments
nrc_data <- nrc_data %>% 
  select(-c('positive','negative'))

#redistribute binary variabes into single categorical column
nrc_data <- tibble::rowid_to_column(nrc_data, "ID")
nrc_data <- cbind(nrc_data[1L], 
                  sentiment = names(nrc_data[-1L])[max.col(nrc_data[-1L] == 1L)])

#combine tracks and sentiment
sentiment_tracks <- cbind(tracks_tidy, nrc_data)


#calculate total for each sentiment per album
coldplay_nrc <- sentiment_tracks %>%
  group_by(album_name, sentiment) %>%
  count(album_name, sentiment) %>% 
  select(album_name, sentiment, total_sent = n)

#Calculate total sentiments for each album
albums_nrc <- sentiment_tracks %>%
  count(album_name) %>% 
  select(album_name, total_album_sent = n)

#Calculate the percentage of each sentiment as a total of all sentiments
radar_prep <- coldplay_nrc %>% 
  inner_join(albums_nrc, by = "album_name") %>% 
  mutate(percent = round((total_sent/total_album_sent * 100), 0)) %>% 
  select(-total_sent, -total_album_sent) %>%
  spread(album_name, percent)

#Change the order of the chart so negatives and positive sentiments are either side
radar_prep <- radar_prep[c(2,7,5,8,4,3,1,6),]


#Radar chart - interactive. Figure 4.
chartJSRadar(radar_prep,
             polyAlpha = 0.1,
             lineAlpha = 0.5,
             addDots = FALSE,
             maxScale = 20,
             scaleStepWidth = 5)


# Audio features

#Create a list of songs that match the lyric DB
song_list <- unique(tracks_df$track_title[unique(tracks_df$track_title) %in% coldplay_disc$track_name])

#Filter the discography to this tracklist
coldplay_tidy <- coldplay_disc %>% 
  select(album_name,track_name,album_release_year,danceability,energy,speechiness,instrumentalness,valence,tempo, key_mode) %>% 
  distinct() %>% 
  filter(track_name %in% song_list)

#Shorten the viva la vida album name
coldplay_tidy$album_name <- replace(coldplay_tidy$album_name, coldplay_tidy$album_name =="Viva La Vida or Death and All His Friends", "Viva La Vida")
coldplay_tidy$album_name <- 
  factor(coldplay_tidy$album_name,
         levels = c('Parachutes',
         'A Rush of Blood to the Head',
         'X&Y',
         'Viva La Vida',
         'Mylo Xyloto',
         'Ghost Stories',
         'A Head Full of Dreams'))

#Ensure only tracks with the same name are in the dataframe
same_tracks <- perc_sent$tracks[perc_sent$tracks %in% coldplay_tidy$track_name]
perc_sent <- perc_sent %>% 
  filter(tracks %in% same_tracks)



#Look at valence, energy and tempo
track_attributes <- coldplay_tidy %>% 
  select(track_name, album_name, valence, tempo, energy) %>% 
  pivot_longer(cols = c(valence, tempo, energy))


#Figure 5
track_attributes %>% 
  group_by(album_name) %>% 
  ggplot() +
  aes(x=album_name, y=value, fill=album_name) +
  geom_violin(draw_quantiles = 1, show.legend = FALSE, alpha = 0.3) +
  geom_boxplot(show.legend = FALSE,width=0.15, alpha=0.7) +
  facet_wrap(~name, scales = 'free_y',ncol = 1) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "",
       y= "Score")





attribute_level <-
  track_attributes %>% 
  group_by(album_name, name) %>% 
  summarise(IQR = IQR(value),median = median(value))

lyrics_stats <- merge(coldplay_tidy,perc_sent, by.x = 'track_name', by.y = 'tracks') %>% 
  select(track_name,album_name,perc_pos,valence) %>% 
  mutate(perc_pos = perc_pos/100)
#There are 0 values, but these can be removed (automatically) as these are likely instrumentals or not picked up by the bing lexicon.

lyrics_stats2 <- merge(coldplay_tidy,quant_sentiment, by.x = 'track_name', by.y = 'track_title') %>% 
  select(track_name,album_name,quant_n,valence)


heatcol <- c("#3399ff","#ffff00")

lyrics_stats2 %>% 
  group_by(album_name) %>% 
  mutate(sent_scale = scale(quant_n)) %>% 
  mutate(valence_scale = scale(valence)) %>% 
  pivot_longer(c(sent_scale, valence_scale)) %>% 
  #  filter(album_name == 'Parachutes') %>% 
  ggplot() +
  aes(x=name, y=track_name) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis()+
  #scale_fill_gradient(low = "#3399ff", high = "#ffff00", breaks=c(-2,2), labels = c("More negative", "Less Negative")) +
  facet_wrap(~album_name, scales = 'free_y', ncol = 2)


ggplot(data=lyrics_stats2) +
  aes(x=quant_n,y=valence, color=album_name) +
  geom_point(show.legend = FALSE)


# Clustering

sent_audio <- merge(coldplay_tidy,quant_sentiment, by.x = 'track_name', by.y = 'track_title')
sent_audio <- sent_audio %>% 
  select(track_name, valence, tempo, energy, quant_n)
sent_audio_mat <- sent_audio %>% 
  select(-track_name)
rownames(sent_audio_mat) <- sent_audio$track_name

#Create a scaled dendrogram data object
docsdissim <- dist(scale(sent_audio_mat)) #Similarity matrix
h <- hclust(docsdissim, method = "ward.D") # Hierarchical Clustering
dend <- as.dendrogram(h) #Dendrogram obj
dend_data <- dendro_data(dend, type = "rectangle") #Dendogram data object
#Join album data
dend_data$labels <- left_join(dend_data$labels,
                              unique(coldplay_tidy[,c("album_name", "track_name")]),
                              by = c("label" = "track_name"))


library(ggdendro)
#Create a dataframe in a matrix format
tracks_mat_prep <- tracks_tidy %>% 
  group_by(word) %>% 
  na.omit %>% 
  mutate(howmany = n()) %>% 
  select(track_title,word, howmany) %>% 
  pivot_wider(names_from = word,values_from = howmany, values_fn = length, values_fill = 0)

#Move the tracks_name column to the row.names
tracks_mat <- tracks_mat_prep %>% 
  select(-track_title)
rownames(tracks_mat) <- tracks_mat_prep$track_title

#Create a scaled dendrogram data object
docsdissim <- dist(scale(tracks_mat)) #Similarity matrix
h <- hclust(docsdissim, method = "ward.D") # Hierarchical Clustering
dend <- as.dendrogram(h) #Dendrogram obj
dend_data <- dendro_data(dend, type = "rectangle") #Dendogram data object



ggplot(dend_data$segments) +
  geom_text(data = dend_data$labels, aes(x, 0, label = label, color=album_name), 
            hjust = 1, size = 2) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  scale_color_discrete(name = "Album Name") +
  theme_dendro() +
  coord_flip() +
  theme(legend.key.size = unit(1,'cm')) +
  scale_y_continuous(expand = c(0.3,0))


