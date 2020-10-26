#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')
install.packages("corrplot")

#LOAD THE GGPLOT2 LIBRARY (MUST BE DONE EVERY TIME)
library(ggplot2)
library(corrplot)
library(plyr)
library(tseries)

###################################################################
###   Read CSV files containing all spotify tracks and data     ###
### cleaning column names by making vector with new column names###
###################################################################

### import dataset
spotify_data <- read.csv("https://raw.githubusercontent.com/gregoriodelrio/PROJECT-1-ECON-494-F20/main/Spotify%20Tracks.csv")

### correcting variable headers
var_names <- c("genre", "popularity", "acousticness", "danceability", "duration_min", "energy", "instrumentalness", "loudness", "speechiness", "tempo")
names(spotify_data) <- var_names

## concerting milliseconds into minutes
spotify_data$duration_min <- spotify_data$duration_min / 60000
View(spotify_data)

###################################################################
####  Giving each column in spotify_data its own variable       ###
###################################################################
genre <- spotify_data$genre
popularity <- spotify_data$popularity
acousticness <- spotify_data$acousticness
danceability <- spotify_data$danceability
duration_min <- spotify_data$duration_min
energy <- spotify_data$energy
instrumentalness <- spotify_data$instrumentalness
loudness <- spotify_data$loudness
speechiness <- spotify_data$speechiness
tempo <- spotify_data$tempo

View(genre)

###########################
### Should be 26 genres ###
###########################
length(unique(genre))


###################################################################
####  calculating mean of column variable                       ###
###################################################################
popularity_mean <- data.frame(aggregate(popularity ~ genre, spotify_data, mean))
acousticness_mean <- data.frame(aggregate(acousticness ~ genre, spotify_data, mean))
danceability_mean <- data.frame(aggregate(danceability ~ genre, spotify_data, mean))
duration_mean <- data.frame(aggregate(duration_min ~ genre, spotify_data, mean))
energy_mean <- data.frame(aggregate(energy ~ genre, spotify_data, mean))
instrumentalness_mean <- data.frame(aggregate(instrumentalness ~ genre, spotify_data, mean))
loudness_mean <- data.frame(aggregate(loudness ~ genre, spotify_data, mean))
speechiness_mean <- data.frame(aggregate(speechiness ~ genre, spotify_data, mean))
tempo_mean <- data.frame(aggregate(tempo ~ genre, spotify_data, mean))

View(popularity_mean)


###################################################################
####  1. % of each genre for dataset                            ###
###################################################################
count_genres <- matrix(0,26,4)
genre_matrix <- matrix(unique(genre))

for (i in 1:dim(count_genres)[1]) {
  count_genres[i,2] <- sum(spotify_data$genre == genre_matrix[i])
}

song_count <- sum(count_genres[,2])
song_count # should be 232,725

for (i in 1:dim(count_genres)[1]) {
  count_genres[i,3] <- count_genres[i,2] / song_count
}

count_genres <- data.frame(count_genres)

### add header names to count_genres
count_genres_names <- c("genre", "count","percentage_num", "percentage")
names(count_genres) <- count_genres_names


### change percentage and count to numeric
count_genres$percentage <- as.numeric(count_genres$percentage)
count_genres$count <- as.numeric(count_genres$count)
count_genres[,1] <- unique(spotify_data$genre)

### making 4th column in count_genres labels for pie chart
count_genres$percentage <- paste(round(count_genres$percentage_num*100, 2), " %")


### checking classes
class(count_genres$genre) # character
class(count_genres$percentage) # character
class(count_genres$percentage_num) # numeric
class(count_genres$count) # numeric

View(count_genres)

count_genres$percentage

### bar chart with ordered genre count
ggplot(count_genres, aes(x= count, y= reorder(genre, count), width=0.6)) +
  geom_bar(stat = "identity", aes(fill= count)) +
  geom_text(aes(x= count, y= genre, label= count, hjust= 0), size= 4) +
  theme(legend.position = "none") +
  xlab("Count") + ylab("Genre")

### pie chart with genre percentage
ggplot(count_genres, aes(x= "", y= percentage_num, fill= genre)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start= 0) +
  theme_void()

### not finished. data labels to close to center. 
ggplot(count_genres, aes(x= "", y= percentage_num, fill= genre)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start= 0) +
  theme_void() +
  geom_text(aes(y= percentage_num/26 + c(0,cumsum(percentage_num)[-length(percentage_num)]), 
                label= paste(round(percentage_num*100,2), " %"))) 


###################################################################
####  2. histogram genres by popularity                         ###
###################################################################
ggplot(spotify_data, aes(x= popularity)) +
  geom_histogram() +
  facet_wrap(~genre) +
  geom_vline(xintercept = mean(spotify_data$popularity), color="blue") +
  ylim(c(0,2000)) +
  xlab("Popularity") + ylab("Count")


###################################################################
####  3. correlation  plot                                      ###
###################################################################
variables_corr <- data.frame(popularity, acousticness, danceability, 
                             duration_min, energy, instrumentalness, 
                             loudness, speechiness, tempo)
corrplot(cor(variables_corr))
corrplot(cor(variables_corr), method = "number")

covar <- cov(spotify_data[,2:10])
View(covar)

###################################################################
####  4. popularity horizontal barchart and rank                ###
###################################################################
ggplot(popularity_mean, aes(popularity, reorder(genre, popularity))) +
  geom_bar(stat = "identity", aes(fill= popularity)) +
  geom_text(aes(x=popularity, y= genre, label= round(popularity,0), hjust=0)) +
  theme(legend.position = "none") +
  xlab("Popularity") + ylab("Genre")

summary(popularity_mean)

###################################################################
####  5. Boxplot each genre by column variable                  ###
###################################################################

# next to each boxplot each be a list ranked by popularity 
# with the according genre std. 

###  1. genre:popularity  ###
ggplot(spotify_data, aes(popularity, reorder(genre, popularity))) + 
  geom_boxplot() +
  xlab("Popularity") + ylab("Genre")
summary(popularity_mean)

View(popularity_mean[order(-popularity_mean$popularity),])

###  2. genre:acousticness  ###
ggplot(spotify_data, aes(x=acousticness, y= reorder(genre, acousticness))) + 
  geom_boxplot() +
  xlab("Acousticness") + ylab("Genre")
summary(acousticness_mean)

View(acousticness_mean[order(-acousticness_mean$acousticness),])

###  3. genre:danceability  ###
ggplot(spotify_data, aes(x=danceability, y= reorder(genre, danceability))) + 
  geom_boxplot() +
  xlab("Danceability") + ylab("Genre")
summary(danceability_mean)

View(danceability_mean[order(-danceability_mean$danceability),])

###  4. genre:energy  ###
ggplot(spotify_data, aes(x=energy, y= reorder(genre, energy))) + 
  geom_boxplot() +
  xlab("Energy") + ylab("Genre")
summary(energy_mean)

View(energy_mean[oreder(-duration_mean$duration_min),])

###  5. genre:instrumentalness  ###
ggplot(spotify_data, aes(x=instrumentalness, y= reorder(genre, instrumentalness))) + 
  geom_boxplot() +
  xlab("Instrumentalness") + ylab("Genre")
summary(instrumentalness_mean)

View(instrumentalness_mean[order(-instrumentalness_mean$instrumentalness),])

###  6. genre:loudness  ###
ggplot(spotify_data, aes(x=loudness, y= reorder(genre, loudness))) + 
  geom_boxplot() +
  xlab("Loudness") + ylab("Genre")
summary(loudness_mean)

View(loudness_mean[order(-loudness_mean$loudness),])

###  7. genre:tempo  ###
ggplot(spotify_data, aes(x=tempo, y= reorder(genre, tempo))) + 
  geom_boxplot() +
  xlab("Tempo") + ylab("Genre")
summary(tempo_mean)

View(tempo_mean[order(-tempo_mean$tempo),])


############################################################################
####  6. Linear Regression for popularity w/ strongest column variables  ###
############################################################################
pop_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy 
             + instrumentalness + loudness + speechiness + tempo, spotify_data)
summary(pop_lm)



#################################################
###   DID NOT USE CODES BELOW FOR REPOT        ##
#################################################

###################################################################
####  7. order groups of 20 in popularity                       ###
###################################################################

### import from excel files ###
eightyone_hundred <- subset(spotify_data, popularity > 80)
sixtyone_eighty <- subset(spotify_data, popularity > 60 & popularity<=80)
fortyone_sixty <- subset(spotify_data, popularity > 40 & popularity<=60)
twentyone_forty <- subset(spotify_data, popularity > 20 & popularity<=40)
zero_twenty <- subset(spotify_data, popularity > 0 & popularity<=20)

View(forty_sixty)

###################################################################
####  8. Correlation between bottom 20 and top 20               ###
###################################################################

### over-writing these variables with eightyone_hundred data to get corr of eightyone-hundred data
genre <- eightyone_hundred$genre
popularity <- eightyone_hundred$popularity
acousticness <- eightyone_hundred$acousticness
danceability <- eightyone_hundred$danceability
duration_min <- eightyone_hundred$duration_min
energy <- eightyone_hundred$energy
instrumentalness <- eightyone_hundred$instrumentalness
loudness <- eightyone_hundred$loudness
speechiness <- eightyone_hundred$speechiness
tempo <- eightyone_hundred$tempo

corr_eighty_hundred <- data.frame(popularity, acousticness, danceability, duration_min, energy, instrumentalness, loudness, speechiness, tempo)
corrplot(cor(corr_eighty_hundred))

genre <- zero_twenty$genre
popularity <- zero_twenty$popularity
acousticness <- zero_twenty$acousticness
danceability <- zero_twenty$danceability
duration_min <- zero_twenty$duration_min
energy <- zero_twenty$energy
instrumentalness <- zero_twenty$instrumentalness
loudness <- zero_twenty$loudness
speechiness <- zero_twenty$speechiness
tempo <- zero_twenty$tempo

corr_zero_twenty <- data.frame(popularity, acousticness, danceability, duration_min, energy, instrumentalness, loudness, speechiness, tempo)
corrplot(cor(corr_zero_twenty))

###################################################################
####  9. Linear Regression of top popularity 20 group           ###
###################################################################
eighty_hundred_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy + instrumentalness + loudness + speechiness + tempo, eightyone_hundred)
summary(eighty_hundred_lm)

sixty_eighty_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy + instrumentalness + loudness + speechiness + tempo, sixtyone_eighty)
summary(sixty_eighty_lm)

forty_sixty_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy + instrumentalness + loudness + speechiness + tempo, fortyone_sixty)
summary(forty_sixty_lm)

twenty_forty_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy + instrumentalness + loudness + speechiness + tempo, twentyone_forty)
summary(twenty_forty_lm)

zero_twenty_lm <- lm(popularity ~ acousticness + danceability + duration_min + energy + instrumentalness + loudness + speechiness + tempo, zero_twenty)
summary(zero_twenty_lm)


###################################################################
####  Sorting Data             ###
###################################################################

orderdata <- spotify_avg[order(-spotify_avg$danceability),]
View(orderdata)





