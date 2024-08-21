# Bio319 assessment 2 script 
# Luisa Shanti Defiebre         SORRY FOR ANY SPELLING ERRORS ---> Dyslexia
# 210535530
# deadline: 08/03/2024


#tidy data rules:
  #Each row is an observation
  #Each column is a variable
  #Each value has its own cell

# packages i will need :
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)

#First things first, let put the data in a table so i can see it better:
spotify <- read.table("Spotify_Messy_210535530.txt", sep="\t", header=T)
spotify.original <- read.table("Spotify_Messy_210535530.txt", sep="\t", header=T) #so i can keep an eye on the original data

#trying to understand the data:
View(spotify)
str(spotify) 
summary(spotify)
head(spotify)

# main problems i see:
#  $danceability.energy, seem to be 2 colums in 1, so need to separate them 
#  $pop , $rap , $rock , $r.b are not variables, so should not be columns
#  there is a mistake with the dates of $track_album_release_date, seems to be that every 2019 was replaced with a 75.
#  $ mode has ramdom "T" which i need to remove 
# the class of variables are wrong in : $ dancebility,  $energy, $mode,  $ track_album_release_date
# misspellings in  $ track_artist 
# the playlist id is not unique an dnot consistent, it should be 1 id per playlist

 
 
 
# going one by one:   
   
# $track_id 
   str(spotify$track_id)
   table(spotify$track_id) #looks good, 
   spotify$track_id[is.na( spotify$track_id)] # some NA ...
   #checking if any ids are repeated more than once:
   spotify$track_id[table(spotify$track_id) > 1]
   #"41knNw2OhgF9gouyy1squL" is used trwice for 2 different playlist... big problem
   #these codes are used, the same code for track_id, album_id, playlist_id... , repeating the same ID 3 times...big problem 
   #ID need to be UNIQUE
   
   
# $track_name, name of column should not have capital letters, and the numbers are not needed 
   spotify <- spotify  %>%
     rename(track_name = "TTrack_nameff5") #changing the name of column
   str(spotify$track_name)
   spotify$track_name[is.na(spotify$track_name)] #no NA
   
   
# $track_artist
   #i know it has spelling mistakes, and i know they will be in:
     #Shakira, Taylor Swift, Janis Joplin, The Four Owls, and Bad Bunny
     #therefore, going use grep to search for short parts of their name (e.g,"sha", "aki", "ira" --> to see if i have the artist Shakira, misspelled in my data)
     spotify$track_artist[grep(spotify$track_artist, pattern = "Bad")]
       #the only artist i found in my data being misspelled is Bad Sunny --> Bad Bunny, using gsub to fix the spelling mistake:
       spotify <- spotify %>%
         mutate('track_artist' = gsub(pattern = "Bad Sunny", replacement = "Bad Bunny", track_artist)  )
   #checking everything else:
   table(spotify$track_artist) #obviously there is no consistency, there is numbers and capital letters, but leaving like that, cause they are artists names
   str(spotify$track_artist)
   spotify$track_artist[is.na(spotify$track_artist)] #no NAs
   #everything else is looking good
   
   
# $track_popularity, checking if values are between 0 and 100:
     summary(spotify$track_popularity) #looks good, values are between 0 and 100
     str(spotify$track_popularity)
     #checking if any decimals in data:
     table(spotify$track_popularity) #numbers without decimal points, so class :int is appropriate
   
     
# $track_album_id, 
     str(spotify$track_album_id) #class is right, 
     spotify$track_album_id[is.na(spotify$track_album_id)] #some NA...
     table(spotify$track_album_id)[table(spotify$track_album_id) > 1] 
     spotify[grep(pattern = "0yTlrnue3pJTJd7h7d43mk", x= spotify)] 
     #when i search for the output in the data table, i see same ID reused for track_id, track_album_id, and playlist_id
     #i also see, when it is the same album, the id repeats, which is good
     
     
# $track_album_name
     str(spotify$track_album_name)
     spotify$track_album_name[is.na(spotify$track_album_name)] #no NAs
     #see wich albums are repeated:
     table(spotify$track_album_name)[table(spotify$track_album_name) > 1]
     #i see that "afterglow" for exaple is repeated, but then the album ID is a different one, although it is the same album... should it not be the same album_ID?

     
# track_album_release_date,
    str(spotify$track_album_release_date) # class is wrong
    table(spotify$track_album_release_date)
     #there is a mistake in $track_album_release_date, seems like every "75" should be a "2019", so to fix that :
         spotify <- spotify %>%
           mutate('track_album_release_date' = gsub(pattern = "75", replacement = "2019", track_album_release_date)) 
     #class is wrong so changing it to date class:
         spotify$track_album_release_date <- as.Date(spotify$track_album_release_date, format = "%Y-%m-%d")
    #checking for NA:
     spotify$track_album_release_date[is.na(spotify$track_album_release_date)] #some NAs, but should not be a problem
     
     
#$ playlist_name 
    str(spotify$playlist_name)
    table(spotify$playlist_name)
    #all looks good, 
     #checking for NA:
     spotify$playlist_name[is.na(spotify$playlist_name)]#no NA

     
#$playlist_id
   str(spotify$playlist_id)
   #checking for NAs:
   spotify$playlist_id[is.na(spotify$playlist_id)]# some NA,as analysis is on playlist, this is crucial 
   table(spotify$playlist_id) #BIG problem --> id is not consistent, right now each playlist has several ids, each playlist should have one consistent id
   #when i search on the data table results from table(spotify$playlist_name), and look at the playlist_ID:
   #i see the same playilist having different playlists ID...but even more problematic, the same ID "0yTlrnue3pJTJd7h7d43mk" e.g, is used for two different playlist ID...
   # WILL SOLVE THIS PROBLEM AT THE END
   
# $dancebility.energy   
   str(spotify$dancebility.energy)
   #the dancebility and energy are both together in the same column,values are separated. by "_"
   #separating these into 2 different columns:
     spotify <- spotify %>% 
       separate_wider_delim("danceability.energy","_",
                            names=c("dancebility",
                                    "energy")) 
   # both columns are class character (chr), but should but numeric (num) type, so changing that:
     spotify$dancebility <- as.numeric(spotify$dancebility)
     spotify$energy <- as.numeric(spotify$energy)
   #checking for NAs:
     spotify$dancebility[is.na(spotify$dancebility)] #no NAs
     spotify$energy[is.na(spotify$energy)] #no NAs
   #checking if values are good:
     summary(spotify$dancebility) #perfect, all values are between 0-1
     summary(spotify$energy) #perfect, all values are between 0-1
   
   
#$key  
  str(spotify$key)
  #i want to see if the numbers are consistent in $key, and staying between 0-11, with no decimals:
  table(spotify$key) #looks good, consistent numbers from 0-11., with no decimals, so :int class is correct
  spotify$key[is.na(spotify$key)] #no NA

   
#$loudness 
  #in $loudness, i want to check if values stay between -60 and 0:
    summary(spotify$loudness) #there is one value that is higher than 0... (0.326) im going to assume that is supposed to be a -0.326
  #checking which value(s) are higher than 0: 
    spotify$loudness[spotify$loudness > 0] #it is only 1 observation, so i will asume that it was a mistake and ist supposed to be a minus 
  #changing this value:
      spotify <- spotify %>%
         mutate('loudness' = gsub(pattern = "0.326", replacement = "-0.326", loudness)) 
      #now $loudness is in character class because i added the "-"... so changing it to numerical
          spotify$loudness <- as.numeric(spotify$loudness)
     spotify$loudness[is.na(spotify$loudness)] #no NA
     
               
#$mode 
    str(spotify$mode) #seeing its chr class
    table(spotify$mode) # $mode seems to be class=chr because "T" after the number which should not be there:
    #to delete the "T":
    spotify <- spotify %>%
      mutate('mode' = gsub(pattern = "T", replacement = "", mode)) 
    #changing mode to int class because it has no decimals
    spotify$mode <-  as.integer(spotify$mode) 
    summary(spotify$mode) #perfect, vales are between  0-1
    spotify$mode[is.na(spotify$mode)]#no NA, perfect
    
    
#$speechiness 
    str(spotify$speechiness) #values with decimals, so class numerical
    summary(spotify$speechiness) #perfect vales are between 0 and 1.0
    spotify$speechiness[is.na(spotify$speechiness)]#no NA
    
    
#$acousticness 
  str(spotify$acousticness) #values with decimals, so :num class is right 
  summary(spotify$acousticness) #perfect, values are bteween 0 and 1
  spotify$acousticness[is.na(spotify$acousticness)] #no NAs perfect
  
  
#$instrumentalness 
  str(spotify$instrumentalness)
  summary(spotify$instrumentalness) #oh, max value is 9.980532 , an dit should not be higher than 1
  spotify$instrumentalness[spotify$instrumentalness > 1] #lets see which values are higher than 1
  #i know the values should not be higher than 1, so i belive its best to delete all values that are higher than 1:
  spotify$instrumentalness[spotify$instrumentalness > 1] <- NA
  #perfect, now, no value is over 1
  table(spotify$instrumentalness) #data looks good
  
  
#$liveness
  str(spotify$liveness) #class num is appropriate 
  summary(spotify$liveness) 
  spotify$liveness[is.na(spotify$liveness)] #no NAs
  table(spotify$liveness)#data seems good


#$valence   
  str(spotify$valence) #seems good
  summary(spotify$valence) #perfect, vales are bewteen 0 and 1
  spotify$valence[is.na(spotify$valence)] #no NA
  
  
#$tempo
  str(spotify$tempo) #seems good
  summary(spotify$tempo)
  spotify$tempo[is.na(spotify$tempo)] #no NAs
  #data semms good

  
#$ duration_ms 
  str(spotify$duration_ms) #seems good
  summary(spotify$duration_ms) #seems good 
  spotify$duration_ms[is.na(spotify$duration_ms)] #no NA
  
  
#$pop, $rap, $rock, $r.b, $edm are not variables
  #checking content of columns:  
        #pop
        table(spotify$pop)
        #rap
        table(spotify$rap)
        #rock
        table(spotify$rock)
        #r.b
        table(spotify$r.b)
        #edm
        table(spotify$edm)
     #summary:   
        #indie poptimism  --> pop
        #southern hip hop  --> rap
        #trap --> rap
        #permanent wave --> rock
        #urban contemporary  --> r.b
        #pop edm --> edm
        #progressive electro house  --> edm
    
  #i want to create a colum called genre:  
    #creating a new colum, that says pop, rap, rock, r.b, edm accordingly
      #creating a fuction i will need later
      removeNA <- function(x) {
        gsub(pattern = "(_NA|NA_)", replacement = "", x) 
      }   
      #creating new column, replacing every subgenre with column name
      spotify <- spotify %>%
        mutate(pop2 = ifelse(test=!is.na(pop), yes= "pop",no= NA)) %>%
        mutate(rap2 = ifelse(!is.na(rap), "rap", NA))%>%
        mutate(rock2 = ifelse(!is.na(rock), "rock", NA))%>%
        mutate(r.b2 = ifelse(!is.na(r.b), "r.b", NA))%>%
        mutate(edm2 = ifelse(!is.na(edm), "edm", NA))
      
      #creating a new column with the genre, by uniting all new columns that i created:
      spotify <- spotify %>%
        unite("genre", c("pop2", "rap2", "rock2", "r.b2", "edm2"), 
              sep= "_", remove = TRUE ) %>%  #TRUE to delete the columns that i am combining
        mutate("genre" = removeNA(genre)) 
        
            #checking new column created
            table(spotify$genre) #perfect! yayyyyy
            spotify$genre[is.na(spotify$genre)] #no NAs
            str(spotify$genre) 
        
            
        
  #creating the new colum "subgenre", by combining the  colums of pop, rap, rock, r.b and edm
     spotify <- spotify %>%
       unite("subgenre", c("pop", "rap", "rock", "r.b", "edm"), 
                    sep= "_", remove = TRUE ) %>%  #TRUE to delete the colums
       mutate("subgenre" = removeNA(subgenre)) 
            
            #checking new colum created
            table(spotify$subgenre) #looks good
            spotify$subgenre[is.na(spotify$subgenre)] #no NA
            str(spotify$subgenre)
            
            
# IDs problem       
    #there is several problems with the IDs:
         #1. i see the same playlist having different playlists ID --> meaning one playlist has several ids 
         #2. the same ID (e.g "0yTlrnue3pJTJd7h7d43mk") is used for two different playlist ID --> Ids are not unique
         #3. the same ID is reused across different columns --> meaning the IDs are therefore not unique
         #4 some playlists don't have id, and as the person will compare playlist, this is a crucial part of the data.
     # concluding : 
          # Id is not unique and not consist
          # problem is not consistent either
          # so right now, all these IDs are useless and confusing
     # my decision: 
         #1. delete all current IDs --> deleting $ track_id, $ track_album_id  
            colnames(spotify)
            spotify <- spotify %>%
              select(track_name, track_artist, track_popularity, track_album_name, 
                     track_album_release_date,playlist_name, playlist_id, 
                     dancebility, energy, key, loudness, mode,
                     speechiness, acousticness, instrumentalness, liveness, 
                     valence, tempo,duration_ms, subgenre, genre) 
                     #i am metioning all the colums i want to keep, excluiding "track_id" and "track_album_id"
                 str(spotify)
          #2. and create my own unique id for each playlist, which replaces the old "playlist_id"
              spotify <- spotify %>%
                group_by(playlist_name) %>%
                mutate(playlist_id = paste0(sample(c(0:9, letters, LETTERS), 6, replace = TRUE), collapse = "")) %>%
                ungroup() #for this part i asked chatgpt for help
              
              
              str(spotify)
              
    write.table(spotify, file = "spotify_tidy_210535530.txt",
                sep = "\t",
                na= "NA",
                col.names = T,
                row.names = F,
                quote = F)
          
    tidy_spotify <- read.table("spotify_tidy_210535530.txt", sep="\t", header=T)
    View(tidy_spotify)


#               :)
                    
      
            
            