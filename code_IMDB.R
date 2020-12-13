library(rvest)
library(data.table)
library(jsonlite)
library(ggplot2)
library(knitr)

url <- 'https://www.imdb.com/chart/top'
file_html <- read_html(url)
write_html(file_html, 'html_file.html')

boxes <- file_html %>% 
  html_nodes('.titleColumn a')

movie_names <- html_text(boxes)

ratings <- as.numeric(
  file_html %>%  
  html_nodes('strong') %>% 
  html_text())

years_p <- 
  file_html %>%  
  html_nodes('.secondaryInfo') %>% 
  html_text()

years <- as.numeric(sub("\\).*", "", sub(".*\\(", "", years))) 

links_p <- 
  boxes %>% 
  html_attr('href')

links_c <- gsub("\\?.*", "", links_p)

links <- c()

for(x in links_c) {
  links <- c(links, paste0('https://www.imdb.com', x)) 
}

actors <- 
  boxes %>% 
  html_attr('title')

votes_p <- file_html %>% 
  html_nodes('.imdbRating') %>% 
  html_node('strong') %>% 
  html_attr('title')

votes <- c()
for (x in votes_p) {
  votes <- c(votes, substr(x, 13, 4))
}

for (x in votes_p) {
  votes <- c(votes, as.numeric(gsub(',','',
                         gsub(' user ratings','',
                              gsub('.*?based on ','',
                                   x)))))
}


# to create a dataframe
df <- data.frame('movie_title' = movie_names, 
                 'year' = years, 
                 'rating' = ratings,
                 'votes' = votes,
                 'cast' = actors, 
                 'link' = links)  


# First EDA: years

# this statistics shows that
# the oldest movie (min) is taken in 1921, and youngest is in 2020 (max)
kable(as.array(summary(years)), caption = "Summary statistics for years movie released")


uniques <- unique(years)

qplot(years, 
      geom="histogram", 
      binwidth = 1,
      xlim=c(1921,2020)) 



# Second EDA: ratings
kable(as.array(summary(ratings)), caption = "Summary statistics of ratings")

# Movie with minimum rating: 8
# Movie with maximum rating: 9.2


hist(
  ratings,
  col = 'skyblue3')


# Third EDA: votes
kable(as.array(summary(votes)), caption = "Summary statistics of votes")


hist(
  votes,
  col = 'skyblue3')
