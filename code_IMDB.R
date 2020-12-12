# website - imdb.com

library(rvest)
library(data.table)
library(jsonlite)
library(ggplot2)

url <- 'https://www.imdb.com/chart/top'
file_html <- read_html(url)

write_html(file_html, 'html_file.html')

movie_names <- 
  file_html %>%  
  html_nodes('.titleColumn a') %>% 
  html_text()

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
  file_html %>%  
  html_nodes('.titleColumn a') %>% 
  html_attr('href')

links_c <- gsub("\\?.*", "", links_p)

links <- c()

for(x in links_c) {
  links <- c(links, paste0('https://www.imdb.com', x)) 
}

# to create a dataframe
df <- data.frame('movie_title' = movie_names, 'rating' = ratings, 'date_released' = years, 'link' = links)  

# First EDA:
summary(ratings)

# Movie with minimum rating: 8
# Movie with maximum rating: 9.2



url_demo <- links[1]
movie_file <- read_html(url_demo)

json_data <- 
  fromJSON(movie_file %>%
      html_nodes(xpath = "//script[@type='application/ld+json']") %>%
      html_text())

revenue_p <- 
  movie_file %>%
  html_nodes('.txt-block:nth-child(15)') %>% 
  html_text(trim = T)
  
revenue <- as.numeric(gsub(",","",substr(revenue_p, 30, 40)))

budget_p <- 
  movie_file %>%
  html_nodes('#titleDetails .txt-block:nth-child(12)') %>% 
  html_text(trim = T)

budget <- as.numeric(gsub(",","",substr(budget_p, 9, 18)))
log(budget)
budget
# is there a connection between the revenue and the budget.

qplot(years, 
      geom="histogram", 
      binwidth = 1,
      xlim=c(1921,2020)) 
years

# Second EDA:
summary(years)
# this statistics shows that
# the oldest movie (min) is taken in 1921, and youngest is in 2020 (max)

most_appeared_years <- table(years)
# most of the movies (8) released in 1995 

# movie with the highest budget
# movie with the lowest budget

for (x in years) {
  print(x)
}

uniques <- unique(years)





# Task 1: when I change car name to Opel, it should get data from that page
# https://joautok.hu/hasznaltauto/alfa-romeo


# datePublished, budget, rating (ratingValue inside aggregateRating)
# "duration": "PT2H22M",
# actors, director, keywords, 




#apply a function to a list
df_list <- lapply(test_urls, get_one_page_from_vox)


# a function which downloads information from the website to dataframe
get_one_page_from_vox <- function(arg_url) {
  
  # to get a web page, pass URL to read_html 
  file_html <- read_html(arg_url)
  
  # save it as html document
  write_html(file_html, 'html_file.html')
  
  headings <- 
    file_html %>% 
    html_nodes('.c-entry-box--compact__title') %>% 
    html_text()
  
  post_links <- 
    file_html %>% 
    html_nodes('.c-entry-box--compact__title') %>% 
    html_nodes('a') %>%
    html_attr('href')
  
  posted_date <- 
    file_html %>% 
    html_nodes('time') %>% 
    html_text(trim = TRUE)
  
  author <- 
    file_html %>% 
    html_nodes('.c-byline__item') %>% 
    html_text(trim = TRUE) 
  
  authors <- c()
  
  for(x in seq(1,length(author),3)) {
    authors <- c(authors, author[x]) 
  }
  
  # to create a dataframe
  df <- data.frame('headings' = headings, 'post_link' = post_links, 'date' = posted_date, 'author' = authors)  
  
  return(df)
}

# test the function
test_df <- get_one_page_from_vox('https://www.vox.com/search?page=1&q=Biden')

# generate the URLs
test_urls <- paste0('https://www.vox.com/search?page=', 1:5, '&q=Biden' )

#apply a function to a list
df_list <- lapply(test_urls, get_one_page_from_vox)

#create a data.table object of the list of data.frames
df <- rbindlist(df_list)