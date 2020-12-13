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





# a function which downloads information from the website to dataframe
get_revenue_budget <- function(arg_url) {
  
  # save it as html document
  # write_html(file_html, 'html_file.html')
  
  # headings <- 
  #   file_html %>% 
  #   html_nodes('.c-entry-box--compact__title') %>% 
  #   html_text()
  # 
  # post_links <- 
  #   file_html %>% 
  #   html_nodes('.c-entry-box--compact__title') %>% 
  #   html_nodes('a') %>%
  #   html_attr('href')
  # 
  # posted_date <- 
  #   file_html %>% 
  #   html_nodes('time') %>% 
  #   html_text(trim = TRUE)
  # 
  # author <- 
  #   file_html %>% 
  #   html_nodes('.c-byline__item') %>% 
  #   html_text(trim = TRUE) 
  # 
  # authors <- c()
  # 
  # for(x in seq(1,length(author),3)) {
  #   authors <- c(authors, author[x]) 
  # }
  
  # to get a web page, pass URL to read_html 
  url_current <- arg_url
  movie_file <- read_html(url_current)
  
  # df_json <- json_data <- 
  #   fromJSON(movie_file %>%
  #              html_nodes(xpath = "//script[@type='application/ld+json']") %>%
  #              html_text())
  
  # df_json$duration
  # name_of_movie <- df_json$name 
  
  revenue_p <- 
    movie_file %>%
    html_nodes('.txt-block:nth-child(15)') %>% 
    html_text(trim = T)
  
  revenue <- as.numeric(gsub(",","",substr(revenue_p, 30, 40)))
  
  # budget_p <- 
  #   movie_file %>%
  #   html_nodes('#titleDetails .txt-block:nth-child(12)') %>% 
  #   html_text(trim = T)
  # 
  # budget <- as.numeric(gsub(",","",substr(budget_p, 9, 18)))
  
  # log(budget)
  
  # is there a connection between the revenue and the budget.

  
  # to create a dataframe
  # df_b_r <- data.frame('budget' = budget, 'revenue' = revenue)  
  # df_b_r <- data.frame('name' = name_of_movie)  
  # df_b_r <- data.frame('revenue' = revenue)    
  
  return(revenue)
}

#apply a function to a list
# df_list <- lapply(df$link, get_revenue_budget)
df_list <- sapply(df$link, get_revenue_budget)

# warning: 5: In FUN(X[[i]], ...) : NAs introduced by coercion



# test the function
test_df <- get_one_page_from_vox('https://www.vox.com/search?page=1&q=Biden')

# generate the URLs
test_urls <- paste0('https://www.vox.com/search?page=', 1:5, '&q=Biden' )

#apply a function to a list
df_list <- lapply(test_urls, get_one_page_from_vox)

#create a data.table object of the list of data.frames
df <- rbindlist(df_list)