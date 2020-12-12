# website - imdb.com

library(rvest)
library(data.table)

# Task 1: when I change car name to Opel, it should get data from that page
# https://joautok.hu/hasznaltauto/alfa-romeo

url <- 'https://www.imdb.com/search/name/?gender=male,female&start=1&ref_=rlm'
# url <- 'https://www.imdb.com/search/name/?gender=male,female&start='

test_urls <- c()

for(x in seq(1,5973906,50)) {
  test_urls <- c(test_urls, paste0(url, x, '&ref_=rlm')) 
}


# for (x in 1:10) {
#   print(test_urls[x])
# }
# test_urls[2]

file_html <- read_html(url)
write_html(file_html, 'html_file.html')

actor_names <- 
  file_html %>% 
  html_nodes('.lister-item-header a') %>% 
  html_text(trim = T)


partial_links <- 
  file_html %>% 
  html_nodes('.lister-item-header a') %>% 
  html_attr('href')


actor_links <- c()

for(x in 1:length(partial_links)) {
  actor_links <- c(actor_links, paste0('https://www.imdb.com/name', partial_links[x])) 
}

actor_links  
  

# datePublished, budget, rating (ratingValue inside aggregateRating)
# "duration": "PT2H22M",
# actors, director, keywords, 

url <- 'https://www.imdb.com/chart/top'
file_html <- read_html(url)

write_html(file_html, 'html_file.html')

movie_names <- 
  file_html %>%  
  html_nodes('.titleColumn a') %>% 
  html_text(trim = T)


movie_names

url <- 'https://www.autotrader.com/cars-for-sale/bmw?channel=ATC&relevanceConfig=default&searchRadius=0&marketExtension=include&isNewSearch=true&showAccelerateBanner=false&sortBy=relevance&numRecords=25'
file_html <- read_html(url)

cars <- 
  file_html %>%  
  html_nodes('.item-card') %>% 
  html_nodes('a')

cars <- 
  file_html %>%  
  html_nodes('.positioned-overlay-base') %>% 
  html_nodes('a') %>% 
  html_attr('href')


cars

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