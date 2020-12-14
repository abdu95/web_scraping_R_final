library(stringr)
library(dplyr)
# movie with the highest budget
# movie with the lowest budget


# a function which downloads information from the website to dataframe
get_movies <- function(arg_url) {
  
  # to get a web page, pass URL to read_html 
  url_current <- arg_url
  movie_file <- read_html(url_current)

  json_data <-
    fromJSON(movie_file %>%
               html_nodes(xpath = "//script[@type='application/ld+json']") %>%
               html_text())
  
  movie_title <- json_data$name
  # genre <- json_data$genre
  
  genre <- 
    movie_file %>% 
    html_nodes('.txt-block~ .canwrap') %>% 
    html_text()
  
  genre_s <- sub('.*Genres\\:', '', genre)
  
  
  genre <-  gsub("\\s", "",
                 gsub(' ', '',
                      gsub('[|]', ',',
                           str_remove_all(genre_s, "\n"))))
  
  text_blocks <- 
    movie_file %>% 
    html_nodes('.txt-block') %>% 
    html_text(trim = T)
  
  search_gross <- "Cumulative"
  show_r_position <- match(1, str_detect(text_blocks, search_gross))
  revenue_p <- text_blocks[show_r_position]
  revenue <- as.numeric(gsub(",","",substr(revenue_p, 30, nchar(revenue_p))))
  
  search_budget <- "Budget"
  show_b_position <- match(1, str_detect(text_blocks, search_budget))
  budget_p <- text_blocks[show_b_position]
  budget_s <- gsub("\\:*","",
                   gsub("\\(.*$", "", budget_p))
  
  budget <- as.numeric(gsub(',','', trimws(sub('.*Budget\\$', '', budget_s))))
  
  
  df_b_r <-  data.frame(movie_title, genre, revenue, budget)
  
  return(df_b_r)
}

some_link <- links[1]
movie_file <- read_html(some_link)


get_revenue_budget <- function(arg_url) {
  
  # to get a web page, pass URL to read_html 
  url_current <- arg_url
  movie_file <- read_html(url_current)
  
  json_data <-
    fromJSON(movie_file %>%
               html_nodes(xpath = "//script[@type='application/ld+json']") %>%
               html_text())
  
  movie_title <- json_data$name

  text_blocks <- 
    movie_file %>% 
    html_nodes('.txt-block') %>% 
    html_text(trim = T)
  
  search_gross <- "Cumulative"
  show_r_position <- match(1, str_detect(text_blocks, search_gross))
  revenue_p <- text_blocks[show_r_position]
  revenue <- as.numeric(gsub(",","",substr(revenue_p, 30, nchar(revenue_p))))
  
  search_budget <- "Budget"
  show_b_position <- match(1, str_detect(text_blocks, search_budget))
  budget_p <- text_blocks[show_b_position]
  budget_s <- gsub("\\:*","",
                   gsub("\\(.*$", "", budget_p))
  
  budget <- as.numeric(gsub(',','', trimws(sub('.*Budget\\$', '', budget_s))))
  
  
  df_b_r <-  data.frame(movie_title, revenue, budget)
  
  return(df_b_r)
}


some_links <- df[1:5,6] 
# list_of_revenue <- sapply(df$link, get_revenue_budget)

# movies_list <- lapply(some_links, get_revenue_budget)

movies_list <- lapply(df$link, get_revenue_budget)
movies_df <- rbindlist(movies_list)

final_df <- movies_df[complete.cases(movies_df),]
final_df$revenue_scale <- final_df$revenue/1000
ggplot(final_df, aes(x=budget, y=revenue)) + geom_point(color="blue") +
  labs(y="Revenue", x = "Budget") +
  ggtitle("Budget and revenue of movie") + 
  geom_smooth(method=lm)

top_movies <- final_df %>% slice_max(revenue, n = 10)

ggplot(top_movies, aes(x=movie_title, y=revenue)) + geom_point(color="blue") +
  labs(y="Revenue", x = "Movie title") +
  ggtitle("Movies with highest revenue") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  NULL

