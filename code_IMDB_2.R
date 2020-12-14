library(stringr)

# movie with the highest budget
# movie with the lowest budget



# a function which downloads information from the website to dataframe
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
  
  df_b_r <-  data.frame(movie_title, revenue)
  return(df_b_r)
}

some_link <- links[1]
movie_file <- read_html(some_link)

list_of_revenue <- sapply(df$link, get_revenue_budget)

list_of_revenue <- sapply(df$link, get_revenue_budget)

# budget <- 
#   movie_file %>% 
#   html_nodes('.txt-block:nth-child(12)') %>% 
#   html_text(trim = T)

text_blocks <- 
  movie_file %>% 
  html_nodes('.txt-block') %>% 
  html_text(trim = T)

search_budget <- "Budget"
show_b_position <- match(1, str_detect(text_blocks, search_budget))
budget_p <- text_blocks[show_b_position]
budget_s <- gsub("\\:*","",
               gsub("\\(.*$", "", budget_p))

budget <- trimws(sub('.*Budget\\$', '', budget_s))

# budget <- as.numeric(gsub(",","",substr(budget_p, 30, nchar(budget_p))))


some_links <- df[1:5,6] 
another_list <- lapply(some_links, get_revenue_budget)
unlisted <- rbindlist(another_list)
