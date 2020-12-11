# news website - vox.com

library(rvest)
library(data.table)
# Task 1: when I change car name to Opel, it should get data from that page
# https://joautok.hu/hasznaltauto/alfa-romeo
.lister-item-header a
# url <- 'https://joautok.hu/hasznaltauto/alfa-romeo?page=3'
# file_html <- read_html(url)
# write_html(file_html, 'html_file.html')

require(httr)

cookies = c(
  'bm2uu' = '0_0_1607656361_25_1_0_(null)~*',
  'ibbid' = 'BBID-01-02823703640242470-16154172'
)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:83.0) Gecko/20100101 Firefox/83.0',
  `Accept` = '*/*',
  `Accept-Language` = 'en,ru-RU;q=0.8,ru;q=0.5,en-US;q=0.3',
  `Origin` = 'https://joautok.hu',
  `Connection` = 'keep-alive',
  `Referer` = 'https://joautok.hu/hasznaltauto/alfa-romeo?page=3'
)

params = list(
  `typkodu` = 'js',
  `_json` = '1',
  `bust` = '0.8270581446191854',
  `uhe` = '864',
  `uwi` = '1536',
  `href` = 'https://joautok.hu/hasznaltauto/alfa-romeo?page=3',
  `one2n` = '2',
  `one2n1` = '/26892/5/1/1/;26892.5.1.1;',
  `one2n2` = '/26892/5/2/1/;26892.5.2.1;'
)

res <- httr::GET(url = 'https://go.cz.bbelements.com/please/showit/0/0/0/1/', httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies))

#NB. Original query string below. It seems impossible to parse and
#reproduce query strings 100% accurately so the one below is given
#in case the reproduced version is not "correct".
# res <- httr::GET(url = 'https://go.cz.bbelements.com/please/showit/0/0/0/1/?typkodu=js&_json=1&bust=0.8270581446191854&uhe=864&uwi=1536&href=https%3A%2F%2Fjoautok.hu%2Fhasznaltauto%2Falfa-romeo%3Fpage%3D3&one2n=2&one2n1=/26892/5/1/1/;26892.5.1.1;&one2n2=/26892/5/2/1/;26892.5.2.1;&', httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))



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