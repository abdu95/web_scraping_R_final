#' Create a function which requires two arguments. 
#' First a keyword then a number of pages to download.
library(readr)

get_vox_pages <- function(keyword, no_pages){
  main_url <- 'https://www.vox.com/search?page='

  # generate the URLs
  links <- paste0(main_url, 1:no_pages, '&q=', keyword)
  return_df <- rbindlist(lapply(links, get_one_page_from_vox))
  
  write_csv(return_df, 'data/result.csv')
  saveRDS(return_df, file = 'data/rdsfile.rds')
  
  return(return_df)
}

# check the function
returned_df <- get_vox_pages(keyword = 'Biden', 6)
