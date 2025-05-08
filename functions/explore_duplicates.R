
explore_duplicates <- function(df,
                               x = "",
                               y = ""){
  df1 <- df %>% 
    select(matches(paste0("^",x,"$"),ignore.case = FALSE), # ensures exact matching
           matches(paste0("^",y,"$"),ignore.case = FALSE)) %>% 
    distinct() %>% 
    arrange(.data[[x]]) 
  
  df2 <- df1 %>% 
    group_by(.data[[x]]) %>% 
    summarise(n_duplicates = n()-1) %>% 
    arrange(-n_duplicates)
  
  retlist <- list(unique_combinations = df1,# this table shows the unique combinations of x and y
                  number_of_duplicates = df2) 
  # the second table shows the number of duplicates of y for every unique values of x
  # n_duplicates is 0 for any value of x that has only one unique value of y
  # n_duplicates is >0 for any value of x that has >1 unique value.
  return(retlist)
}
