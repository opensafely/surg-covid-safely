# fnc_make7wkTable.R
#
# This script defines a function that makes a table with 7week threshold
# instead of interim intervals. The function  is called in the script entitled
# "Make_Table1.R".
#

fnc_make7wkTable <-
  function(inputTable)
{
    output <- 
      inputTable %>%
      dplyr::select(tidyselect::all_of(n_intervals_less_than_7wks)) %>%
      data.matrix() %>% rowSums() %>% as.data.frame() %>% 
      dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
      tibble::add_column(inputTable %>%
                           dplyr::select(-tidyselect::any_of(n_intervals_less_than_7wks),
                                         -tidyselect::any_of(pct_intervals_less_than_7wks),
                                         -c(n_infection_7wk, pct_infection_7wk)),
                         .before = ".") %>%
      tibble::add_column(inputTable %>%
                           dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
      dplyr::rename(`n_infection_<7wk` = ".") %>%
      tidyr::replace_na(list("pct_infection_<7wks" = 0))
  
  return(output)

}