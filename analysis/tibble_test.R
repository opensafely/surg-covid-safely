list_of_packages <- c("tidyverse", "here")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
for (i in 1:length(list_of_packages))
{
  library(list_of_packages[i],character.only = T)
}

cols_of_interest <- c("count_1", "count_2", "count_3", "count_4", "count_5", "count_6");

myTibble <-
  data.frame(period = rep(c("first", "second", "third"), each = 2),
             answer = rep(c("Yes", "No"), by = 2), count_1 = rbinom(6, 100, 0.1),
             count_2 = rbinom(6, 100, 0.1), count_3 = rbinom(6, 100, 0.5),
             count_4 = rbinom(6, 100, 0.1), count_5 = rbinom(6, 100, 0.1),
             count_6 = rbinom(6, 100, 0.1)) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(., (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));
#myTibble[,cols_of_interest] <-
  #round(myTibble[,cols_of_interest] / 5) * 5

myTibble

write.csv(
  x = myTibble,
  file = here::here("output",
                    "myTibble.csv")
)