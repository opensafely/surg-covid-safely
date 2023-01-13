## If running on OpenSAFELY.
library("plyr")
library('tidyverse')
library('lubridate')
library("kableExtra")
library("here")
library("e1071")
## If ever running locally.
# list_of_packages <- c("tidyverse", "lubridate", "kableExtra","here", "e1071")
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# for (i in 1:length(list_of_packages))
# {
#   library(list_of_packages[i],character.only = T)
# }

#############################
# Load requisite functions. #
#############################

source(here::here("analysis", "dataset_preparation.R"))
myDataSelect <- myData %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded"),
                preOperative_infection_status!=
                  "Error: Test result after surgery. Check study_definition.",
                era != "No surgery recorded",
                era != "No surgery date recorded",
                COVIDSurg_data_collection_period != "No surgery date recorded")
data_to_use_all <- myDataSelect %>% dplyr::filter(has_surgery == TRUE)
data_age <-
  data_to_use_all %>%
  dplyr::select(age_at_surgery, era) %>%
  bind_rows(
    data_to_use_all %>%
      dplyr::select(age_at_surgery, COVIDSurg_data_collection_period) %>%
      dplyr::filter(COVIDSurg_data_collection_period ==
                      'COVIDSurg data collection period') %>%
      `colnames<-`(c('age_at_surgery', 'era'))
  )

########################################################
# Compute basic summary statistics of age distribution #
########################################################
tbl_age_distribution <-
  data_age %>%
  dplyr::group_by(era) %>%
  summarize(mean = mean(age_at_surgery, na.rm = TRUE),
            sd = sd(age_at_surgery, na.rm = TRUE),
            skew = e1071::skewness(age_at_surgery, na.rm = TRUE),
            kurt = e1071::kurtosis(age_at_surgery, na.rm = TRUE)) 

eras_of_interest <- c("Pre-pandemic", "Pandemic no vaccine",
                     "Pandemic with vaccine", "COVIDSurg data collection period")
plot_age_distribution <-
data_age %>%
  droplevels() %>%
  mutate(era = 
           factor(era, levels = c("Pre-pandemic", "Pandemic no vaccine",
                           "Pandemic with vaccine",
                           "COVIDSurg data collection period"))
         ) %>%
  ggplot(aes(x = age_at_surgery)) +
  geom_histogram(colour = 1, bins = 10) +
  xlab("Age at surgery") +
  ylab("Count of patients") +
  theme(axis.title = element_text(size = 12),
        strip.text = element_text(size = 6)) + 
  facet_wrap(vars(era))
  
#######################
# Write output to file #
#######################
write.csv(
  x = tbl_age_distribution,
  file = here::here("output",paste0("tbl_age_distribution.csv"))
)
ggsave(filename = "plot_age_distribution.jpeg",
       plot = plot_age_distribution,
       device = "jpeg",
       path = here::here("output"),
       width = 3.5, height = 3.25, units = "in",
       dpi = 600)
