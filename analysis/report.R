library('tidyverse')
library('lubridate')

# Read data.
df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(SARS_CoV_2_test_type = col_factor(),
                   SARS_CoV_2_symptomatic = col_factor(),
                   Sex = col_factor(),
                   patient_id = col_integer())
  ) %>% 
  mutate(Sex=fct_relevel(Sex, c("Female", "Male")),
         SARS_CoV_2_test_type = fct_relevel(SARS_CoV_2_test_type,
                                            c("LFT_Only", "PCR_Only", "LFT_WithPCR")
                                            ),
         SARS_CoV_2_symptomatic = fct_relevel(SARS_CoV_2_symptomatic,
                                              c("", "N", "Y")
                                              )
          ) #%>%
  # mutate(date_surgery = lubridate::dmy(date_surgery),
  #        date_test_SARS_CoV_2_outcome_any = lubridate::dmy(date_test_SARS_CoV_2_outcome_any),
  #        date_test_SARS_CoV_2_outcome_positive = lubridate::dmy(date_test_SARS_CoV_2_outcome_positive),
  #        date_test_SARS_CoV_2_outcome_negative = lubridate::dmy(date_test_SARS_CoV_2_outcome_negative),
  #        date_death_ons = lubridate::dmy(date_death_ons),
  #        date_death_cpns = lubridate::dmy(date_death_cpns),
  #        date_COVID_first_vaccination = lubridate::dmy(date_COVID_first_vaccination)
  #       )
myData <- df_input

# Define required variables.
myData <- myData %>%
            ## Distinction pre and post COVID.
            mutate(
              surgery_pre_or_post_COVID_UK = case_when(
                .$date_surgery <= "2020-03-17" ~ "preCOVID",
                .$date_surgery > "2020-03-17" ~ "postCOVID",
                is.na(.$date_surgery) ~ NA_character_
                                              )
                  ) %>%
            ## Date of death.
            mutate(
              date_death = case_when(
                is.na(.$date_death_ons) & !is.na(.$date_death_cpns) ~ .$date_death_cpns,
                is.na(.$date_death_cpns) & !is.na(.$date_death_ons) ~ .$date_death_ons,
                is.na(.$date_death_ons) & is.na(.$date_death_cpns) ~ NA_Date_
              )
            )
myData <- myData %>%
            ## Indicator for 30-day post-operative mortality.
           mutate(
             postOp_mortality_30day = case_when(
              (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 30 ~ "Dead within 30-day post-operation",
                (.$date_death - .$date_surgery) > 30 ~ "Alive within 30-day post-operation",
              is.na(.$date_death) ~ "No death recorded",
              is.na(.$date_surgery) ~ "No surgery recorded"
              )
           ) %>%
            ## Month of surgery.
           mutate(Month_surgery = lubridate::month(myData$date_surgery)) %>%
            ## Year of surgery.
           mutate(Year_surgery = lubridate::year(myData$date_surgery)) %>%
            ## Indicator for pre-surgery COVID vaccination. 
           mutate(
            preSurgery_vaccinated = case_when(
              (.$date_COVID_first_vaccination < .$date_surgery) ~ "PreSurgery vaccination",
              (.$date_COVID_first_vaccination > .$date_surgery) ~ "PostSurgery vaccination",
              is.na(.$date_COVID_first_vaccination) ~ "No vaccination recorded"
             )
           )


# Collect plot data.
myPlotData <- myData %>%
    group_by(Year_surgery, Month_surgery) %>%
      summarise(n_tdpo = n(),
                count_tdpo = sum(ifelse(postOp_mortality_30day=="Dead within 30-day post-operation",1,0)),
                prop_tdpo = (count_tdpo / n_tdpo))

# Make basic plot.
plot_postOp_mortality_30day <- ggplot(myPlotData[1:12,], aes(x=Month_surgery, y=n_tdpo, group=Year_surgery)) + geom_line()
# Save plot
ggsave(
  plot = plot_postOp_mortality_30day,
  filename="plot_postOp_mortality_30day.png",
  path=here::here("output"),
)