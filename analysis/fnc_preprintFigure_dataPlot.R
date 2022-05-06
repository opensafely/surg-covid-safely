# fnc_preprintFigure_dataPlot.R
#
# This script defines a function that plots the submitted dataset  The function
# is called in the script entitled "preprintFigure.R".
#

fnc_preprintFigure_dataPlot <- function(data, cohort)
{
  if (cohort == "with") {cancer <- "cancer"}else{cancer <- "noCancer"}
  # Plot showing weekly summary of the proportion of surgeries conducted more
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p <-
    data %>%
    ggplot(
      aes(x = interaction(Week_surgery, Month_surgery, Year_surgery),
          y = weekly_7wkPreOpInfection)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 1.2),
                       labels = c(0, .25, .5, .75, 1),
                       breaks = c(0, .25, .5, .75, 1)) +
    theme(panel.grid.major.x = element_blank()) +
    ggtitle(paste0("Weekly summary of the proportion of surgeries ",
                   "conducted more than 7 weeks from a positive PCR test ",
                   "for SARS-CoV-2"),
            subtitle = paste0("Cohort = Patients ", cohort,
                              " a cancer diagnosis within 6 months of surgery")) +
    ylab("Proportion of surgeries conducted more\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 54) + # Index for March 2020
    annotate(x = 54, y = +Inf, label = "1st COVIDSurg\npublication", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 56) + # Index for March 2020
    annotate(x = 56, y = +Inf, label = "NICE\nguidelines", vjust = 1, hjust = "left", geom = "label") +
    geom_vline(xintercept = 98) + # Index for January 2021
    annotate(x = 98, y = +Inf, label = "Estimated\ndate of vaccine\neffectiveness", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 106) + # Index for March 2021
    annotate(x = 106, y = +Inf, label = "2nd COVIDSurg\npublication", vjust = 1, hjust = "left", geom = "label")
  ggsave(filename = 
           paste0(cancer,"_",
                  "weekly_summary_proportion_surgeries_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 30, height = 20, units = "cm")
  
  # Plot showing monthly summary of the proportion of surgeries conducted more
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p<-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(monthly_7wkPreOpInfection = median(monthly_7wkPreOpInfection)) %>%
    select(Year_surgery, Month_surgery, monthly_7wkPreOpInfection) %>%
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = monthly_7wkPreOpInfection)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 1.15),
                       labels = c(0, .25, .5, .75, 1),
                       breaks = c(0, .25, .5, .75, 1)) +
    ggtitle(paste0("Monthly summary of the proportion of surgeries ",
                   "conducted more than 7 weeks from a positive PCR test ",
                   "for SARS-CoV-2"),
            subtitle = paste0("Cohort = Patients ", cohort,
                              " a cancer diagnosis within 6 months of surgery")) +
    ylab("Proportion of surgeries conducted more\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = +Inf, label = "1st COVIDSurg\npublication", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = +Inf, label = "Estimated\ndate of vaccine\neffectiveness", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = +Inf, label = "2nd COVIDSurg\npublication", vjust = 1, hjust = "left", geom = "label")
  ggsave(filename = 
           paste0(cancer,"_",
                  "1monthly_summary_proportion_surgeries_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 30, height = 20, units = "cm")
  
  # Plot showing 2-monthly summary of the proportion of surgeries conducted more
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p<-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(twoMonthly_7wkPreOpInfection = median(twoMonthly_7wkPreOpInfection)) %>%
    select(Year_surgery, Month_surgery, twoMonthly_7wkPreOpInfection) %>%
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = twoMonthly_7wkPreOpInfection)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 1.15),
                       labels = c(0, .25, .5, .75, 1),
                       breaks = c(0, .25, .5, .75, 1)) +
    ggtitle(paste0("Rolling two-monthly summary of the proportion of surgeries ",
                   "conducted more than 7 weeks from a positive PCR test ",
                   "for SARS-CoV-2"),
            subtitle = paste0("Cohort = Patients ", cohort,
                              " a cancer diagnosis within 6 months of surgery")) +
    ylab("Proportion of surgeries conducted more\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = +Inf, label = "1st COVIDSurg\npublication", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = +Inf, label = "Estimated\ndate of vaccine\neffectiveness", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = +Inf, label = "2nd COVIDSurg\npublication", vjust = 1, hjust = "left", geom = "label")
  ggsave(filename = 
           paste0(cancer,"_",
                  "2monthly_summary_proportion_surgeries_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 30, height = 20, units = "cm")
  
  # Plot showing 3-monthly summary of the proportion of surgeries conducted more
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p<-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(threeMonthly_7wkPreOpInfection = median(threeMonthly_7wkPreOpInfection)) %>%
    select(Year_surgery, Month_surgery, threeMonthly_7wkPreOpInfection) %>%
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = threeMonthly_7wkPreOpInfection)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 1.15),
                       labels = c(0, .25, .5, .75, 1),
                       breaks = c(0, .25, .5, .75, 1)) +
    ggtitle(paste0("Rolling three-monthly summary of the proportion of surgeries ",
                   "conducted more than 7 weeks from a positive PCR test ",
                   "for SARS-CoV-2"),
            subtitle = paste0("Cohort = Patients ", cohort,
                              " a cancer diagnosis within 6 months of surgery")) +
    ylab("Proportion of surgeries conducted more\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = +Inf, label = "1st COVIDSurg\npublication", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = +Inf, label = "Estimated\ndate of vaccine\neffectiveness", vjust = 1, hjust = "right", geom = "label") +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = +Inf, label = "2nd COVIDSurg\npublication", vjust = 1, hjust = "left", geom = "label")
  ggsave(filename = 
           paste0(cancer,"_",
                  "3monthly_summary_proportion_surgeries_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 30, height = 20, units = "cm")
}