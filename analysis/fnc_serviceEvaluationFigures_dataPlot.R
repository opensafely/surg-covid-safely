# fnc_preprintFigure_dataPlot.R
#
# This script defines a function that plots the submitted dataset  The function
# is called in the script entitled "preprintFigure.R".
#

fnc_serviceEvaluationFigures_dataPlot <- function(data, cancer = c("with", "without"), window, figureCaption = F)
{
  if(!cancer %in% c("with", "without")){stop("Argument <cancer> must be with 'with' or 'without'")}
  if (cancer == "with") {fileprefix1 <- "cancer"}else{fileprefix1 <- "noCancer"}
  if (window == "")
  {
    fileprefix2 <- "_"
    subtitleSuffix <- ""
  } else {
      fileprefix2 <- paste0("_",window,"months_")
      subtitleSuffix <- paste0(" within ", window, " months of\nsurgery")
  }
  
  # Plot showing weekly summary of the proportion of surgeries conducted less
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p <-
    data %>%
    ggplot(
      aes(x = interaction(Week_surgery, Month_surgery, Year_surgery),
          y = weekly_prop_within_7wk)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 100),
                       labels = c(0, 25, 50, 75, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
    theme(panel.grid.major.x = element_blank()) +
    ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 54) + # Index for March 2020
    annotate(x = 54, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 56) + # Index for March 2020
    annotate(x = 56, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 98) + # Index for January 2021
    annotate(x = 98, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 106) + # Index for March 2021
    annotate(x = 106, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  
  if(figureCaption == T)
  {
  p <- p +
    ggtitle(paste0("Rolling weekly summary of the proportion of\n",
                   "surgeries conducted less than 7 weeks from a positive\n",
                   "PCR test for SARS-CoV-2"),
            subtitle = paste0("(Cohort = Patients ", cancer,
                              " a cancer diagnosis", subtitleSuffix, ")")) +
    theme(plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 7),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.line = element_line(colour = "black"))
  } else {
    p <- p +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"),
            plot.margin = margin(t = 20)) 
  }
  
  ggsave(filename = 
           paste0(fileprefix1,fileprefix2,"_",
                  "weekly_summary_proportion_surgeries_within_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 3.5, height = 3.25, units = "in")
  
  # Plot showing monthly summary of the proportion of surgeries conducted less
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p <-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(monthly_prop_within_7wk = median(monthly_prop_within_7wk)) %>%
    select(Year_surgery, Month_surgery, monthly_prop_within_7wk) %>%
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = monthly_prop_within_7wk)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 100),
                       labels = c(0, 25, 50, 75, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
    ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
    annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  
  if(figureCaption == T)
  {
    p <- p +
      ggtitle(paste0("Rolling monthly summary of the proportion of\n",
                     "surgeries conducted less than 7 weeks from a positive\n",
                     "PCR test for SARS-CoV-2"),
              subtitle = paste0("(Cohort = Patients ", cancer,
                                " a cancer diagnosis ", window, ")")) +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  } else {
    p <- p +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black")) 
  }
  
  ggsave(filename = 
           paste0(fileprefix1,fileprefix2,"_",
                  "1monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 3.5, height = 3.25, units = "in")
  
  # Plot showing 2-monthly summary of the proportion of surgeries conducted less
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p <-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(twoMonthly_prop_within_7wk = median(twoMonthly_prop_within_7wk)) %>%
    select(Year_surgery, Month_surgery, twoMonthly_prop_within_7wk) %>%
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = twoMonthly_prop_within_7wk)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 100),
                       labels = c(0, 25, 50, 75, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
    ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
    annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  
  if(figureCaption == T)
  {
    p <- p +
      ggtitle(paste0("Rolling two-monthly summary of the proportion of\n",
                     "surgeries conducted less than 7 weeks from a positive\n",
                     "PCR test for SARS-CoV-2"),
              subtitle = paste0("(Cohort = Patients ", cancer,
                                " a cancer diagnosis ", window, ")")) +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  } else {
    p <- p +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black")) 
  }
  
  ggsave(filename = 
           paste0(fileprefix1,fileprefix2,"_",
                  "2monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 3.5, height = 3.25, units = "in")
  
  # Plot showing 3-monthly summary of the proportion of surgeries conducted less
  # than 7 weeks from a positive PCR test for SARS-CoV-2.
  p <-
    data %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(threeMonthly_prop_within_7wk = median(threeMonthly_prop_within_7wk)) %>%
    select(Year_surgery, Month_surgery, threeMonthly_prop_within_7wk) %>% 
    ggplot(.,
           aes(x = interaction(Month_surgery, Year_surgery),
               y = threeMonthly_prop_within_7wk)) +
    geom_col() +
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, 100),
                       labels = c(0, 25, 50, 75, 100),
                       breaks = c(0, 25, 50, 75, 100)) +
    ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
    annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  
  if(figureCaption == T)
  {
    p <- p +
      ggtitle(paste0("Rolling three-monthly summary of the proportion of\n",
                     "surgeries conducted less than 7 weeks from a positive\n",
                     "PCR test for SARS-CoV-2"),
              subtitle = paste0("(Cohort = Patients ", cancer,
                                " a cancer diagnosis ", window, ")")) +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  } else {
    p <- p +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  }
  
  ggsave(filename = 
           paste0(fileprefix1,fileprefix2,"_",
                  "3monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png"),
         plot = p,
         device = "png",
         path = here::here("output"),
         width = 3.5, height = 3.25, units = "in")
}
