# fnc_serviceEvaluationigures_dataPlot.R
#
# This script defines a function that plots the submitted dataset  The function
# is called in the script entitled "preprintFigure.R".
#

fnc_serviceEvaluationFigures_dataPlot <-
  function(data, cancer = c("with", "without", ""),
           window, figureCaption = F, strata = NULL, y_axis_top = 100)
{
  # Check arguments.
  if(!cancer %in% c("with", "without", "")){stop("Argument <cancer> must be with 'with', 'without', or ''. ")}
  if(cancer == "with") {fileprefix1 <- "cancer"}
    else
    {if(cancer == "without") {fileprefix1 <- "noCancer"}else{fileprefix1 <- ""}
      }
  if(window == "")
  {fileprefix2 <- "_"; subtitleSuffix <- ""} else {
      fileprefix2 <- paste0("_",window,"months_")
      subtitleSuffix <- paste0(" within ", window, " months of\nsurgery")
  }
  if (is.null(strata))
  {fileprefix3 <- ""} else {fileprefix3 <- "_stratified"}
  if (is.null(y_axis_top)){y_axis_top <- 100}
  
    
  # Plot showing 6-monthly summary of the proportion of surgeries conducted less
  # than 7 weeks from a positive PCR test for SARS-CoV-2.----
  if(missing(strata) | is.null(strata))
  {
    p <- 
      data %>%
      dplyr::group_by(Year_surgery, Month_surgery) %>%
      dplyr::summarise(sixMonthly_prop_within_7wk = median(sixMonthly_prop_within_7wk))%>%
      ggplot(.,
             aes(x = interaction(Month_surgery, Year_surgery),
                 y = sixMonthly_prop_within_7wk)) +
      geom_col()
    
  } else {
    p <- 
      data %>%
      dplyr::group_by(Admission_method, Year_surgery, Month_surgery) %>%
      dplyr::summarise(sixMonthly_prop_within_7wk = median(sixMonthly_prop_within_7wk))%>%
      ggplot(.,
             aes_string(x = "interaction(Month_surgery, Year_surgery)",
                        y = "sixMonthly_prop_within_7wk",
                        fill = strata
             )) +
      geom_col(position = position_dodge2()) +
      scale_fill_grey(start = 0.5, end = 0.2) +
      guides(fill = guide_legend(title = strata)) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8))
  }
  
  p <- p + 
    scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
    scale_y_continuous(limits = c(0, y_axis_top),
                       labels = floor(seq(from = 0 , to = y_axis_top, length.out =5)),
                       breaks = floor(seq(from = 0 , to = y_axis_top, length.out =5))) +
    ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
    xlab("") +
    geom_vline(xintercept = 13) + # Index for March 2020
    annotate(x = 13, y = y_axis_top*0.95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
    annotate(x = 13, y = y_axis_top*0.75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 23) + # Index for January 2021
    annotate(x = 23, y = y_axis_top*0.70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
    geom_vline(xintercept = 25) + # Index for March 2021
    annotate(x = 25, y = y_axis_top*0.95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  
  
  if(figureCaption == T)
  {
    p <- p +
      ggtitle(paste0("Rolling six-monthly summary of the proportion of\n",
                     "surgeries conducted less than 7 weeks from a positive\n",
                     "PCR test for SARS-CoV-2"),
              subtitle = paste0("(Cohort = Patients ", cancer,
                                " a cancer diagnosis ", window, ")")) +
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  } else {
    p <- p +
      theme(axis.title = element_text(size = 8),
            axis.text = element_text(size = 10),
            axis.line = element_line(colour = "black"))
  }
  
  ggsave(filename = 
           paste0(fileprefix1,fileprefix2,fileprefix3,"_",
                  "6monthly_summary_proportion_surgeries_within_7wks_after_pos_test.tiff"),
         plot = p,
         device = "tiff",
         path = here::here("output"),
         width = 3.5, height = 3.25, units = "in",
         dpi = 600)
  
    

  # ## The weekly, monthly, 2-monthly, and 3-monthly plots have been discontinued
  # ## because there aren't enough events to withstand the low-number disclosure
  # ## measures. Only a 4-monthly plot is now produced.
  # 
  # # Plot showing weekly summary of the proportion of surgeries conducted less
  # # than 7 weeks from a positive PCR test for SARS-CoV-2.----
  #   if(missing(strata) | is.null(strata))
  #   {
  #     p <- data %>%
  #       ggplot(.,
  #              aes(x = interaction(Week_surgery, Month_surgery, Year_surgery),
  #                  y = weekly_prop_within_7wk)) +
  #       geom_col()
  #     
  #   } else {
  #     p <- data %>%
  #       ggplot(.,
  #              aes_string(x = "interaction(Week_surgery, Month_surgery, Year_surgery)",
  #                         y = "weekly_prop_within_7wk",
  #                         fill = strata
  #              )) +
  #       geom_col(position = position_dodge2()) +
  #       scale_fill_grey(start = 0.5, end = 0.2) +
  #       guides(fill = guide_legend(title = strata)) +
  #       theme(legend.position = "bottom",
  #             legend.title = element_blank(),
  #             legend.text = element_text(size = 8))
  #   }  
  # 
  # p <- p + 
  #   scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
  #   scale_y_continuous(limits = c(0, 100),
  #                      labels = c(0, 25, 50, 75, 100),
  #                      breaks = c(0, 25, 50, 75, 100)) +
  #   theme(panel.grid.major.x = element_blank()) +
  #   ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
  #   xlab("") +
  #   geom_vline(xintercept = 54) + # Index for March 2020
  #   annotate(x = 54, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 56) + # Index for March 2020
  #   annotate(x = 56, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 98) + # Index for January 2021
  #   annotate(x = 98, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 106) + # Index for March 2021
  #   annotate(x = 106, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  # 
  # if(figureCaption == T)
  # {
  # p <- p +
  #   ggtitle(paste0("Rolling weekly summary of the proportion of\n",
  #                  "surgeries conducted less than 7 weeks from a positive\n",
  #                  "PCR test for SARS-CoV-2"),
  #           subtitle = paste0("(Cohort = Patients ", cancer,
  #                             " a cancer diagnosis", subtitleSuffix, ")")) +
  #   theme(plot.title = element_text(size = 8),
  #         plot.subtitle = element_text(size = 7),
  #         axis.title = element_text(size = 8),
  #         axis.text = element_text(size = 10),
  #         axis.line = element_line(colour = "black"))
  # } else {
  #   p <- p +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black"),
  #           plot.margin = margin(t = 20)) 
  # }
  # 
  # ggsave(filename = 
  #          paste0(fileprefix1,fileprefix2,fileprefix3,"_",
  #                 "weekly_summary_proportion_surgeries_within_7wks_after_pos_test.tiff"),
  #        plot = p,
  #        device = "tiff",
  #        path = here::here("output"),
  #        width = 3.5, height = 3.25, units = "in",
  #        dpi = 600)
  # #----
  # 
  # # Plot showing monthly summary of the proportion of surgeries conducted less
  # # than 7 weeks from a positive PCR test for SARS-CoV-2. ----  
  # if(missing(strata) | is.null(strata))
  # {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(monthly_prop_within_7wk = median(monthly_prop_within_7wk)) %>%
  #     ggplot(.,
  #            aes(x = interaction(Month_surgery, Year_surgery),
  #                y = monthly_prop_within_7wk)) +
  #     geom_col()
  #   
  # } else {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Admission_method, Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(monthly_prop_within_7wk = median(monthly_prop_within_7wk)) %>%
  #     ggplot(.,
  #            aes_string(x = "interaction(Month_surgery, Year_surgery)",
  #                       y = "monthly_prop_within_7wk",
  #                       fill = strata
  #            )) +
  #     geom_col(position = position_dodge2()) +
  #     scale_fill_grey(start = 0.5, end = 0.2) +
  #     guides(fill = guide_legend(title = strata)) +
  #     theme(legend.position = "bottom",
  #           legend.title = element_blank(),
  #           legend.text = element_text(size = 8))
  # }
  # 
  # p <- p +
  #   scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
  #   scale_y_continuous(limits = c(0, 100),
  #                      labels = c(0, 25, 50, 75, 100),
  #                      breaks = c(0, 25, 50, 75, 100)) +
  #   ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
  #   xlab("") +
  #   geom_vline(xintercept = 13) + # Index for March 2020
  #   annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
  #   annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 23) + # Index for January 2021
  #   annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 25) + # Index for March 2021
  #   annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  # 
  # if(figureCaption == T)
  # {
  #   p <- p +
  #     ggtitle(paste0("Rolling monthly summary of the proportion of\n",
  #                    "surgeries conducted less than 7 weeks from a positive\n",
  #                    "PCR test for SARS-CoV-2"),
  #             subtitle = paste0("(Cohort = Patients ", cancer,
  #                               " a cancer diagnosis ", window, ")")) +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black"))
  # } else {
  #   p <- p +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black")) 
  # }
  # 
  # ggsave(filename = 
  #          paste0(fileprefix1,fileprefix2,fileprefix3,"_",
  #                 "1monthly_summary_proportion_surgeries_within_7wks_after_pos_test.tiff"),
  #        plot = p,
  #        device = "tiff",
  #        path = here::here("output"),
  #        width = 3.5, height = 3.25, units = "in",
  #        dpi = 600)
  # # ----
  # 
  # # Plot showing 2-monthly summary of the proportion of surgeries conducted less
  # # than 7 weeks from a positive PCR test for SARS-CoV-2.----
  # if(missing(strata) | is.null(strata))
  # {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(twoMonthly_prop_within_7wk = median(twoMonthly_prop_within_7wk)) %>%
  #     ggplot(.,
  #            aes(x = interaction(Month_surgery, Year_surgery),
  #                y = twoMonthly_prop_within_7wk)) +
  #     geom_col()
  #   
  # } else {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Admission_method, Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(twoMonthly_prop_within_7wk = median(twoMonthly_prop_within_7wk)) %>%
  #     ggplot(.,
  #            aes_string(x = "interaction(Month_surgery, Year_surgery)",
  #                       y = "twoMonthly_prop_within_7wk",
  #                       fill = strata
  #            )) +
  #     geom_col(position = position_dodge2()) +
  #     scale_fill_grey(start = 0.5, end = 0.2) +
  #     guides(fill = guide_legend(title = strata)) +
  #     theme(legend.position = "bottom",
  #           legend.title = element_blank(),
  #           legend.text = element_text(size = 8))
  # }
  # 
  # p <- p +
  #   scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
  #   scale_y_continuous(limits = c(0, 100),
  #                      labels = c(0, 25, 50, 75, 100),
  #                      breaks = c(0, 25, 50, 75, 100)) +
  #   ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
  #   xlab("") +
  #   geom_vline(xintercept = 13) + # Index for March 2020
  #   annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
  #   annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 23) + # Index for January 2021
  #   annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 25) + # Index for March 2021
  #   annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  # 
  # if(figureCaption == T)
  # {
  #   p <- p +
  #     ggtitle(paste0("Rolling two-monthly summary of the proportion of\n",
  #                    "surgeries conducted less than 7 weeks from a positive\n",
  #                    "PCR test for SARS-CoV-2"),
  #             subtitle = paste0("(Cohort = Patients ", cancer,
  #                               " a cancer diagnosis ", window, ")")) +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black"))
  # } else {
  #   p <- p +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black")) 
  # }
  # 
  # ggsave(filename = 
  #          paste0(fileprefix1,fileprefix2,fileprefix3,"_",
  #                 "2monthly_summary_proportion_surgeries_within_7wks_after_pos_test.tiff"),
  #        plot = p,
  #        device = "tiff",
  #        path = here::here("output"),
  #        width = 3.5, height = 3.25, units = "in",
  #        dpi = 600)
  # # ----
  # 
  # # Plot showing 3-monthly summary of the proportion of surgeries conducted less
  # # than 7 weeks from a positive PCR test for SARS-CoV-2.----
  # if(missing(strata) | is.null(strata))
  # {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(threeMonthly_prop_within_7wk = median(threeMonthly_prop_within_7wk))%>%
  #     ggplot(.,
  #            aes(x = interaction(Month_surgery, Year_surgery),
  #                y = threeMonthly_prop_within_7wk)) +
  #     geom_col()
  #     
  # } else {
  #   p <- 
  #     data %>%
  #     dplyr::group_by(Admission_method, Year_surgery, Month_surgery) %>%
  #     dplyr::summarise(threeMonthly_prop_within_7wk = median(threeMonthly_prop_within_7wk))%>%
  #     ggplot(.,
  #            aes_string(x = "interaction(Month_surgery, Year_surgery)",
  #                y = "threeMonthly_prop_within_7wk",
  #                fill = strata
  #                )) +
  #     geom_col(position = position_dodge2()) +
  #     scale_fill_grey(start = 0.5, end = 0.2) +
  #     guides(fill = guide_legend(title = strata)) +
  #     theme(legend.position = "bottom",
  #           legend.title = element_blank(),
  #           legend.text = element_text(size = 8))
  # }
  # 
  # p <- p + 
  #   scale_x_discrete(guide = guide_axis_nested(check.overlap = T)) +
  #   scale_y_continuous(limits = c(0, 100),
  #                      labels = c(0, 25, 50, 75, 100),
  #                      breaks = c(0, 25, 50, 75, 100)) +
  #   ylab("Proportion of surgeries conducted less\n than 7 weeks from a positive PCR test (%)") +
  #   xlab("") +
  #   geom_vline(xintercept = 13) + # Index for March 2020
  #   annotate(x = 13, y = 95, label = "1st COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2) +
  #   annotate(x = 13, y = 75, label = "NICE\nguidelines",  hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 23) + # Index for January 2021
  #   annotate(x = 23, y = 70, label = "Estimated\ndate of vaccine\neffectiveness", hjust = "right", geom = "label", size = 2) +
  #   geom_vline(xintercept = 25) + # Index for March 2021
  #   annotate(x = 25, y = 95, label = "2nd COVIDSurg\npublication",  hjust = "right", geom = "label", size = 2)
  # 
  # 
  # if(figureCaption == T)
  # {
  #   p <- p +
  #     ggtitle(paste0("Rolling three-monthly summary of the proportion of\n",
  #                    "surgeries conducted less than 7 weeks from a positive\n",
  #                    "PCR test for SARS-CoV-2"),
  #             subtitle = paste0("(Cohort = Patients ", cancer,
  #                               " a cancer diagnosis ", window, ")")) +
  #     theme(plot.title = element_text(size = 8),
  #           plot.subtitle = element_text(size = 7),
  #           axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black"))
  # } else {
  #   p <- p +
  #     theme(axis.title = element_text(size = 8),
  #           axis.text = element_text(size = 10),
  #           axis.line = element_line(colour = "black"))
  # }
  # 
  # ggsave(filename = 
  #          paste0(fileprefix1,fileprefix2,fileprefix3,"_",
  #                 "3monthly_summary_proportion_surgeries_within_7wks_after_pos_test.tiff"),
  #        plot = p,
  #        device = "tiff",
  #        path = here::here("output"),
  #        width = 3.5, height = 3.25, units = "in",
  #        dpi = 600)
  # # ----

  # ----
}
