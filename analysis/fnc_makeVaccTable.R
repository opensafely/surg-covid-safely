 # Function that makes the final table.
#
#

makeVaccTable <- function(inputTable, prefix = c('SNOMED', 'TPP', 'AlwynSNOMED', 'AlwynTPP'))
{
  ###########################################
  # Make vectors to inform the kable table. #
  ###########################################
  # ----
  # Row names for later table.
  TableRowNames <- 
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(postOp_mortality_30day)
  # Pre-March 2020 totals.
  # ## 30-day post-operative mortality.
  n_preDec2020_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_per_group)
  prop_preDec2020_Vacc_postOp_mortality_30day <-
    n_preDec2020_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery") %>%
    select(n_per_group) %>% sum()
  
  # Post-March 2020 totals.
  # ## 30-day post-operative mortality.
  n_postDec2020_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_per_group)
  prop_postDec2020_Vacc_postOp_mortality_30day <-
    n_preDec2020_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_per_group) %>% sum()
  
  # No pre-operative infection.
  # ## 30-day post-operative mortality.
  n_subtotals_infection_none_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_infection_none)
  prop_subtotals_infection_none_Vacc_postOp_mortality_30day <-
    n_subtotals_infection_none_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_infection_none) %>% sum()
  
  # Pre-operative infection (0-2 weeks).
  # ## 30-day post-operative mortality.
  n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_infection_0to2wk)
  prop_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day <-
    n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_infection_0to2wk) %>% sum()
  
  # Pre-operative infection (3-4 weeks).
  # ## 30-day post-operative mortality.
  n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_infection_3to4wk)
  prop_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day <-
    n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_infection_3to4wk) %>% sum()
  
  # Pre-operative infection (5-6 weeks).
  # ## 30-day post-operative mortality.
  n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_infection_5to6wk)
  prop_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day <-
    n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_infection_5to6wk) %>% sum()
  
  # Pre-operative infection (>=7 weeks).
  # ## 30-day post-operative mortality.
  n_subtotals_infection_7wk_Vacc_postOp_mortality_30day <-
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                  (postOp_mortality_30day=="Dead within 30-day post-operation"|
                     postOp_mortality_30day=="Alive within 30-day post-operation" |
                     postOp_mortality_30day=="Missing")) %>%
    dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
    dplyr::select(n_infection_7wk)
  prop_subtotals_infection_7wk_Vacc_postOp_mortality_30day <-
    n_subtotals_infection_7wk_Vacc_postOp_mortality_30day /
    inputTable %>%
    dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
    select(n_infection_7wk) %>% sum()
  # ----
  
  #####################
  # Make kable table. #
  #####################
  # ----
  # Make data frame.
  df_Vacc <- data.frame(
    # Row names.
    TableRowNames,
    # Pre-December 2020 totals, n.
    n_preDec2020_Vacc_postOp_mortality_30day,
    # Pre-December 2020 totals, %.
    round(prop_preDec2020_Vacc_postOp_mortality_30day * 100, 0),
    # Post-December 2020 totals, n.
    n_postDec2020_Vacc_postOp_mortality_30day,
    # Post-December 2020 totals, %.
    round(prop_postDec2020_Vacc_postOp_mortality_30day * 100, 0),
    # No pre-operative infection, n.
    n_subtotals_infection_none_Vacc_postOp_mortality_30day,
    # No pre-operative infection, %.
    round(prop_subtotals_infection_none_Vacc_postOp_mortality_30day * 100, 0),
    # Pre-operative infection (0-2 weeks), n.
    n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day,
    # Pre-operative infection (0-2 weeks), %.
    round(prop_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day * 100, 0),
    # Pre-operative infection (3-4 weeks), n.
    n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day,
    # Pre-operative infection (3-4 weeks), %.
    round(prop_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day * 100, 0),
    # Pre-operative infection (5-6 weeks), n.
    n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day,
    # Pre-operative infection (5-6 weeks), %.
    round(prop_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day * 100, 0),
    # Pre-operative infection (>=7 weeks), n.
    n_subtotals_infection_7wk_Vacc_postOp_mortality_30day,
    # Pre-operative infection (>=7 weeks), %.
    round(prop_subtotals_infection_7wk_Vacc_postOp_mortality_30day * 100, 0)
  )
  
  # Label data frame.
  colnames(df_Vacc) <- c(" ", rep(c("n", "%"),7))
  # Make kable table depending on prefix.
  if (prefix == "SNOMED")
  {
    myCaption = paste0("<b>Table *</b> Thirty-day post-operative ",
                       "mortality for cancer patients undergoing surgery. ",
                       "Stratified by time from indication of ",
                       "SARS-CoV-2 infection (columns) and by ",
                       "vaccination status (rows). Vaccination ",
                       "status is indicated by SNOMED-CT clinical codes.",
                       "\nValues are counts (n) and percentages (%).", collapse = "")
    df_Vacc %>%
      kableExtra::kbl(caption = myCaption,
                      format = 'html') %>%
      kableExtra::kable_classic(full_width = F,
                                fixed_thead = T,
                                html_font="Cambria") %>%
      kableExtra::pack_rows(index = c("Confirmed fully vaccinated before test" = 3,
                                      "Confirmed partially vaccinated before test" = 3,
                                      "At least partially vaccinated before test" = 3,
                                      "Confirmed not vaccinated before test" = 3,
                                      "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing" = 3,
                                      "Error: No data on vaccine administration" = 3,
                                      "Unknown vaccination status before test" = 3
      )) %>%
      kableExtra::add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                                     "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
      kableExtra::add_header_above(c(" " = 1, "Pre-December 2020,\ntotals" = 2,
                                     "Post-December 2020,\ntotals" = 2,
                                     "No indication of infection" = 2,
                                     "\nIndication of infection" = 8)) %>%
      kableExtra::add_header_above(c(" " = 5, "Post-December 2020" = 10)) %>%
      kableExtra::column_spec(1, width = "25em") %>%
      kableExtra::column_spec(c(2:15), width = "5em") %>%
      kableExtra::row_spec(0, align = "c") #%>%
    #kableExtra::save_kable(file = here::here("output","Tables_pretty",paste0(prefix, "_TableVacc_3mths",".png")))
    
    # Save data frame.
    saveName <- paste0("df_Vacc_3mths", "_", prefix)
    assign(saveName,
           data.frame(" " =
                        rep(c("Confirmed fully vaccinated before test",
                              "Confirmed partially vaccinated before test",
                              "At least partially vaccinated before test",
                              "Confirmed not vaccinated before test",
                              "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
                              "Error: No data on vaccine administration",
                              "Unknown vaccination status before test"
                              ),
                            each = 3),
                      df_Vacc)
    ) %>%
    write.csv(
     file = here::here("output",paste0(prefix,"_table_Vaccination_status_3mths", ".csv"))
    )
    
  } else if (prefix == "TPP")
  {
    myCaption = paste0("<b>Table *</b> Thirty-day post-operative ",
                       "mortality for cancer patients undergoing surgery. ",
                       "Stratified by time from indication of ",
                       "SARS-CoV-2 infection (columns) and by ",
                       "vaccination status (rows). Vaccination ",
                       "status is indicated by TPP clinical codes.",
                       "\nValues are counts (n) and percentages (%).", collapse = "")
    df_Vacc %>%
      kableExtra::kbl(caption = myCaption,
                      format = 'html') %>%
      kableExtra::kable_classic(full_width = F,
                                fixed_thead = T,
                                html_font="Cambria") %>%
      kableExtra::pack_rows(index = c("Confirmed fully vaccinated before test" = 3,
                                      "Confirmed partially vaccinated before test" = 3,
                                      "At least partially vaccinated before test" = 3,
                                      "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing" = 3,
                                      "Error: No data on vaccine administration" = 3,
                                      "Unknown vaccination status before test" = 3
      )) %>%
      kableExtra::add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                                     "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
      kableExtra::add_header_above(c(" " = 1, "Pre-December 2020,\ntotals" = 2,
                                     "Post-December 2020,\ntotals" = 2,
                                     "No indication of infection" = 2,
                                     "\nIndication of infection" = 8)) %>%
      kableExtra::add_header_above(c(" " = 5, "Post-December 2020" = 10)) %>%
      kableExtra::column_spec(1, width = "25em") %>%
      kableExtra::column_spec(c(2:15), width = "5em") %>%
      kableExtra::row_spec(0, align = "c") #%>%
    #kableExtra::save_kable(file = here::here("output","Tables_pretty",paste0(prefix, "_TableVacc_3mths",".png")))
    
    # Save data frame.
    saveName <- paste0("df_Vacc_3mths", "_", prefix)
    assign(saveName,
           data.frame(" " =
                        rep(c("Confirmed fully vaccinated before test",
                              "Confirmed partially vaccinated before test",
                              "At least partially vaccinated before test",
                              "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
                              "Error: No data on vaccine administration",
                              "Unknown vaccination status before test"),
                        each = 3),
                      df_Vacc)
    ) %>%
      write.csv(file = here::here("output",paste0(prefix,"_table_Vaccination_status_3mths", ".csv"))
    )
    
  } else if (prefix == "AlwynSNOMED" | prefix == "AlwynTPP")
  {
    if (prefix == "AlwynSNOMED")
    {
      myCaption = paste0("<b>Table *</b> Thirty-day post-operative ",
                         "mortality for cancer patients undergoing surgery. ",
                         "Stratified by time from indication of ",
                         "SARS-CoV-2 infection (columns) and by ",
                         "vaccination status (rows). Vaccination ",
                         "status is indicated by a basic logic using ",
                         "SNOMED-CT clinical codes.",
                         "\nValues are counts and percentages.", collapse = "")
    } else
    {
      myCaption = paste0("<b>Table *</b> Thirty-day post-operative ",
                         "mortality for cancer patients undergoing surgery. ",
                         "Stratified by time from indication of ",
                         "SARS-CoV-2 infection (columns) and by ",
                         "vaccination status (rows). Vaccination ",
                         "status is indicated by a basic logic using ",
                         "TPP clinical codes.",
                         "\nValues are counts (n) and percentages (%).", collapse = "")
    }
    
    df_Vacc %>%
      kableExtra::kbl(caption = myCaption,
                      format = 'html') %>%
      kableExtra::kable_classic(full_width = F,
                                fixed_thead = T,
                                html_font="Cambria") %>%
      kableExtra::pack_rows(index = c("Confirmed fully vaccinated before test" = 3,
                                      "Double vaccination before test not confirmed" = 3
      )) %>%
      kableExtra::add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                                     "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
      kableExtra::add_header_above(c(" " = 1, "Pre-December 2020,\ntotals" = 2,
                                     "Post-December 2020,\ntotals" = 2,
                                     "No indication of infection" = 2,
                                     "\nIndication of infection" = 8)) %>%
      kableExtra::add_header_above(c(" " = 5, "Post-December 2020" = 10)) %>%
      kableExtra::column_spec(1, width = "25em") %>%
      kableExtra::column_spec(c(2:15), width = "5em") %>%
      kableExtra::row_spec(0, align = "c") #%>%
    #kableExtra::save_kable(file = here::here("output","Tables_pretty",paste0(prefix, "_TableVacc_3mths",".png")))
    
    # Save data frame.
    saveName <- paste0("df_Vacc_3mths", "_", prefix)
    assign(saveName,
           data.frame(" " =
                        rep(c("Confirmed fully vaccinated before test",
                              "Double vaccination before test not confirmed"),
                        each = 3),
                      df_Vacc)
    ) %>%
      write.csv(file = here::here("output",paste0(prefix, "_table_Vaccination_status_3mths", ".csv"))
    )
    
  }
  
  
  
  
  # ----
}