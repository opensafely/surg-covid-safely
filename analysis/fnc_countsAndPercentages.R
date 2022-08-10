# fnc_countsAndPercentages.R
#
# This script defines a function that produces a table of the stratified counts
# and proportion of patients. The function  is called in the script entitled
# "Make_Table1.R".
#

fnc_countsAndPercentages <-
  function(table_to_use, strata, strata_col,
           interval_counts = c("all", "NC", "C_within3m", "C_outwith3m",
                               "C_within6m", "C_outwith6m"))
  {
    # Set default value.
    if(missing(interval_counts))
    {
      if(exists(sensitivity_cohort))
      {interval_counts <- sensitivity_cohort}
      else
      {interval_counts <- "all"}
    }
    # Define reusable items.
    era_longname <- c("Pre-pandemic", "Pandemic no vaccine",
                      "COVIDSurg data collection period", "Pandemic with vaccine")
    #era_shortname <- paste0(c("PP", "PNV", "CSP", "PWV"), strata)
    era_shortname <- c("PP", "PNV", "CSP", "PWV")
    
    # Make tables
    table_to_use <- table_to_use %>% dplyr::rename(strata = strata_col)
    counts_to_use <-
      table_counts %>% rownames_to_column() %>%
        dplyr::filter(grepl(interval_counts, .$rowname))
    

    for(i in 1:length(era_longname))
    {  
      i_table_to_use <- 
        table_to_use %>%
        dplyr::filter(era == era_longname[i], strata !="Missing") %>%
        dplyr::arrange(strata) %>% dplyr::ungroup()
      i_counts_to_use <-
        counts_to_use %>%
        dplyr::filter(grepl(era_shortname[i], .$rowname)) %>% dplyr::select(-rowname)
      # Get counts per intervals and overall.
      n <- i_table_to_use %>% dplyr::select(-c("era", strata))
      # Get percentages per intervals and overall.
      pct <- 
        n %>% mapply('/', ., i_counts_to_use) %>% '*'(100) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(dplyr::across(, ~ ifelse(is.nan(.),NA,.))) %>%
        tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                               "n_infection_0to2wk" = 0, "n_infection_3to4wk" = 0,
                               "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
        )) %>%
        `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                       "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
      # Interlace counts and percentages.
      era_Strata <-
        matrix(0,
               nrow = length(rownames(n)),
               ncol = length(colnames(n))*2) %>%
        as.data.frame()
      era_Strata[,seq(1,length(colnames(era_Strata)),2)] <- n
      era_Strata[,seq(2,length(colnames(era_Strata)),2)] <- pct
      colnames(era_Strata)[seq(1,length(colnames(era_Strata)),2)] <- colnames(n)
      colnames(era_Strata)[seq(2,length(colnames(era_Strata)),2)] <- colnames(pct)
      era_Strata <- i_table_to_use %>% dplyr::select(strata) %>%
        dplyr::bind_cols(era_Strata)
      # Assign era_shortname.
      assign(x = paste0(era_shortname[i], strata), value = era_Strata)
      # Clean up.
      rm(n, pct)
    }
    
    eval(parse(text = paste0("output <- list(",
                             paste(paste0(era_shortname, strata),
                                   collapse=","),
                             ")")))
    names(output) <- paste0(era_shortname, strata)
    return(output)
  }
