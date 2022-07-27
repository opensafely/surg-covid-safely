# fnc_countsAndPercentages.R
#
# This script defines a function that produces a table of the stratified counts
# and proportion of patients. The function  is called in the script entitled
# "Make_Table1.R".
#

fnc_countsAndPercentages <-
  function(table_to_use, strata, strata_col)
  {

    # Make sure variable 'OS_all_counts' exists.
    if(!exists("all_counts"))
    {
      stop(paste0("\nVariable 'all_counts' is needed for this function.",
                  "\nPlease, run Make_Table1.R."))
      
    }
    # Define reusable items.
    era_longname <- c("Pre-pandemic", "Pandemic no vaccine",
                      "COVIDSurg data collection period", "Pandemic with vaccine")
    era_shortname <- paste0(c("PP", "PNV", "CSP", "PWV"), strata)
    
    # Make tables
    table_to_use <- table_to_use %>% dplyr::rename(strata = strata_col)
    counter = 1
    for(i_era in era_longname)
    {  
      # Get counts per intervals and overall.
      n <- 
        table_to_use %>%
        dplyr::filter(era == i_era, strata !="Missing") %>%
        dplyr::arrange(strata) %>% dplyr::ungroup() %>%
        dplyr::select(-c("era", strata))
      # Get percentages per intervals and overall.
      pct <- 
        n %>%
        '/'(all_counts %>% dplyr::filter(era == i_era) %>%
              dplyr::select(n) %>% as.integer()) %>% '*'(100) %>%
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
      era_Strata <- table_to_use %>%
        dplyr::filter(era == i_era, strata !="Missing") %>%
        dplyr::arrange(strata) %>% dplyr::select(strata) %>%
        dplyr::bind_cols(era_Strata)
      # Assign era_shortname.
      assign(x = era_shortname[counter], value = era_Strata)
      # Clean up.
      rm(n, pct)
      counter = counter + 1
    }
    
    eval(parse(text = paste0("output <- list(", paste(era_shortname, collapse=","), ")")))
    names(output) <- era_shortname
    return(output)
  }
