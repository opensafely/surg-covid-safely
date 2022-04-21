# redaction_function.R
#
# The purpose of this script is to define a function that will process any
# supplied dataframe to redaction any potentially-disclosive information.
#
#

redactos = function(x, era, threshold = 5)
{
  # Check is <x> is a data.frame.
  if (is.na(as.numeric(as.matrix(x))))
  {
    stop(paste0("The <x> object supplied is not numeric. The <x> ",
                   "object must a numeric dataframe."))
  }
  if (inherits(x, "data.frame") != T)
  {
    message(paste0("The <x> object supplied is not a dataframe.\n",
                   "Attempting to coerce the supplied <x> object to a dataframe..."))
    x <- data.frame(x)
    if (inherits(x, "data.frame") == T)
    {
      message("\t...coercing the supplied <x> object to a dataframe was successful.")
    } else {
      stop("\t...coercing the supplied <x> object to a dataframe was NOT successful.")
    }
  }
  # Check if <era> is specified.
  if (era != "pre" & era != "post")
  {
    stop(cat('The <era> variable was neither \"pre\" nor \"post\". Please check.'))
  }
  
  # # Check if variable is binary or multinomial.
  # if (nrow(x))
  # {
  #   
  # }
  
  
  if (era == "pre")
  {
  ########################
  ## PreEvent redaction ##
  ########################
    ## # Binary variable.
    # Check if any count is less than the threshold value.
    if (sum((x<threshold & x>0)) > 0)
    {
      # If any count < threshold, then redact all.
      x[] <- rep(NA, nrow(x))
    }
    else
    {
      # If no counts < threshold, then no redaction needed.
      message("Redaction necessary at the given threshold.")  
      return()
    }
    
    ## # Multinomial variable.
    ############################################################
    # Check if any count is less than the threshold value.
    if (sum((x<threshold & x>0)) > 0)
    {
      # If any count < threshold, then, first redact all guilty cells.
      rows_redact <- which((x<threshold & x>0))
      values_redact <- x[rows_redact,]
      x[rows_redact,] <- NA
      
      # Check if only one row has been redacted.
      len_rows_redact <- length(rows_redact)
      while (len_rows_redact = 1)
      {
        # If only one row has been redacted, we need to redact at least another
        # row so that the value redacted can't be computed for that row (by
        # comparing against the total).
        #
        # Get next smallest cell value and row index.
        next_smallest_val <- min(x[x>0,], na.rm = T)
        next_smallest_idx <- min(which(x==next_smallest_val))
        
        # Redact the next smallest cell.
        add_values_redact <- numeric();
        add_values_redact <- c(add_values_redact, next_smallest_val)
        add_rows_redact <- numeric();
        add_rows_redact <- c(add_rows_redact, next_smallest_idx)
        x[add_rows_redact,] <- NA
        
        # Check if the sum of redacted values is greater than the threshold.
        values_redact <- values_redact + add_values_redact
        
        
        #...
        # If another row was redacted, increase rows_redact by one.
        rows_redact <- c(rows_redact, add_rows_redact)
        len_rows_redact <- len_rows_redact + 1
      }
      
      #######################
      else
      {
        # If more that one row has been redacted, then we need to check that the
        # sum of values redacted is also greater than the threshold.
        # No need for further redaction
      }
    }
    else
    {
      # If no counts < threshold, then no redaction needed.
      message("Redaction necessary at the given threshold.")  
      return()
    }
  }
  
  
  
  
}



