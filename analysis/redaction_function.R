# redaction_function.R
#
# The purpose of this script is to define a funcation that will process any
# supplied dataframe to redaction any potentially-disclosive information.
#
#

redactos = function(data, era)
{
  # Check is <data> is a data.frame.
  if (is.na(as.numeric(as.matrix(data))))
  {
    warning(paste0("The <data> object supplied is not numeric. The <data> ",
                   "object must a numeric dataframe."))
    return()
  }
  if (inherits(data, "data.frame") != T)
  {
    message(paste0("The <data> object supplied is not a dataframe.\n",
                   "Attempting to coerce the supplied <data> object to a dataframe..."))
    data <- data.frame(data)
    if (inherits(data, "data.frame") == T)
    {
      message("\t...coercing the supplied <data> object to a dataframe was successful.")
    } else {
      message("\t...coercing the supplied <data> object to a dataframe was NOT successful.")
    }
  }
  # Check if <era> is specified.
  if (era != "pre" & era != "post")
  {
    print(cat('The <era> variable was neither \"pre\" nor \"post\". Please check.'))
    return()
  }
  
  # # Check if variable is binary or multinomial.
  # if (nrow(data))
  # {
  #   
  # }
  
  
}



