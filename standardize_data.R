##################################
# standardize_data.R
# Sep 25, 2017
# Anthony Williams
#
# Read in common datasets and subset to desired columns.
# If columns are not present in the dataset, add them
#
#
##################################


standardize_data <- function(data, subset=NA, rename=list(), add_cols=list()) {
  
  # Read in data based on filetype
  if (any(class(data) == "data.frame")) ds <- data
  else ds <- switch(tolower(tools::file_ext(data)),
               "sas7bdat"=haven::read_sas(data),
               "csv"=read.csv(data, stringsAsFactors=FALSE),
               "txt"=read.delim(data, stringsAsFactors=FALSE))
  
  # Rename columns
  names(ds)[names(ds) %in% names(rename)] <- unlist(unname(rename))
  
  # Add add_cols to data
  if (length(add_cols)>0) ds <- cbind(ds, add_cols)
  
  # After adding columns, check if any subset columns are not present. Fill with NA and warn
  if (!all(subset %in% names(ds))){
    mc <- subset[!subset %in% names(ds)]
    names(mc) <-  mc
    
    add_mc <- lapply(mc, function(x) NA)
    ds <- cbind(ds, add_mc)
    
    warning(paste(c(unname(mc), "Columns missing from data. They are filled with NA."), collapse=" "))
  }
  
  # Subset data based on subset vector
  if (any(!is.na(subset))) ds <- ds[,subset]

  ds
  
}

