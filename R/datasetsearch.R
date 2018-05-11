#' Get information about data sets in a package.
#' 
#' @param package_name The name of an R package. The package must be loaded.
#' @param types A vector of variable types that should be counted.
#' @return A data frame summarizing the data sets in a package
#' @export

get_dataset_info <- function(package_name = "datasets", 
                             types = c("factor", "numeric", 
                                       "ordered", "integer", "ts")){
  dataset_list <- data(package = package_name)
  package_datasets <- as.data.frame(dataset_list[["results"]], stringsAsFactors = FALSE)
  package_datasets$short <- gsub( " .*$", "", package_datasets$Item )
  package_datasets[c("n_vars", "n_rows", types) ] <- NA

  dataset_name_list <- list()
  # Uh oh  Generate a list of data frames, can I do this more nicely?
  for (i in 1:nrow(package_datasets)){
    dataset_name_list[[i]] <- get(package_datasets$short[i])
  }
   class_name <- lapply(dataset_name_list, class)
   # Get the first name when there is more than one. 
   package_datasets$class <- lapply(class_name, "[[", 1)
   package_datasets$n_classes <- purrr::map(class_name, length)

   # Not pretty but it works ...
   for (i in seq_along(package_datasets[,1])){ 
   # Better to offer more options for class, probably.
    if (package_datasets$class[i] == "data.frame"){
     package_datasets[i,] <- get_dataframe_info(package_datasets[i,])
    }
   }
  package_datasets
}

get_dataframe_info <- function(datasets_df_i ){

  df_item <- get(datasets_df_i$short)
  var_classes <- unlist(lapply(df_item, class))
  n_var_classes <- table(var_classes)
  datasets_df_i[names(n_var_classes)] <- n_var_classes
  datasets_df_i$n_vars <- length(df_item)
  datasets_df_i$n_rows <- nrow(df_item)
  datasets_df_i
}