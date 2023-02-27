#' Title
#'
#' @param data data to run TSNE on. Each row should be a sample. Must contain at leat one numeric column
#' @param id_col name of the column in data that represents the sample identifier.
#' @param perplexity tsne perplexity
#' @param ... set any other argument of \link[Rtsne]{Rtsne}
#' @param remove_non_numeric_columns  remove non numeric columns (bool)
#' @param remove_zero_variance_columns remove columns with zero variance (bool)
#' @param check_duplicates Should Rtsne check for dups after PCA (bool)
#' @param return_preprocessed_dt Don't run TSNE. Just preprocess the data then return the data.table bool
#' @param input_is_mutable_datatable are we allowed to mutate the input dataset in place (Should never be directly set - defaults to false) (bool)
#'
#' @return Rtsne (tibble)
#' @export
#'
#' @examples
#' \dontrun{
#' tsne_wide(iris)
#' }
tsne_wide <- function(data, id_col = NULL, perplexity = 30, remove_non_numeric_columns = TRUE, remove_zero_variance_columns = TRUE,  check_duplicates = TRUE, input_is_mutable_datatable = FALSE, return_preprocessed_dt = FALSE, ...){
  # Intro assertions
  assertive::assert_is_data.frame(data)
  assertive::assert_all_are_greater_than(ncol(data), 1)
  assertive::assert_all_are_greater_than(nrow(data) - 1, 3 * perplexity)

  if(input_is_mutable_datatable)
    df = data
  else
    df = data.table::copy(data.table::as.data.table(data))


  # Add basic id col if none specified
  if (is.null(id_col)) {
    id_vals = as.character(seq_along(data[[1]]))
    }
  else{
    assertive::assert_is_a_non_empty_string(id_col)
    assertive::assert_all_are_true(id_col %in% colnames(data))
    assertive::assert_no_duplicates(data[[id_col]])

    id_vals = as.character(df[[id_col]])
    #rownames(df) <- df[[id_col]]
    df[, c(id_col) := NULL]
  }



  # Select only numeric columns
  if(remove_non_numeric_columns){
    column_is_numeric = sapply(df, FUN = is.numeric)

    if(!all(column_is_numeric)){
      total_numeric_columns = sum(column_is_numeric==FALSE)
      assertive::assert_all_are_greater_than(total_numeric_columns, 0)

      message("Dropping ", total_numeric_columns, " non-numeric column/s: ", paste0(names(column_is_numeric)[column_is_numeric==FALSE], collapse=", "))

      numeric_columns = names(column_is_numeric)[column_is_numeric==TRUE]
      non_numeric_columns = names(column_is_numeric)[column_is_numeric==FALSE]
      df[, c(non_numeric_columns) := NULL]
    }
    else{
     message("Keeping all columns since all are numeric")
    }
  }

  # Remove columns with no variance
  if(remove_zero_variance_columns){
    number_of_unique_observations_per_column = apply(df, MARGIN = 2, FUN = data.table::uniqueN)
    zero_variance_columns = names(number_of_unique_observations_per_column[number_of_unique_observations_per_column == 1])

    if(length(zero_variance_columns) > 0){
      message("Dropping ", length(zero_variance_columns), " columns with Zero variance [", paste0(zero_variance_columns, collapse = ", "), "]")
      df[, c(zero_variance_columns) := NULL]
    }
    else
      message("Passed check: no columns jave zero variance")
  }

  #Maybe should add dropping of near zero variance columns

  # make inf values reasonable
  message("Setting any infinite values to the largest / smallest datapoint in that column")
  for (j in 1:ncol(df)) set(df, which(df[[j]] == Inf), j, max(df[[j]][is.finite(df[[j]])]))
  for (j in 1:ncol(df)) set(df, which(df[[j]] == -Inf), j, min(df[[j]][is.finite(df[[j]])]))

  # Drop columns with NAs
  columns_na_status = apply(df, MARGIN = 2, anyNA)
  columns_with_na = names(columns_na_status)[columns_na_status==TRUE]

  if(length(columns_with_na) > 0){
    message("Removing ", length(columns_with_na), " columns that contain na [", paste0(columns_with_na, collapse = ", "), "]")
    df[,c(columns_with_na) := NULL]
  }
  else
    message("Passed check: no columns contain NA")

  if(return_preprocessed_dt){
    message("Returning tsne-compatible data.table (due to return_preprocessed_dt = TRUE)")
    return(df)
  }
  # Run Rtsne
  rtsne_fit <- Rtsne::Rtsne(df, perplexity = perplexity, check_duplicates = check_duplicates, ...)

  return(rtsne_convert_dataframe(rtsne_fit, ids = id_vals))
}

#' Run tsne from long data format
#'
#'
#' @param data long format dataframe - should contain one column with sample level identifiers (data.frame)
#' @param id_col name of column describing ID (string)
#' @param names_from column containing names of the features (string)
#' @param values_from column containing values of the feature
#' @inheritParams tsne_wide
#' @inheritDotParams tsne_wide
#'
#' @details
#' ... param also allows user to set any param of \link[Rtsne]{Rtsne}
#'
#' @return Rtsne dataframe
#' @export
#'
tsne_long <- function(data, id_col, names_from, values_from, check_duplicates = TRUE, ...){
  # Still need to Add assertions

  df = data.table::as.data.table(data)
  message("Reshaping dt to wide form")
  df = data.table::dcast(df, formula = paste0(id_col, " ~ ", names_from), value.var = values_from)
  tsne_wide(df, id_col = id_col, input_is_mutable_datatable = TRUE, check_duplicates = check_duplicates, ...)
}
