
#' Title
#'
#' @param rtnse_fit object returned by \link[Rtsne]{Rtsne}
#' @param ids name of each samples input into Rtsne (character).
#' @param id_column_in_metadata name of column in sample_metadata_dataframe containing sample identifier (string)
#' @param sample_metadata_dataframe  sample metadata. Should be 1 row per sample (dataframe)
#'
#' @return dataframe
#' @export
#'
rtsne_convert_dataframe <- function(rtnse_fit, ids, sample_metadata_dataframe = NULL, id_column_in_metadata = "id"){
  `Number of samples in tsne` = rtnse_fit$N
  assertive::assert_are_identical(x = `Number of samples in tsne`,y = length(ids), allow_attributes = TRUE)
  assertive::assert_is_a_string(id_column_in_metadata)

  df = dplyr::tibble(
    id = ids,
    X = rtnse_fit$Y[,1],
    Y = rtnse_fit$Y[,2]
  )

  if(!is.null(sample_metadata_dataframe)){
    assertive::assert_is_data.frame(sample_metadata_dataframe)
    assertive::assert_is_identical_to_true(id_column_in_metadata %in% colnames(sample_metadata_dataframe))
    df <- dplyr::left_join(x=df, y = sample_metadata_dataframe, by = list(x="id", y=id_column_in_metadata))
  }

  return(df)
}
