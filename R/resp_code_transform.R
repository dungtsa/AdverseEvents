


#' Convert RECIST response code
#'
#' @param data A dataframe object that includes the column of interest to convert.
#' @param response_col A string. The column name of the column to convert.
#' @param new_col A string. The new column name of the converted response column.
#'                 Defaults to original response column name.
#' @param key A named vector key on how to convert the response IDs.
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' df <- data.frame(SampleID = c("A","B","C","D","E","F"),
#' Response = c("1","2","3","4","5","6"))
#' resp_code_transform(df,"Response", new_col = "Response_New")
#'
#' @usage
#' resp_code_transform(
#'   data = NULL,
#'   response_col = NULL,
#'   new_col = response_col,
#'   key = c("1" = "Baseline (BL)", 
#'           "2" = "Not Evaluable (NE)",
#'           "3" = "Stable Disease (SD)",
#'           "4" = "Partial Response (PR)", 
#'           "5" = "Complete Response (CR)", 
#'           "6" = "Progressive Disease (PD)")
#' )


resp_code_transform <- function(data = NULL, response_col = NULL, 
                                new_col = response_col,
                                  key = c("1" = "Baseline (BL)", 
                                          "2" = "Not Evaluable (NE)", 
                                          "3" = "Stable Disease (SD)",
                                          "4" = "Partial Response (PR)", 
                                          "5" = "Complete Response (CR)", 
                                          "6" = "Progressive Disease (PD)")) {

  if (is.null(data)) stop("Input data not found.")
  if (is.null(response_col)) stop("Response column ID not found")
  if (is.numeric(response_col)) stop("Please identify column of interest with 
                                     column name as string.")
  if (length(intersect(names(key),unique(data[,response_col]))) == 0) stop("No 
                   response IDs match response key names. Check key argument.")

  data[,new_col] <- key[as.character(data[,response_col])]
  return(data)

}
