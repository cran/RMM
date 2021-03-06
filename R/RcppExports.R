# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Collects a choice set exposed to individuals.
#'
#' Collects a choice set exposed to individuals. Internal function that users should not call directly.
#'
#' @param df a long format tibble.
#' @param num_id Number of unique Booking_IDs appearing in transaction data
#' @param uniq_id Unique Booking_ID in transaction data.
#' @param idvar Variable name representing customer id (Booking_ID).
#' @return Returns a list containing the values required for calculation within the \code{\link{rmm_reshape}} function.
#' @export
Choice_Set <- function(df, num_id, uniq_id, idvar) {
    .Call('_RMM_Choice_Set', PACKAGE = 'RMM', df, num_id, uniq_id, idvar)
}

