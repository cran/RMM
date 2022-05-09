#' Reshape Long-Format Data
#'
#' This function reshapes a 'Long-Format' data (with the repeated measurements in separate rows) to 'Wide-Format' data  (with repeated measurements in separate columns of the same row).
#' The reshaped 'wide-format' data is an S3 class called 'rmm_data' and contains information for fitting the model with the \code{\link{rmm}} function.
#' Users who want to use the rmm function must first use the rmm_reshape function. The \code{\link{rmm}} function receives only S3 class 'rmm_data' as input.
#'
#' @param data data frame, a 'Long-Format' transaction data.
#' @param idvar character, variable name representing each individual's id in the transaction data.
#' @param resp character, variable name representing result of a individual choice.
#' @param alts character vector, variable names representing a alternatives.
#' @param asv character vector, variable names representing a alternative specific variables.
#' @param min_obs numeric, specify the minimum observation for each choice set in the transaction data.
#' @return The 'Wide-Format' data and various information required for the \code{\link{rmm}} function.
#' @examples
#' \donttest{
#' data(Hotel_Long)
#'
#' rst_reshape <- rmm_reshape(data=Hotel_Long, idvar="Booking_ID",
#' resp="Purchase", alts="Room_Type", asv="Price", min_obs=30)
#'
#' class(rst_reshape)  # "rmm_data"
#' ls(rst_reshape)     # "Alts_Code_Desc" "ASV" "asv_name" "data_wide"
#'                     # "Rem_Choice_Set"     "Removed_Choice_Set"
#'
#' rst_reshape$data_wide  # reshaped data
#' }
#' @seealso \code{\link{rmm}} for estimating parameters.
#' @export
rmm_reshape <- function(data, idvar, resp, alts, asv, min_obs){



  args <- match.call()[-1]

  idvar <- args$idvar # id variable
  resp <- args$resp   # response
  alts <- args$alts   # alternativs
  asv <- args$asv     # alternative-specific variable



  data_tib <- tibble::as_tibble(data) %>%
    dplyr::arrange(idvar, alts)



  vars <- colnames(data_tib)
  fixed <- vars[!vars %in% c(alts, asv, resp)]



  data_wide <- data_tib %>%
    dplyr::select(dplyr::all_of(fixed)) %>%
    dplyr::group_by_at(idvar) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  wide_vars <- colnames(data_wide)


  #### Ouput: (2) Alts code description ####

  Alts_Code_Desc <- unique(data_tib[, alts]) %>% dplyr::arrange_at(alts)
  nrow_Alts_Code_Desc <- nrow(Alts_Code_Desc)  # the number of unique products
  Alts_Code_Desc <- Alts_Code_Desc %>% dplyr::mutate(Alts_Code = 1:nrow_Alts_Code_Desc)
  Alts_Code_Desc <- Alts_Code_Desc[, c(2, 1)]


  data_tib <- dplyr::left_join(data_tib, Alts_Code_Desc, by=alts)


  decision <- data_tib[data_tib[, resp]==1, c(idvar, alts, "Alts_Code")]
  colnames(decision)[3] <- "Decis_Alts_Code"



  data_wide <- dplyr::left_join(data_wide, decision, by=idvar)



  num_id <- data_tib %>% dplyr::distinct_at(idvar) %>% nrow
  uniq_id <- data_tib %>% dplyr::distinct_at(idvar) %>% .[[1]]


  # load Cpp code

  Rcpp_rst <- RMM::Choice_Set(data_tib, num_id, uniq_id, idvar)
  Choice_Set <- Rcpp_rst$Choice_Set
  #row_idx <- Rcpp_rst$row_idx
  #where_one <- Rcpp_rst$where_one
  chk_num_alts <- Rcpp_rst$chk_num_alts



  uniq_choice_set <- sort(unique(Rcpp_rst$Choice_Set))



  choice_set_df <- tibble::tibble(idvar=uniq_id, Choice_Set=Choice_Set)
  names(choice_set_df)[1] <- idvar




  remove_id <- uniq_id[chk_num_alts == 1]


  remove_one_alts <- choice_set_df %>%
    dplyr::filter(get(idvar) %in% remove_id) %>%
    dplyr::select(Choice_Set) %>%
    dplyr::distinct() %>% .[[1]]


  data_tib <- dplyr::left_join(data_tib, choice_set_df, by=idvar) %>%
    dplyr::filter(!get(idvar) %in% remove_id)



  sel_choice_set <- uniq_choice_set[!table(Choice_Set) < min_obs]
  sel_choice_set <- sel_choice_set[sel_choice_set != remove_one_alts]
  nrow_sel_choice_set <- length(sel_choice_set)





  Rem_Choice_Set <- tibble::as_tibble(table(Choice_Set)[!table(Choice_Set) < min_obs])
  colnames(Rem_Choice_Set) <- c("Remaining_Choice_Set", "Observation")


  Rem_Choice_Set[, "Choice_Set_Code"] <- 1:nrow_sel_choice_set
  Rem_Choice_Set <- Rem_Choice_Set[, c(3, 1, 2)]



  Removed_Choice_Set <- tibble::as_tibble(table(Choice_Set)[table(Choice_Set) < min_obs | names(table(Choice_Set)) == remove_one_alts] )
  colnames(Removed_Choice_Set) <- c("Removed_Choice_Set", "Observation")



  data_wide <- dplyr::left_join(data_wide, choice_set_df, by=idvar)


  data_wide <- data_wide %>% dplyr::filter(Choice_Set %in% Rem_Choice_Set$Remaining_Choice_Set)
  data_wide <- dplyr::left_join(data_wide, Rem_Choice_Set, by=c("Choice_Set" = "Remaining_Choice_Set")) %>%
    dplyr::select(-Observation)




  nrow_wide <- nrow(data_wide)
  len_alts <- nrow(Alts_Code_Desc)


  data_sel <- data_tib %>% dplyr::filter(Choice_Set %in% sel_choice_set)
  selected_booking_id <- unique(data_sel$Booking_ID)


  ASV <- matrix(0, nrow=nrow_wide, ncol=len_alts)
  colnames(ASV) <- rep(asv, len_alts)
  for(i in 1:len_alts){
    colnames(ASV)[i] <- paste0(asv, "_", i, sep="")
  }


  for(i in 1:nrow_wide){

    input_attrb <- data_sel[data_sel[, idvar] == selected_booking_id[i], c(asv, "Alts_Code")]
    attrb_value <- input_attrb[, 1] %>% .[[1]]
    where_input <- input_attrb[, 2] %>% .[[1]]

    ASV[i,  where_input] <- attrb_value
  }

 data_wide <- cbind(data_wide, ASV)
 data_wide <- tibble::as_tibble(data_wide)
 ASV <- tibble::as_tibble(ASV)
 rst_lst <- list(data_wide=data_wide, Alts_Code_Desc=Alts_Code_Desc, Rem_Choice_Set=Rem_Choice_Set, Removed_Choice_Set=Removed_Choice_Set,
                 ASV=ASV, asv_name=asv)
 class(rst_lst) <- "rmm_data"

 return(rst_lst)
}
