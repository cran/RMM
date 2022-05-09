#' The log-likelihood value for each individual and each alternative.
#'
#' For function that calculating standard error of estimate. Internal function that users should not call directly.
#' @inheritParams loglike
#' @return The log-likelihood value for each individual and each alternative.
#' @keywords internal
iloglike <- function(eta, Choice_Set_List, Choice_Set_Code, Base_Prod, Num_Prod, Num_Attr, Alts_Same_Choice_Set, Is_Purchase){
  ## input argument description ##
  # eta: Parameters eta=(alpha, beta)
  # Exposed_Choice_Set: A choice set exposed to customer.
  # Base_Prod: A baseline product
  # Alts_Same_Choice_Set: A list that is classified by each choice set code.

  i_Choice_Set <- as.numeric(Choice_Set_List[[Choice_Set_Code]])#  string -> numeric
  beta_loc <- Num_Prod - 1 + Num_Attr

  # normalizing covariates
  dx <- as.matrix(Alts_Same_Choice_Set[[Choice_Set_Code]] - Alts_Same_Choice_Set[[Choice_Set_Code]][, Base_Prod])

  nr_likelihood_dat <- nrow(Alts_Same_Choice_Set[[Choice_Set_Code]])

  # numerator
  num <- matrix(rep(0,  nr_likelihood_dat * Num_Prod),
                nrow=nr_likelihood_dat, ncol=Num_Prod)

  # If the baseline product is contained given the choice set, numerator = 1
  if(Base_Prod %in% i_Choice_Set){
    num[, Base_Prod] <- 1


    for(i in setdiff(i_Choice_Set, Base_Prod)){

      if(i < Base_Prod){
        num[, i] <- exp( eta[i] + eta[ (Num_Prod-1)+1 ] * dx[, i] )  # pi_ij's numerator
      }else{
        num[, i] <- exp( eta[i-1] + eta[ (Num_Prod-1)+1 ] * dx[, i] )  # pi_ij's numerator
      }  # End If

    }  # End For

  }else{

    for(i in i_Choice_Set){

      if(i < Base_Prod){
        num[, i] <- exp(eta[i] + eta[ (Num_Prod-1)+1 ] * dx[, i])  # pi_ij's numerator
      }else{
        num[, i] <- exp(eta[i-1] + eta[ (Num_Prod-1)+1 ] * dx[, i])  # pi_ij' numerator
      }  # End If

    }  # End For

  }  # End If




  dnum <- rowSums(num[, i_Choice_Set])  # denominator

  p <- num[, i_Choice_Set] / dnum  # pi

  ilogl <- vector(mode="numeric", length=nrow(p))
  for(i in 1:nrow(p)){
    select <- log(p)
    sel_col <- Is_Purchase[[Choice_Set_Code]] == 1
    ilogl[i] <- select[i , sel_col[i, ]]
  }

  return(ilogl)

}

