#' Calculating the log-likelihood function.
#'
#' Calculating model's log-likelihood function. Internal function that users should not call directly.
#' @param eta numeric vector, the model parameters to be estimated
#' @param Choice_Set_List list, product code included for each choice set.
#' @param Choice_Set_Code numeric, choice set code Number specified for calculating log-likelihood.
#' @param Base_Prod numeric, the baseline product number
#' @param Num_Prod numeric, the number of all products in observed data.
#' @param Num_Attr numeric, the number of attributes in observed data.
#' @param Alts_Same_Choice_Set list, for internal calculation, a list of data exposed to the same choice set.
#' @param Is_Purchase list, for internal calculation, list of customers who are
#' exposed to the same choice set who purchased the product. If purchased 1, otherwise 0.
#' @return The value of log-likelihood.
#' @keywords internal

loglike <- function(eta, Choice_Set_List, Choice_Set_Code, Base_Prod, Num_Prod,
                    Num_Attr, Alts_Same_Choice_Set, Is_Purchase){
## input argument description ##
# eta: Parameters eta=(alpha, beta)
# Exposed_Choice_Set: A choice set exposed to customer.
# Base_Prod: A baseline product
# Alts_Same_Choice_Set: A list that is classified by each choice set code.

  i_Choice_Set <- as.numeric(Choice_Set_List[[Choice_Set_Code]])

  beta_loc <- Num_Prod - 1 + Num_Attr

  # normalizing covariates
  dx <- as.matrix(Alts_Same_Choice_Set[[Choice_Set_Code]] -
                    Alts_Same_Choice_Set[[Choice_Set_Code]][, Base_Prod])

  nr_likelihood_dat <- nrow(Alts_Same_Choice_Set[[Choice_Set_Code]])

  # numerator
  num <- matrix(rep(0,  nr_likelihood_dat * Num_Prod),
                nrow=nr_likelihood_dat, ncol=Num_Prod)

  # If the baseline product is contained gi ven the choice set, numerator = 1
  if(Base_Prod %in% i_Choice_Set){
    num[, Base_Prod] <- 1


    for(i in setdiff(i_Choice_Set, Base_Prod)){

      if(i < Base_Prod){
        # pi_ij's numerator
        num[, i] <- exp( eta[i] + eta[ (Num_Prod-1)+1 ] * dx[, i] )
      }else{
        # pi_ij's numerator
        num[, i] <- exp( eta[i-1] + eta[ (Num_Prod-1)+1 ] * dx[, i] )
      }  # End If

    }  # End For

  }else{

    for(i in i_Choice_Set){

      if(i < Base_Prod){
        # pi_ij's numerator
        num[, i] <- exp(eta[i] + eta[ (Num_Prod-1)+1 ] * dx[, i])
      }else{
        # pi_ij' numerator
        num[, i] <- exp(eta[i-1] + eta[ (Num_Prod-1)+1 ] * dx[, i])
      }  # End If

    }  # End For

  }  # End If




  dnum <- rowSums(num[, i_Choice_Set])  # denominator

  p <- num[, i_Choice_Set] / dnum  # pi

  logl <- sum( diag( t(Is_Purchase[[Choice_Set_Code]]) %*% log(p) ))

  return(logl)

}


