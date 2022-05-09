#' Estimating total arrivals and the number of no purchase.
#'
#' Estimating total arrivals and the number of no purchase. Internal function that users should not call directly.
#' @param CCB numeric vector, estimated model parameters(gamma and eta).
#' @param Attr list, a set of data frame in which alternative specific variables.
#' @inheritParams loglike
#' @return The estimated value of total arrivals and the number of no purchase.
#' @keywords internal

Arrival <- function(CCB, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr){
  n_R <- nrow(Attr[[1]])
  Num_Set <- length(Choice_Set_List)
  gamma <- CCB[1]
  eta_wo_k <- CCB[-1]

  bin2 <- vector(mode="numeric", length = Num_Set)
  for(i in 1:Num_Set){

    i_Choice_Set <- as.numeric(Choice_Set_List[[i]])
    bin <- matrix(rep(0, times=nrow(Is_Purchase[[i]]) * ncol(Is_Purchase[[i]]) ) , nrow=nrow(Is_Purchase[[i]]), ncol=ncol(Is_Purchase[[i]]) )

    # normalizing covariates
    dx <- Alts_Same_Choice_Set[[i]] - Alts_Same_Choice_Set[[i]][, Base_Prod]


    for(j in 1:ncol(Is_Purchase[[i]])){



      if(i_Choice_Set[j] > Base_Prod){

        bin[, j] <- t( exp(eta_wo_k[i_Choice_Set[j] - 1]  +  eta_wo_k[(Num_Prod - 1) + Num_Attr] * dx[i_Choice_Set[j]]) )

      }else if(i_Choice_Set[j] == Base_Prod){

        bin[, j] <- t( exp(eta_wo_k[(Num_Prod - 1) + Num_Attr] * dx[i_Choice_Set[j]]) )

      }else if(i_Choice_Set[j] < Base_Prod){

        bin[, j] <- t( exp(eta_wo_k[i_Choice_Set[j]] + eta_wo_k[(Num_Prod - 1) + Num_Attr] * dx[i_Choice_Set[j]]) )

      }

    }

    bin2[i] <- sum( 1 / rowSums(bin) )

  }

  No_Purchase <- exp(gamma) * sum(bin2)

  Arrival <- n_R + No_Purchase


  return(list(Total_Arrivals=Arrival, No_Purchase=No_Purchase))
}
