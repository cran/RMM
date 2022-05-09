#' Calculating a value of score function with respect to parameter gamma.
#'
#' Calculating a value of score function with respect to parameter gamma. Internal function that users should not call directly.
#' @inheritParams Sandwich
#' @return a value of score function with respect to parameter gamma.
#' @keywords internal

U <- function(eta_wo_k, gamma, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, cond){
  n_R <- nrow(Attr[[1]])

  Num_Set <- length(Choice_Set_List)

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
    sum_j <- 1 / rowSums(bin)
    bin2[i] <- sum( sum_j )
  }
  rst <- (1/n_R) * exp(gamma) * sum(bin2) - (cond)

  return(rst)
}
