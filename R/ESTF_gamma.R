#' Estimating model parameter gamma(no-purchase utility).
#'
#' Estimating model parameter gamma(no-purchase utility) given estimated model parameters. Internal function that users should not call directly.
#' @inheritParams loglike
#' @param Attr list, a set of data frame in which alternative specific variables.
#' @param cond numeric, the value associated with prop, which is the argument of the \code{\link{rmm}} function. (1-prop)/prop.
#' @return The estimated value of parameters except gamma(no-purchase utility).
#' @keywords internal

ESTF_gamma <- function(eta_wo_k, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, cond){

  n_R <- nrow(Attr[[1]])
  Num_Set <- length(Choice_Set_List)


  bin2 <- vector(mode="numeric", length = Num_Set)
  for(i in 1:Num_Set){

    i_Choice_Set <- as.numeric(Choice_Set_List[[i]])

    bin <- matrix(0, nrow=nrow(Is_Purchase[[i]]), ncol=ncol(Is_Purchase[[i]]) )

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

  rst <- log(n_R * cond) - log(sum(bin2))

  return(rst)
}
