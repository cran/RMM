#' Estimating model parameters.
#'
#' Estimating model parameters except gamma(no-purchase utility) when a particular baseline_product is specified. Internal function that users should not call directly.
#' @inheritParams loglike
#' @return The estimated value of parameters except gamma(no-purchase utility) when a particular baseline_product is specified.
#' @keywords internal

ESTF_bp <- function(Base_Prod=1, Choice_Set_List, Alts_Same_Choice_Set, Num_Prod, Num_Attr, Is_Purchase){


  eta <- rep(0, Num_Prod-1 + Num_Attr)

  repeat{
    eta0 <- eta

    S <- matrix(0, nrow=(Num_Prod-1 + Num_Attr), ncol=length(Alts_Same_Choice_Set))
    for(i in 1:length(Alts_Same_Choice_Set)){
      jaco <- jacobian(loglike, eta, Choice_Set_List=Choice_Set_List, Choice_Set_Code=i,
                       Base_Prod=Base_Prod, Num_Prod=Num_Prod, Num_Attr=Num_Attr, Alts_Same_Choice_Set=Alts_Same_Choice_Set,
                       Is_Purchase=Is_Purchase)
      S[, i] <- jaco
    }
    S_rowsum <- rowSums(S)


    H <- vector(mode="list", length=length(Alts_Same_Choice_Set))
    for(i in 1:length(Alts_Same_Choice_Set)){
      hessi <- hessian(loglike, eta, Choice_Set_List=Choice_Set_List, Choice_Set_Code=i,
                       Base_Prod=Base_Prod, Num_Prod=Num_Prod, Num_Attr=Num_Attr, Alts_Same_Choice_Set=Alts_Same_Choice_Set,
                       Is_Purchase=Is_Purchase)
      H[[i]] <- hessi
    }
    H <- Reduce("+", H)

    eta <- eta0 - solve(H) %*% S_rowsum
    dif <- sum((eta0-eta)^2)
    if(dif < 1e-4) break}

  return(list(eta, H, S))
}


