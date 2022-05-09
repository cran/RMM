#' Calculating standard error of estimate.
#'
#' Calculating statndard error of model parameters and gamma(no-purchase utility). Internal function that users should not call directly.
#' @param eta_wo_k numeric, estimated model parameters normalized for baseline product.
#' @param gamma numeric, estimated gamma(no-purchase utility).
#' @inheritParams loglike
#' @inheritParams ESTF_gamma
#' @param H matrix, a hessian matrix of the log-likelihood function.
#' @return a value of standard error of estimate.
#' @keywords internal


Sandwich <- function(eta_wo_k, gamma, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, H, cond){
  n_R <- nrow(Attr[[1]])

  B11 <- jacobian(U, x=gamma,
                  eta_wo_k=eta_wo_k,
                  Attr=Attr,
                  Choice_Set_List=Choice_Set_List,
                  Alts_Same_Choice_Set=Alts_Same_Choice_Set,
                  Is_Purchase=Is_Purchase,
                  Base_Prod=Base_Prod,
                  Num_Prod=Num_Prod,
                  Num_Attr=Num_Attr,
                  cond=cond)

  B12 <- jacobian(U, x=eta_wo_k,
                  gamma=gamma,
                  Attr=Attr,
                  Choice_Set_List=Choice_Set_List,
                  Alts_Same_Choice_Set=Alts_Same_Choice_Set,
                  Is_Purchase=Is_Purchase,
                  Base_Prod=Base_Prod,
                  Num_Prod=Num_Prod,
                  Num_Attr=Num_Attr,
                  cond=cond)

  B21 <- matrix(0, ncol=1, nrow=(Num_Prod-1 + Num_Attr))

  B22 <- H

  B <- rbind(cbind(B11, B12), cbind(B21, B22))

  InvB <- solve(B)




  S <- NULL
  for(i in 1:length(Alts_Same_Choice_Set)){

    S_temp <-  jacobian(iloglike, eta_wo_k, Choice_Set_List=Choice_Set_List,
                        Choice_Set_Code=i, Base_Prod=Base_Prod, Num_Prod=Num_Prod, Num_Attr=Num_Attr,
                        Alts_Same_Choice_Set=Alts_Same_Choice_Set, Is_Purchase=Is_Purchase)

    S <- rbind(S, S_temp)


  }

  S <- cbind(iU(eta_wo_k=eta_wo_k, gamma=gamma, Attr=Attr, Choice_Set_List=Choice_Set_List, Alts_Same_Choice_Set=Alts_Same_Choice_Set,
                Is_Purchase=Is_Purchase, Base_Prod=Base_Prod, Num_Prod=Num_Prod, Num_Attr=Num_Attr, cond=cond), S)


  VS <- stats::var(S) * (n_R - 1)


  rst <- InvB %*% VS %*% t(InvB)

}



