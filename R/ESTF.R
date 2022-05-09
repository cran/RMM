#' Estimating model parameters.
#'
#' Estimating model parameters except gamma(no-purchase utility). Internal function that users should not call directly.
#' @inheritParams loglike
#' @return The estimated value of parameters except gamma(no-purchase utility).
#' @keywords internal
ESTF <- function(Alts_Same_Choice_Set, Choice_Set_List, Num_Prod, Num_Attr, Is_Purchase){

  est1 <- ESTF_bp(Base_Prod=1, Choice_Set_List=Choice_Set_List, Alts_Same_Choice_Set=Alts_Same_Choice_Set, Num_Prod=Num_Prod, Num_Attr=Num_Attr, Is_Purchase=Is_Purchase)
  parest <- est1[[1]][1:(Num_Prod-1)]
  bp <- which(parest == min(parest)) + 1

  test <- sum(parest > 0) == (Num_Prod-1)


  if(test == TRUE){
    est <- est1
    bp <- 1
  }


  if(test != TRUE){
    est <- ESTF_bp(Base_Prod=bp, Choice_Set_List=Choice_Set_List, Alts_Same_Choice_Set=Alts_Same_Choice_Set, Num_Prod=Num_Prod, Num_Attr=Num_Attr, Is_Purchase=Is_Purchase)
  }

  eta_wo_k <- est[[1]]
  var_eta_uc <- -solve(est[[2]])
  H <- est[[2]]
  score <- est[[3]]

  return(list(eta_wo_k=eta_wo_k, var_eta_uc=var_eta_uc, Base_Prod=bp, score=score, H=H))

}
