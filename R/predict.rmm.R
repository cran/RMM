#' Predict method for Revenue Management Model Fits
#'
#' Predicted values based on RMM object
#'
#' @param object Object of class inheriting from "\code{rmm}"
#' @param newdata A data frame in which to look for variables with which to predict.
#' @param Rem_Choice_Set List of choice sets remaining in the data.
#' @param Choice_Set_Code Specifies the choice set of \code{newdata}.
#' @param fixed If fixed=TRUE, the alternative with the highest prediction probability is determined as decision.
#'              Otherwise (fixed=FALSE), one of the alternatives is determined
#'              in proportion to the predictive probability.
#' @param ... further arguments passed to or from other methods.
#' @return \code{preict.rmm} produces a list of predictions, which contains decisions and probabilities.
#' @examples
#' \donttest{
#' data(Hotel_Long)
#'
#' # Before using the rmm function, the user must first use the rmm_shape function.
#' rst_reshape <-  rmm_reshape(data=Hotel_Long, idvar="Booking_ID",
#'      alts="Room_Type", asv="Price", resp="Purchase", min_obs=30)
#'
#' # Fitting a model
#' rst_rmm <- rmm(rst_reshape, prop=0.7, model="cl")
#'
#' # Predictions
#' Rem_Choice_Set <- rst_reshape$Rem_Choice_Set
#'
#' newdata1 <- data.frame(Price_1=c(232, 122, 524), Price_3=c(152, 531, 221),
#'                        Price_4=c(163, 743, 192), Price_5=c(132, 535, 325),
#'                        Price_7=c(136, 276, 673), Price_8=c(387, 153, 454),
#'                        Price_9=c(262, 163, 326), Price_10=c(421, 573, 472))
#'
#' predict(rst_rmm, newdata=newdata1, Rem_Choice_Set=Rem_Choice_Set,
#'         Choice_Set_Code=3, fixed=TRUE)
#'
#'
#' newdata2 <- data.frame(Price_1=c(521, 321, 101, 234, 743),
#'                        Price_5=c(677, 412, 98, 321, 382),
#'                        Price_8=c(232, 384, 330, 590, 280))
#'
#' predict(rst_rmm, newdata=newdata2, Rem_Choice_Set=Rem_Choice_Set,
#'         Choice_Set_Code=7, fixed=FALSE)
#' }
#' @export
predict.rmm <- function(object, newdata, Rem_Choice_Set, Choice_Set_Code, fixed=TRUE, ...){

  model_name <- object$Model
  message <- paste0("Prdiction by ", model_name,".", sep="" )

  Choice_Set_List <- strsplit(x=Rem_Choice_Set$Remaining_Choice_Set, split="\\|")

  i_Choice_Set <- as.numeric(Choice_Set_List[[Choice_Set_Code]])

  Base_Prod <- object$Baseline_Product

  est <- object$Coefficients[, 1]
  gamma <- est[1]
  asc <- est[2:(length(est)-1)]
  beta <- est[length(est)]


  # normalizing
  if(sum(i_Choice_Set %in% Base_Prod) == 0){
    dx <- newdata
  }else{
    for(i in 1:ncol(newdata)){
    dx <- newdata -  newdata[, which(i_Choice_Set %in% Base_Prod)]
    }
  }


  idx <- i_Choice_Set[!(i_Choice_Set %in% Base_Prod)]
  idx[idx > Base_Prod] <- idx[idx > Base_Prod] - 1

  asc_cal <- asc[idx]
  if(sum(i_Choice_Set %in% Base_Prod) != 0){
  if(which(i_Choice_Set %in% Base_Prod) == 1){
    asc_cal <- c(0, asc_cal)
  }else if(which(i_Choice_Set %in% Base_Prod) == length(i_Choice_Set)){
    asc_cal <- c(asc_cal, 0)
  }else{
    asc_cal <- c(asc_cal[which(i_Choice_Set %in% Base_Prod)-1],
                 0,
                 asc_cal[which(i_Choice_Set %in% Base_Prod):(length(i_Choice_Set)-1)])
  }
  }





  dnum <- matrix(0, nrow=nrow(dx), ncol=length(i_Choice_Set))
  if(sum(i_Choice_Set %in% Base_Prod) == 0){ # no base_prod

    for(i in which(!(i_Choice_Set %in% Base_Prod))){
      dnum[, i] <- exp(asc_cal[i] + beta * dx[, i])
    }
  }else{
    for(i in which(!(i_Choice_Set %in% Base_Prod))){

      dnum[, i] <- exp(asc_cal[i] + beta * dx[, i])


    }
    dnum[, which(i_Choice_Set %in% Base_Prod)] <- 1


  }


  prob <- matrix(0, nrow=nrow(dx), ncol=length(i_Choice_Set))


  for(i in 1:nrow(dnum)){
    prob[i, ] <- dnum[i, ] / rowSums(dnum)[i]
  }


  # decision
  decision <- vector(mode="numeric", length=nrow(prob))
  if(fixed == TRUE){
      decision <- apply(prob, 1, function(x) which(x == max(x)))
      decision <- i_Choice_Set[decision]
  }else{
    for(i in 1:nrow(prob)){
      decision[i] <- sample(i_Choice_Set, size=1, prob=prob[i, ])
    }
  }


  colnames(prob) <- paste0("Alts_", i_Choice_Set, sep="")


 rst <- list(Model=message, Decision=decision, Probabiltiy=prob)
 return(rst)
}




