#' Fitting Revenue Management Models
#'
#' \code{\link{rmm}} is used to fit Revenue Management Models. Users can specify
#' cl (conditional logit model) and ml (multinomial logit model) as RMM model.
#'
#' @param rmm_data an object of class "rmm_data", a output of \code{\link{rmm_reshape}} function.
#' @param prop numeric, user assumed market share.
#' @param model character, specify fitting method ("cl" or "ml"). "cl" (default) refers to the Conditional Logit Model, and "ml" refers to the Multinomial Logit Model.
#'
#' @return \code{\link{rmm}} returns an object of class inheriting from "rmm".
#' @seealso \code{rmm} fits the model with the RDE method introduced in \url{doi:10.2139/ssrn.3598259}.
#' @examples
#' \donttest{
#' data(Hotel_Long)
#'
#' # Before using the rmm function, the user must first use the rmm_shape function.
#' rst_reshape <- rmm_reshape(data=Hotel_Long, idvar="Booking_ID", alts="Room_Type",
#'                            asv="Price", resp="Purchase", min_obs=30)
#'
#' # Fitting a model
#' rst_rmm <- rmm(rst_reshape, prop=0.7, model="cl")
#' print(rst_rmm)
#' }
#' @export
rmm <- function(rmm_data, prop=0.7, model="cl"){

  # model
  if(model=="cl"){
    model_name <- "Conditional Logit Model"
  }else{
    model_name <- "Multinomial Logit Model"
  }


  # prop
  cond <- (1-prop)/prop


  cond1 <- max(cond - 0.15, 0.01)
  cond2 <- cond + 0.15



  #### Setting (start) ####


  odat <- rmm_data$data_wide  # Observed data
  n_R <- nrow(odat)  # # of observed sample
  odat <- dplyr::mutate(odat, id_temp=1:n_R)  # create temporary index


  Rem_Choice_Set <- rmm_data$Rem_Choice_Set
  Decis_Alts_Code <- odat$Decis_Alts_Code  # decision
  asv_name <- rmm_data$asv_name

  ASV <- rmm_data$ASV
  Attr <- list(ASV = ASV)

  Num_Prod <- ncol(Attr[[1]])  # product
  Num_Attr <- length(Attr)  # attribute
  Num_Set <- nrow(Rem_Choice_Set) # # of choice set


  # Choice_Set (Hotel Data)
  Choice_Set_List <- strsplit(x=Rem_Choice_Set$Remaining_Choice_Set, split="\\|")




  idx_Same_Choice_Set <- vector(mode="list", length=Num_Set)
  for(i in 1:Num_Set){
    idx_Same_Choice_Set[[i]] <- odat %>% dplyr::filter(Choice_Set_Code == i) %>% dplyr::select(id_temp) %>% .[[1]]
  }


  Alts_Same_Choice_Set <- vector(mode = "list", length = Num_Set)
  for(i in 1:Num_Set){
    Alts_Same_Choice_Set[[i]] <-  ASV[idx_Same_Choice_Set[[i]], ] %>% as.data.frame
  }


  ################################################
  #### Is_Purchase ####

  Is_Purchase <- vector(mode = "list", length = Num_Set)

  for(i in 1:Num_Set){

    i_Choice_Set <- as.numeric(Choice_Set_List[[i]])


    d <- matrix(0, nrow=length(idx_Same_Choice_Set[[i]]), ncol=length(i_Choice_Set))
    for(k in 1:length(i_Choice_Set)){
      d[which(Decis_Alts_Code[idx_Same_Choice_Set[[i]]] == i_Choice_Set[k]), k] <- 1
    }

    Is_Purchase[[i]] <- d

  }


  #### Setting (end) ####



  #### Estimating (start) ####

  rst <- ESTF(Alts_Same_Choice_Set=Alts_Same_Choice_Set, Choice_Set_List=Choice_Set_List, Num_Prod=Num_Prod, Num_Attr=Num_Attr, Is_Purchase=Is_Purchase)
  eta_wo_k <- rst$eta_wo_k
  Base_Prod <- rst$Base_Prod

  H <- rst$H  # hessian matrix

  # estimating gamma
  gamma <- ESTF_gamma(eta_wo_k, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, cond)

  gamma_L <- ESTF_gamma(eta_wo_k, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, cond1)
  gamma_U <- ESTF_gamma(eta_wo_k, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, cond2)


  if(gamma_L * gamma_U <= 0){
    CCB <- c(0, eta_wo_k) # CCB-UC
    gamma <- 0
  }else{
    CCB <- c(gamma, eta_wo_k)  # CCB-MS
  }


  Arrival_rst <- Arrival(CCB, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr)
  Total_Arrivals <- Arrival_rst$Total_Arrivals
  No_Purchase <- Arrival_rst$No_Purchase


  CCB_rst <- as.matrix(round(CCB, 4))

  var_xi <- Sandwich(eta_wo_k, gamma, Attr, Choice_Set_List, Alts_Same_Choice_Set, Is_Purchase, Base_Prod, Num_Prod, Num_Attr, H, cond)
  sd_xi <- sqrt(diag(var_xi))
  #### Estimating (end) ####


  # naming result

  rownames(CCB_rst) <- c(paste0("gamma (-ASC", Base_Prod, ")",sep="" ), rep("", nrow(CCB_rst) -2), asv_name)

  ASC_name <- NULL
  for(i in (1:Num_Prod)[!(1:Num_Prod) %in% Base_Prod]){
    ASC_temp <- paste0("ASC", i, sep="")
    ASC_name <- c(ASC_name, ASC_temp)
  }
  rownames(CCB_rst)[2:(Num_Prod)] <- ASC_name


  CCB_rst <- cbind(CCB_rst, round(sd_xi, 4))



  z <- round(CCB_rst[,1]/ CCB_rst[,2], 4)
  p_value <- round((1- stats::pnorm(abs(z)))*2, 4)


  ## Wald statistic
  # wald <- z^2
  # p_value <- 1-pchisq(wald, df=1)

  CCB_rst <- cbind(CCB_rst, z, p_value)
  colnames(CCB_rst) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")


  rst <- list(Model=model_name,
              Baseline_Product = Base_Prod,
              Coefficients=CCB_rst,
              Total_Arrivals=round(Total_Arrivals),
              Observed_Arrivals = n_R,
              No_Purchase=round(No_Purchase))


  class(rst) <- "rmm"

  return(rst)

}
##########################################################
