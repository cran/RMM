#' RMM: fitting Revenue Management Models
#'
#' The RMM fits Revenue Management Models using the RDE(Robust Demand Estimation) method introduced by  Cho et al. (2020) \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3598259}.
#'
#' @docType package
#' @name RMM
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select arrange all_of group_by_at ungroup arrange_at left_join distinct_at distinct row_number
#' @importFrom numDeriv jacobian hessian
#' @importFrom stats pnorm var
#' @importFrom rlang .data
#' @import Rcpp
#' @useDynLib RMM
#'
NULL
if(getRversion() >= "3.1.0")  utils::globalVariables(c(".", "Choice_Set_Code", "id_temp", "Observation"))
