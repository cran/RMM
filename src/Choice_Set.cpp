#include <Rcpp.h>
using namespace Rcpp;

String con(NumericVector x){
  std::sort(x.begin(), x.end());
  Function con_r("paste0");

  return con_r(x, _["collapse"] = "|");
}

Function which_rcpp("which");
Function subset("[.data.frame");


DataFrame subset_test(DataFrame x, NumericVector y) {
  return subset(x, y, R_MissingArg);
}
//' Collects a choice set exposed to individuals.
//'
//' Collects a choice set exposed to individuals. Internal function that users should not call directly.
//'
//' @param df a long format tibble.
//' @param num_id Number of unique Booking_IDs appearing in transaction data
//' @param uniq_id Unique Booking_ID in transaction data.
//' @param idvar Variable name representing customer id (Booking_ID).
//' @return Returns a list containing the values required for calculation within the \code{\link{rmm_reshape}} function.
//' @export
// [[Rcpp::export]]
Rcpp::List Choice_Set(DataFrame df, int num_id, NumericVector uniq_id, String idvar) {
  NumericVector chk_num_alts(num_id);
  CharacterVector Choice_Set(num_id);
  LogicalVector row_idx(num_id);
  NumericVector where_one;
  NumericVector exposed_product;


  for(int i = 0; i < num_id; i++){

    row_idx = as<NumericVector>(df[idvar]) == uniq_id[i];
    where_one = which_rcpp(row_idx);
    exposed_product = as<NumericVector>(subset_test(df, where_one)["Alts_Code"]);

    chk_num_alts[i] = exposed_product.size();
    Choice_Set[i] = con(exposed_product);

  }



  Rcpp::List output;
  output["Choice_Set"] = Choice_Set;
  output["row_idx"] = row_idx;
  output["where_one"]  = where_one;
  output["exposed_product"] = exposed_product;
  output["chk_num_alts"] = chk_num_alts;
  output["Choice_Set"] = Choice_Set;

  return(output);
}
