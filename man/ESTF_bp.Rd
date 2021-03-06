% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ESTF_bp.R
\name{ESTF_bp}
\alias{ESTF_bp}
\title{Estimating model parameters.}
\usage{
ESTF_bp(
  Base_Prod = 1,
  Choice_Set_List,
  Alts_Same_Choice_Set,
  Num_Prod,
  Num_Attr,
  Is_Purchase
)
}
\arguments{
\item{Base_Prod}{numeric, the baseline product number}

\item{Choice_Set_List}{list, product code included for each choice set.}

\item{Alts_Same_Choice_Set}{list, for internal calculation, a list of data exposed to the same choice set.}

\item{Num_Prod}{numeric, the number of all products in observed data.}

\item{Num_Attr}{numeric, the number of attributes in observed data.}

\item{Is_Purchase}{list, for internal calculation, list of customers who are
exposed to the same choice set who purchased the product. If purchased 1, otherwise 0.}
}
\value{
The estimated value of parameters except gamma(no-purchase utility) when a particular baseline_product is specified.
}
\description{
Estimating model parameters except gamma(no-purchase utility) when a particular baseline_product is specified. Internal function that users should not call directly.
}
\keyword{internal}
