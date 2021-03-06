% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.rmm.R
\name{predict.rmm}
\alias{predict.rmm}
\title{Predict method for Revenue Management Model Fits}
\usage{
\method{predict}{rmm}(object, newdata, Rem_Choice_Set, Choice_Set_Code, fixed = TRUE, ...)
}
\arguments{
\item{object}{Object of class inheriting from "\code{rmm}"}

\item{newdata}{A data frame in which to look for variables with which to predict.}

\item{Rem_Choice_Set}{List of choice sets remaining in the data.}

\item{Choice_Set_Code}{Specifies the choice set of \code{newdata}.}

\item{fixed}{If fixed=TRUE, the alternative with the highest prediction probability is determined as decision.
Otherwise (fixed=FALSE), one of the alternatives is determined
in proportion to the predictive probability.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
\code{preict.rmm} produces a list of predictions, which contains decisions and probabilities.
}
\description{
Predicted values based on RMM object
}
\examples{
\donttest{
data(Hotel_Long)

# Before using the rmm function, the user must first use the rmm_shape function.
rst_reshape <-  rmm_reshape(data=Hotel_Long, idvar="Booking_ID",
     alts="Room_Type", asv="Price", resp="Purchase", min_obs=30)

# Fitting a model
rst_rmm <- rmm(rst_reshape, prop=0.7, model="cl")

# Predictions
Rem_Choice_Set <- rst_reshape$Rem_Choice_Set

newdata1 <- data.frame(Price_1=c(232, 122, 524), Price_3=c(152, 531, 221),
                       Price_4=c(163, 743, 192), Price_5=c(132, 535, 325),
                       Price_7=c(136, 276, 673), Price_8=c(387, 153, 454),
                       Price_9=c(262, 163, 326), Price_10=c(421, 573, 472))

predict(rst_rmm, newdata=newdata1, Rem_Choice_Set=Rem_Choice_Set,
        Choice_Set_Code=3, fixed=TRUE)


newdata2 <- data.frame(Price_1=c(521, 321, 101, 234, 743),
                       Price_5=c(677, 412, 98, 321, 382),
                       Price_8=c(232, 384, 330, 590, 280))

predict(rst_rmm, newdata=newdata2, Rem_Choice_Set=Rem_Choice_Set,
        Choice_Set_Code=7, fixed=FALSE)
}
}
