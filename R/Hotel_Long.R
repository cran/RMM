#' Data from a Major Hotel Chain
#'
#' '\code{Hotel_Long}', a 'Long format', is a preprocessing data of the publicly available 'Hotel 1' data introduced in Bodea et al. (2009).
#'
#' 'Hotel 1' data contains information on the available alternatives, i.e.,
#' choice sets and the associated prices at the time of each customer’s booking decision.
#' We preprocessed 'Hotel 1' data and provide it in two types of
#' data format, '\code{Hotel_Long}' and '\code{Hotel_Wide}'.
#'
#' The following are the preprocessing of 'Hotel 1' data.
#'
#'   1. Customers' booking transactions that had only one room type available
#'            in their choice set were removed as our methods require at least
#'            two different products in each choice set.
#'
#'   2. Duplicate records was removed.
#'
#'   3. Choice sets with less than 30 observations, representing rare case were removed.
#'
#'
#' @format '\code{Hotel_Long}': A data frame with 8,318 rows and 11 variables:
#' \describe{
#'   \item{Booking_ID}{ID associated with a booking. Begins at one for each hotel property.}
#'   \item{Purchase}{Indicator variable equal to one if the product identified by product ID is purchased, zero otherwise.}
#'   \item{Room_Type}{Code describing the room type associated with the product ID.}
#'   \item{Price}{The average nightly rate the customer pays in USD (e.g., $199.99).
#'   Note that the average nightly rate will not match the rate of any available product rates
#'   if an upsell occurs at time of check-in, if the customer requests a specific
#'   discount rate at time of check-in, etc.}
#'   \item{Party_Size}{Number of adults and children associated with the booking.}
#'   \item{Membership_Status}{Status in rewards program (0—not a member, 1—basic, 2—elevated, 3—premium).}
#'   \item{VIP_Membership_Status}{Membership status of a VIP rewards program member (0—not a VIP, 1—basic VIP, 2—premium VIP member).}
#'   \item{Booking_Date}{Date the booking was created (e.g., 20070303 = March 3, 2007).}
#'   \item{Check_In_Date}{Check-in date (e.g., 20070307 = March 7, 2007).}
#'   \item{Check_Out_Date}{Check-out date (e.g., 20070310 = March 10, 2007).}
#'   \item{Length_of_Stay}{Length of stay/number of nights (e.g., three).}
#'  }
#' @source \url{doi:10.1287/msom.1080.0231}
"Hotel_Long"

