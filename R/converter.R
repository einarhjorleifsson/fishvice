#' @title Converts FLStock object to rbya
#'
#' @description The rbya (results by year and age) is a long \code{data.frame}
#' popular in Reykjavik
#'
#' @export
#'
#' @param x An FLStock object
#' @param scale scaler used on abundance values (stock in numbers, catches etc)
#' @param project A boolean, if TRUE (default), propagates terminal stock numbers
#' forward by one year (into the assessment year). Note that the weights in the
#' assessment year are the same as in the terminal year.
#' @param plusgroup A boolean, if TRUE (default), last age group is a plus group.
#' Only used if project is TRUE.

flstock_to_rbya <- function(x, scale=1, project = TRUE, plusgroup = TRUE)
{

  y    <- reshape2::melt(x@stock.n@.Data,value.name = "n")[,c("year","age","n")]
  y$n  <- y$n/scale
  y$f  <- reshape2::melt(x@harvest@.Data)[,c("value")]
  # if(class(x) != "FLSAM") {  # This may be needed
  y$oC <- reshape2::melt(x@catch.n@.Data)[,c("value")]/scale
  y$cW <- reshape2::melt(x@catch.wt@.Data)[,c("value")]
  y$sW <- reshape2::melt(x@stock.wt@.Data)[,c("value")]
  y$oD  = reshape2::melt(x@discards.n@.Data)[,c("value")]/scale
  y$dW  = reshape2::melt(x@discards.wt@.Data)[,c("value")]
  y$oL  = reshape2::melt(x@landings.n@.Data)[,c("value")]/scale
  y$lW  = reshape2::melt(x@landings.wt@.Data)[,c("value")]
  y$mat = reshape2::melt(x@mat@.Data)[,c("value")]
  y$pF  = reshape2::melt(x@harvest.spwn@.Data)[,c("value")]
  y$pM  = reshape2::melt(x@m.spwn@.Data)[,c("value")]
  y$m   = reshape2::melt(x@m@.Data)[,c("value")]

  # propagate stock forward
  if (project) {
    y2 <- y[y$year == max(y$year),]
    y2$year <- y2$year + 1
    y2$n <- y2$n * exp(-(y2$m + y2$f))
    if(plusgroup) {
      y2$n[(nrow(y2)-1)] <- y2$n[(nrow(y2)-1)] + y2$n[nrow(y2)]
    }
    y2$n <- c(NA, y2$n[2:length(y2$n)])
    y2$f <- y2$oC <- y2$oD <- y2$oL <- NA

    y <- rbind(y, y2)
  }

  return(tibble::as_data_frame(y))

}

#' @title Converts FLIndices object to rbya
#'
#' @description The rbya (results by year and age) is a long \code{data.frame}
#' popular in Reykjavik
#'
#' @export
#'
#' @param x An FLIndices object
#'
flindices_to_rbya <- function(x) {

  indices <- x@names
  x <- x@.Data

  x2 <- vector("list", length(indices))
  names(x2) <- indices

  for(i in seq_along(indices)) {
    x2[[i]] <- reshape2::melt(x[[i]]@index)[,c("year","age","value")] %>%
      dplyr::mutate(age = ifelse(age == "all",-9,age),
                    sur = indices[i])
  }

  x <- x2 %>% purrr::map_df(tibble::as_data_frame)


  return(x)

}
