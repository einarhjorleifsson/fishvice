#' @title Converts FLStock object to rbya
#'
#' @description The rbya (results by year and age) is a long \code{data.frame}
#' popular in Reykjavik
#'
#' @export
#'
#' @param x An FLStock object
#' @param scale scaler used on abundance values (stock in numbers, catches etc)

FLStock_to_rbya <- function(x, scale=1)
{

  y <- reshape2::melt(FLCore::stock.n(x),value.name = "n")[,c("year","age","n")]
  y$n <- y$n/scale
  y$f <- reshape2::melt(FLCore::harvest(x))[,c("value")]
  # if(class(x) != "FLSAM") {  # This may be needed
  y$oC <- reshape2::melt(FLCore::catch.n(x))[,c("value")]/scale
  y$cW <- reshape2::melt(FLCore::catch.wt(x))[,c("value")]
  y$ssbW <- reshape2::melt(FLCore::stock.wt(x))[,c("value")]
  y$oD  = reshape2::melt(FLCore::discards.n(x))[,c("value")]/scale
  y$dW  = reshape2::melt(FLCore::discards.wt(x))[,c("value")]
  y$oL  = reshape2::melt(FLCore::landings.n(x))[,c("value")]/scale
  y$lW  = reshape2::melt(FLCore::landings.wt(x))[,c("value")]
  y$mat = reshape2::melt(FLCore::mat(x))[,c("value")]
  y$pF  = reshape2::melt(FLCore::harvest.spwn(x))[,c("value")]
  y$pM  = reshape2::melt(FLCore::m.spwn(x))[,c("value")]
  y$m   = reshape2::melt(FLCore::m(x))[,c("value")]
  return(y)

}
