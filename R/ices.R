#' @title ICES standard graph
#'
#' @description XXX
#'
#' @export
#'
#' @param rby data.frame containing ...
#' @param graph_name Name of the graph to be created
#'
ices_standard_graph <- function(rby,graph_name) {

  my_margins <- rep(0.10,4)

  # dummy
  year <- oY <- r <- value <- variable <- 0

  yield <-
    ggplot2::ggplot(rby,ggplot2::aes(year,oY)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(stat="identity", fill="azure4", col="white") +
    ggplot2::labs(x="",y="kilotonnes",title="Landings") +
    #ggplot2::scale_x_continuous(breaks=seq(1960,2010,by=10)) +
    #ggplot2::scale_y_continuous(breaks=seq(0,500,by=100)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(colour = "transparent"),
                   plot.margin=grid::unit(my_margins,"cm"))

  rec <-
    ggplot2::ggplot(rby,ggplot2::aes(year,r)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(stat="identity", fill="azure4",col="white") +
    ggplot2::labs(x="",y="millions",title="Recruitment") +
    #ggplot2::scale_x_continuous(breaks=seq(1960,2010,by=10)) +
    #ggplot2::scale_y_continuous(breaks=seq(0,400,by=50)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(colour = "transparent"),
                   plot.margin=grid::unit(my_margins,"cm"))

  bios <- rby[,c("year","ssb","bio")]
  names(bios) <- c("year","Spawning stock","Reference stock")
  bios <- reshape2::melt(bios,id.vars = "year")
  bio <-
    ggplot2::ggplot(bios,ggplot2::aes(year,value,linetype=variable)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    #ggplot2::scale_y_continuous(breaks=seq(0,2250,by=250),lim=c(0,max(rby$bio)*1.05),expand=c(0,0)) +
    #ggplot2::geom_hline(yintercept=125,linetype=3) +
    #ggplot2::scale_x_continuous(breaks=seq(1960,2010,by=10)) +
    ggplot2::scale_color_brewer(palette="Set1") +
    ggplot2::labs(x="",y="kilotonnes",title="Spawning stock and reference biomass",linetype="") +
    ggplot2::theme(legend.position=c(0.8,0.87),
                   legend.key=ggplot2::element_rect(colour="white",fill="white"),
                   legend.text=ggplot2::element_text(size=ggplot2::rel(0.7)),
                   legend.title=ggplot2::element_text(size=1),
                   plot.margin=grid::unit(my_margins,"cm"),
                   panel.grid.minor = ggplot2::element_line(colour = "transparent")) +
    ggplot2::expand_limits(y=0)
    #ggplot2::annotate("text",x=1955,y=175, label = "Blim", size=3)

  morts <- rby[,c("year","fbar")]
  names(morts) <- c("year","Fishing mortality")
  morts <- reshape2::melt(morts,id.vars = "year")
  mort <-
    ggplot2::ggplot(morts,ggplot2::aes(year,value,linetype=variable)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    #ggplot2::geom_hline(yintercept=0.20,linetype=3) +
    #ggplot2::scale_x_continuous(breaks=seq(1960,2010,by=10)) +
    #ggplot2::scale_y_continuous(breaks=seq(0,0.9,by=0.1),lim=c(0,max(rby$f)*1.07),expand=c(0,0)) +
    ggplot2::labs(x="",y=" ",title="Fishing mortality and harvest rate",linetype="") +
    ggplot2::theme(legend.position=c(0.87,0.87),
                   legend.key=ggplot2::element_rect(colour="white",fill="white"),
                   legend.margin = grid::unit(0,"cm"),
                   legend.text=ggplot2::element_text(size=ggplot2::rel(0.7)),
                   legend.title=ggplot2::element_text(size=1),
                   plot.margin=grid::unit(my_margins,"cm"),
                   panel.grid.minor = ggplot2::element_line(colour = "transparent")) +
    ggplot2::expand_limits(y=0)
    #ggplot2::annotate("text",x=1965,y=0.22, label = "advisory harvest rate", size=3)

  my_height <- 8
  goldenRatio <- (1+sqrt(5))/2
  if(!missing(graph_name)) pdf(paste0(graph_name,".pdf"),height=my_height,width=goldenRatio*my_height)
  gridExtra::grid.arrange(yield, rec, mort, bio, ncol=2)
  if(!missing(graph_name)) {
    dev.off()
    system(paste0("convert -density 200x200 ",graph_name,".pdf ",graph_name,".png"))
  }
}


#' @title Get a copy of ICES sharepoint directory
#'
#' @description Makes a full copy of a sharepoint directory onto your computer.
#' In theory you can request a full mirror of the whole sharepoint :-)
#' So use at your own discretion!
#'
#' Still only function for some directories
#'
#' @param user_name Only the name after the ices-backslash
#' @param password Your password
#' @param directory The path to the directory you want. I
#'
get_ices_directory <- function(user_name,password,directory)
  {

  cmd <- paste0("wget -r -l4 --no-parent --user='ices\\",user_name)
  cmd <- paste0(cmd,"' --password=")
  cmd <- paste0(cmd,password)
  cmd <- paste0(cmd," 'https://community.ices.dk/")
  cmd <- paste0(cmd,directory,"'")

  system(cmd)

}
