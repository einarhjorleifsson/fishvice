#  Problem The ICES System does not accept mat in rba

#' @title standard_graph_xml
#'
#' @description Converts data.frame to ices xml format for standard graphs. The
#' function is a brute force coding and does not have any error checking on the
#' input data.
#'
#' @export
#'
#' @param rby A data frame containing stock summary data. Column names
#' besides the Year column which is required can be any of the following:
#' \itemize{
#'  \item \emph{Year} - Year
#'  \item \emph{Low_Recruitment} - XX
#'  \item \emph{Recruitment} - XXX
#'  \item \emph{High_Recruitment} - XXX
#'  \item \emph{Low_TBiomass} - XXX
#'  \item \emph{TBiomass} - XXX
#'  \item \emph{High_TBiomass} - XXX
#'  \item \emph{Low_SSB} - XXX
#'  \item \emph{SSB} - XXX
#'  \item \emph{High_SSB} - XXX
#'  \item \emph{Catches} - XXX
#'  \item \emph{Landings} - XXX
#'  \item \emph{Discards} - XXX
#'  \item \emph{IBC} - XXX
#'  \item \emph{Unallocated_Removals} - XXX
#'  \item \emph{YieldSSB} - XXX
#'  \item \emph{Low_F} - XXX
#'  \item \emph{F} - XXX
#'  \item \emph{High_F} - XXX
#'  \item \emph{F_Landings} - XXX
#'  \item \emph{F_Discards} - XXX
#'  \item \emph{F_IBC} - XXX
#'  \item \emph{F_Unallocated} - XXX
#'  \item \emph{SoP} - XXX
#'  \item \emph{Custom1} - XXX
#'  \item \emph{Custom2} - XXX
#'  \item \emph{Custom3} - XXX
#'  \item \emph{Custom4} - XXX
#'  \item \emph{Custom5} - XXX
#' }
#' @param FishStock A character vector containing stock "name". e.g. "cod-farp", "cod-iceg".
#' @param AssessmentYear An integer representing assessment year
#' @param RecruitmentAge An integer representing recruitment age
#' @param FAge A character vector containg reference fishing moralites, e.g. "F5-10"
#' @param rba \code{data.frame} containing yield per recruit input. If missing it is
#' ignored. If supplied required column names are:
#' \itemize{
#'  \item \emph{Age} - age
#'  \item \emph{M} - natural mortality
#'  \item \emph{Mat} - maturity
#'  \item \emph{PF} - partial fishing mortality before spawning
#'  \item \emph{PM} - partial natural mortality before spawning
#'  \item \emph{WeSt} - spawning stock weights
#'  \item \emph{F} - selection pattern
#'  \item \emph{WeCa} - catch weights
#'  \item \emph{Fd} - selection pattern of discards
#'  \item \emph{WeCad} - discard catch weights
#'  \item \emph{Fi} - selection pattern of industrial waste
#'  \item \emph{WeCai} - industrial catch weights
#' }
#' @param Flim A numeric value
#' @param Fpa A numeric value
#' @param Blim A numeric value
#' @param Bpa A numeric value
#' @param FMSY A numeric value
#' @param MSYBtrigger A numeric value
#' @param Fmanagement A numeric value
#' @param Bmanagement A numeric value
#' @param RecruitmentLength A numeric value
#' @param UnitsWeigths A character, expect any one of "Kilograms", "Tonnes",
#' "Thousand tonnes", "Million tonnes"
#' @param UnitsRecruits A character, expect any one of "Thousands", "Millions",
#' "Billions"
#' @param TypeLandings A character, e.g. "official", "ices estimates".
#' @param Custom1 XXX
#' @param Custom2 XXX
#' @param Custom3 XXX
#' @param Custom4 XXX
#' @param Custom5 XXX
#' @param VersionStock XXX
#' @param NameSystemProducedFile XXX
#'
standard_graph_xml <- function(rby,FishStock,AssessmentYear,RecruitmentAge,FAge,rba,UnitsWeigths="tonnes",
                               UnitsRecruits="millions",TypeLandings="official",
                               Flim,Fpa,Blim,Bpa,FMSY,MSYBtrigger,
                               Fmanagement,Bmanagement,RecruitmentLength,
                               Custom1,Custom2,Custom3,
                               Custom4,Custom5,
                               VersionStock,NameSystemProducedFile)
{

  # Header data:
  h_xml <- "<?xml version='1.0' encoding='utf-8' standalone='no'?>\n"
  h_xml <- paste0(h_xml,"<?xml-stylesheet type='text/xsl' href='StandrdGraphsStyle.xsl'?>\n")
  h_xml <- paste0(h_xml,"<Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='ICES_Standard_Graphs.xsd'>\n")
  h_xml <- paste0(h_xml,"<FishStock>",FishStock,"</FishStock>\n")
  h_xml <- paste0(h_xml,"<AssessmentYear>",AssessmentYear,"</AssessmentYear>\n")
  if(!missing(Flim)) h_xml <- paste0(h_xml,"<Flim>",Flim,"</Flim>\n")
  if(!missing(Fpa))  h_xml <- paste0(h_xml,"<Fpa>",Fpa,"</Fpa>\n")
  if(!missing(Blim)) h_xml <- paste0(h_xml,"<Blim>",Blim,"</Blim>\n")
  if(!missing(Bpa))  h_xml <-  paste0(h_xml,"<Bpa>",Bpa,"</Bpa>\n")
  if(!missing(FMSY)) h_xml <- paste0(h_xml,"<FMSY>",FMSY,"</FMSY>\n")
  if(!missing(MSYBtrigger))    h_xml <- paste0(h_xml,"<MSYBtrigger>",MSYBtrigger,"</MSYBtrigger>\n")
  if(!missing(Fmanagement))    h_xml <- paste0(h_xml,"<Fmanagement>",Fmanagement,"</Fmanagement>\n")
  if(!missing(Bmanagement))    h_xml <- paste0(h_xml,"<Bmanagement>",Bmanagement,"</Bmanagement>\n")
  if(!missing(RecruitmentAge)) h_xml <- paste0(h_xml,"<RecruitmentAge>",RecruitmentAge,"</RecruitmentAge>\n")
  if(!missing(FAge))           h_xml <- paste0(h_xml,"<FAge>",FAge,"</FAge>\n")
  if(!missing(RecruitmentLength)) h_xml <- paste0(h_xml,"<RecruitmentLength>",RecruitmentLength,"</RecruitmentLength>\n")
  if(!missing(UnitsWeigths))   h_xml <- paste0(h_xml,"<UnitsWeigths>",UnitsWeigths,"</UnitsWeigths>\n")
  if(!missing(UnitsRecruits))  h_xml <- paste0(h_xml,"<UnitsRecruits>",UnitsRecruits,"</UnitsRecruits>\n")
  if(!missing(TypeLandings))   h_xml <- paste0(h_xml,"<TypeLandings>",TypeLandings,"</TypeLandings>\n")

  rby_names <- names(rby)

  # the year loop
  for (i in 1:nrow(rby)) {
    x <- paste0("<FishData>\n","<Year>",rby$Year[i],"</Year>\n")

    if(any(match(rby_names,"Low_Recruitment"),na.rm=T)) x <- paste0(x,"<Low_Recruitment>",rby$Low_Recruitment[i],"</Low_Recruitment>\n")
    if(any(match(rby_names,"Recruitment"),na.rm=T)) x <- paste0(x,"<Recruitment>",rby$Recruitment[i],"</Recruitment>\n")
    if(any(match(rby_names,"High_Recruitment"),na.rm=T)) x <- paste0(x,"<High_Recruitment>",rby$High_Recruitment[i],"</High_Recruitment>\n")

    if(any(match(rby_names,"Low_TBiomass"),na.rm=T)) x <- paste0(x,"<Low_TBiomass>",rby$Low_TBiomass[i],"</Low_TBiomass>\n")
    if(any(match(rby_names,"TBiomass"),na.rm=T)) x <- paste0(x,"<TBiomass>",rby$TBiomass[i],"</TBiomass>\n")
    if(any(match(rby_names,"High_TBiomass"),na.rm=T)) x <- paste0(x,"<High_TBiomass>",rby$High_TBiomass[i],"</High_TBiomass>\n")

    if(any(match(rby_names,"Low_SSB"),na.rm=T)) x <- paste0(x,"<Low_SSB>",rby$Low_SSB[i],"</Low_SSB>\n")
    if(any(match(rby_names,"SSB"),na.rm=T)) x <- paste0(x,"<SSB>",rby$SSB[i],"</SSB>\n")
    if(any(match(rby_names,"High_SSB"),na.rm=T)) x <- paste0(x,"<High_SSB>",rby$High_SSB[i],"</High_SSB>\n")

    if(i < nrow(rby)) {
      if(any(match(rby_names,"Catches"),na.rm=T)) x <- paste0(x,"<Catches>",rby$Catches[i],"</Catches>\n")
      if(any(match(rby_names,"Landings"),na.rm=T)) x <- paste0(x,"<Landings>",rby$Landings[i],"</Landings>\n")
      if(any(match(rby_names,"Discards"),na.rm=T)) x <- paste0(x,"<Discards>",rby$Discards[i],"</Discards>\n")

      if(any(match(rby_names,"IBC"),na.rm=T)) x <- paste0(x,"<IBC>",rby$IBC[i],"</IBC>\n")
      if(any(match(rby_names,"Unallocated_Removals"),na.rm=T)) x <- paste0(x,"<Unallocated_Removals>",rby$Unallocated_Removals[i],"</Unallocated_Removals>\n")
      if(any(match(rby_names,"YieldSSB"),na.rm=T)) x <- paste0(x,"<YieldSSB>",rby$YieldSSB[i],"</YieldSSB>\n")

      if(any(match(rby_names,"Low_F"),na.rm=T)) x <- paste0(x,"<Low_F>",rby$Low_F[i],"</Low_F>\n")
      if(any(match(rby_names,"F"),na.rm=T)) x <- paste0(x,"<F>",rby$F[i],"</F>\n")
      if(any(match(rby_names,"High_F"),na.rm=T)) x <- paste0(x,"<High_F>",rby$High_F[i],"</High_F>\n")

      if(any(match(rby_names,"F_Landings"),na.rm=T)) x <- paste0(x,"<F_Landings>",rby$F_Landings[i],"</F_Landings>\n")
      if(any(match(rby_names,"F_Discards"),na.rm=T)) x <- paste0(x,"<F_Discards>",rby$F_Discards[i],"</F_Discards>\n")
      if(any(match(rby_names,"F_IBC"),na.rm=T)) x <- paste0(x,"<F_IBC>",rby$F_IBC[i],"</F_IBC>\n")
      if(any(match(rby_names,"F_Unallocated"),na.rm=T)) x <- paste0(x,"<F_Unallocated>",rby$F_Unallocated[i],"</F_Unallocated>\n")
      if(any(match(rby_names,"SoP"),na.rm=T)) x <- paste0(x,"<SoP>",rby$SoP[i],"</SoP>\n")
    }

    if(!missing(Custom1))
      if(any(match(rby_names,Custom1),na.rm=T)) x <- paste0(x,"<Custom1>",rby[i,Custom1],"</Custom1>\n")
      if(!missing(Custom2))
        if(any(match(rby_names,Custom2),na.rm=T)) x <- paste0(x,"<Custom2>",rby[i,Custom2],"</Custom2>\n")
        if(!missing(Custom3))
          if(any(match(rby_names,Custom3),na.rm=T)) x <- paste0(x,"<Custom3>",rby[i,Custom3],"</Custom3>\n")
          if(!missing(Custom4))
            if(any(match(rby_names,Custom4),na.rm=T)) x <- paste0(x,"<Custom4>",rby[i,Custom4],"</Custom4>\n")
            if(!missing(Custom5))
              if(any(match(rby_names,Custom5),na.rm=T)) x <- paste0(x,"<Custom5>",rby[i,Custom5],"</Custom5>\n")

              x <- paste0(x,"</FishData>\n")
              if (i == 1) {
                rby_xml <- x
              } else {
                rby_xml <- paste0(rby_xml,x)
              }
  }

  ## Yield per recruit input
  if(missing(rba)) {
    return(paste0(h_xml,rby_xml,"</Assessment>\n"))
  } else {
    rba_names <- names(rba)
    # the age loop
    for (i in 1:nrow(rba)) {
      x <- "<Sensitivity_Data>"
      x <- paste0(x,"<Age>",rba$Age[i],"</Age>\n")
      x <- paste0(x,"<M>",rba$M[i],"</M>\n")
      x <- paste0(x,"<Mat>",rba$Mat[i],"</Mat>\n")
      x <- paste0(x,"<PF>",rba$PF[i],"</PF>\n")
      x <- paste0(x,"<PM>",rba$PM[i],"</PM>\n")
      x <- paste0(x,"<WeSt>",rba$WeSt[i],"</WeSt>\n")
      x <- paste0(x,"<F>",rba$F[i],"</F>\n")
      x <- paste0(x,"<WeCa>",rba$WeCa[i],"</WeCa>\n")
      if(any(match(rba_names,"Fd"),na.rm=T)) x <- paste0(x,"<Fd>",rba$Fd[i],"</Fd>\n")
      if(any(match(rba_names,"WeCad"),na.rm=T)) x <- paste0(x,"<WeCad>",rba$WeCad[i],"</WeCad>\n")
      if(any(match(rba_names,"Fi"),na.rm=T)) x <- paste0(x,"<Fi>",rba$Fi[i],"</Fi>\n")
      if(any(match(rba_names,"WeCai"),na.rm=T)) x <- paste0(x,"<WeCai>",rba$WeCai[i],"</WeCai>\n")
      x <- paste0(x,"</Sensitivity_Data>\n")
      if (i == 1) {
        rba_xml <- x
      } else {
        rba_xml <- paste0(rba_xml,x)
      }
    }
    return(paste0(h_xml,rby_xml,rba_xml,"</Assessment>\n"))
  }
}


