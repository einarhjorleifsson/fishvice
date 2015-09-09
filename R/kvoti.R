# ----------------------------------------------------------
# kvotascriptur

#' @title Upplýsingar um kvótastöðu
#'
#' @description XXX
#'
#' @export
#'
#' @param sid Tegundarnúmer
#'
kvota_stada <- function(sid = c(1,2,3,5,6,7,8,9,11,22,23,24,25,27,28,30,31,34,36,40,41,61))
  {

  #mar <- dplyrOracle::src_oracle("mar")
  mar <- 0

  # --------------------------------------------------------
  # species list
  teg <- tbl(mar,sql("orri.fisktegundir")) %>%
    select(TEGUND, HEITI) %>%
    filter(TEGUND %in% sid) %>%
    collect() %>%
    rename(sid = TEGUND,tegund=HEITI) %>%
    mutate(tegund = ifelse(sid==5,"Gullkarfi",tegund),
      tegund = ifelse(sid==24, "Þykkvalúra", tegund),
      tegund = ifelse(sid==40, "Humar", tegund)) %>%
    arrange(sid)

  # --------------------------------------------------------
  # convertion factor
  studlar <- tbl(mar,sql("kvoti.studlar")) %>%
    select(TIMABIL, FTEGUND, I_OSLAEGT) %>%
    collect() %>%
    rename(FTEG = FTEGUND, r = I_OSLAEGT)

  # --------------------------------------------------------
  x <- tbl(mar, sql("kvoti.kv_stada")) %>%
    filter(FTEG %in% sid) %>%
    group_by(FTEG, TIMABIL) %>%
    summarise(varanlegt = sum(VARANLEGT),
      jofnsj = sum(JOFNSJ),
      m_ara  = sum(M_ARA),
      kvoti  = sum(KVOTI),
      afli   = sum(AFLI),
      stada  = sum(STADA),
      tilf   = sum(TILF),
      eftir  = sum(EFTIR),
      upptaka = sum(UPPTAKA),
      n_ar    = sum(N_AR),
      onotad  = sum(ONOTAD))  %>%
    collect() %>%
    reshape2::melt(c("TIMABIL","FTEG")) %>%
    left_join(studlar, by = c("TIMABIL","FTEG")) %>%
    mutate(value = round(value * r /1e3,0)) %>%
    reshape2::dcast(TIMABIL + FTEG ~ variable, value.var = "value") %>%
    mutate(TIMABIL = ifelse(stringr::str_sub(TIMABIL,1,1) %in% "9",
      paste0(1900+as.integer(stringr::str_sub(TIMABIL,1,2)),"/",stringr::str_sub(TIMABIL,3)),
      paste0(2000+as.integer(stringr::str_sub(TIMABIL,1,2)),"/",stringr::str_sub(TIMABIL,3)))) %>%
    rename(timabil = TIMABIL, sid=FTEG) %>%
    left_join(teg, by = "sid") %>%
    arrange(sid, timabil) %>%
    select(tegund, timabil, varanlegt, jofnsj, m_ara, kvoti, afli, stada,
      tilf, eftir, upptaka, n_ar, onotad)

  return(x)
}
