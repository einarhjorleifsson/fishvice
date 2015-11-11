#' @title Get the WoRMS id for a taxonomic name
#'
#' @description Connection to WoRMS webservice \href{http://marinespecies.org/aphia.php?p=soap}{getAphiaID}.
#'
#' @return Returns a numerical value (AphiaID):
#' \itemize{
#'   \item{NULL: when no matches are found}
#'   \item{-999: when multiple matches was found}
#'   \item{an integer: AphiaID when one exact match was found}
#' }
#'
#' @export
#'
#' @param scientificname Linnean type of name

get_worms_id <- function(scientificname = "Gadus morhua") {

  marine_only <- TRUE

  getproducts.xml <-
    paste('
          <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
          <getAphiaID xmlns="http://marinespecies.org/aphia.php?p=soap">
          <scientificname>',scientificname,'</scientificname>
          <marine_only>',marine_only,'</marine_only>
          </getAphiaID>
          </soap:Body>
          </soap:Envelope>',sep = "")


  header.fields <- c('Content-Type' = "text/xml; charset=utf-8",
                     SOAPAction="")
  reader <- RCurl::basicTextGatherer()
  header <- RCurl::basicTextGatherer()
  RCurl::curlPerform(url = "http://marinespecies.org/aphia.php?p=soap",
                     httpheader = header.fields,
                     postfields = getproducts.xml,
                     writefunction = reader$update,
                     verbose = FALSE,
                     cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

  # Check the server is not down by insepcting the XML response for internal server error message.
  ##  This should be moved further up
  if(grepl("Internal Server Error", reader$value())) {
    stop("Web service failure: the server seems to be down, please try again later.")
  }
  res_character <- reader$value()

  i1 <- stringr::str_locate(res_character,"<return xsi:type=\"xsd:int\">")[,2] + 1
  i2 <- stringr::str_locate(res_character,"</return>")[,1] - 1

  res <- as.numeric(stringr::str_sub(res_character,i1,i2))

  return(res)
}

#' @title Get a record from WoRMS for a given species id
#'
#' @description xxx
#'
#' Connection to WoRMS webservice \href{http://marinespecies.org/aphia.php?p=soap}{getAphiaRecordByID}.
#'
#' @export
#'
#'
#'@return The returned AphiaRecord has this format:
#' \itemize{
#'   \item{AphiaID: unique and persistent identifier within WoRMS. Primary key in the database.}
#'   \item{url: HTTP URL to the AphiaRecord}
#'   \item{scientificname: the full scientific name without authorship}
#'   \item{authority: the authorship information for the scientificname formatted according to the conventions of the applicable nomenclaturalCode}
#'   \item{rank: the taxonomic rank of the most specific name in the scientificname}
#'   \item{status: the status of the use of the scientificname (usually a taxonomic opinion). Additional technical statuses are (1) quarantined: hidden from public interface after decision from an editor and (2) deleted: AphiaID should NOT be used anymore, please replace it by the valid_AphiaID}
#'   \item{unacceptreason: the reason why a scientificname is unaccepted}
#'   \item{valid_AphiaID: the AphiaID (for the scientificname) of the currently accepted taxon}
#'   \item{valid_name: the scientificname of the currently accepted taxon}
#'   \item{valid_authority: the authorship information for the scientificname of the currently accepted taxon}
#'   \item{kingdom: the full scientific name of the kingdom in which the taxon is classified}
#'   \item{phylum: the full scientific name of the phylum or division in which the taxon is classified}
#'   \item{class: the full scientific name of the class in which the taxon is classified}
#'   \item{order: the full scientific name of the order in which the taxon is classified}
#'   \item{family: the full scientific name of the family in which the taxon is classified}
#'   \item{genus: the full scientific name of the genus in which the taxon is classified}
#'   \item{citation: a bibliographic reference for the resource as a statement indicating how this record should be cited (attributed) when used}
#'   \item{lsid: LifeScience Identifier. Persistent GUID for an AphiaID}
#'   \item{isMarine: a boolean flag indicating whether the taxon is a marine organism, i.e. can be found in/above sea water. Possible values: 0/1/NULL}
#'   \item{isBrackish: a boolean flag indicating whether the taxon occurrs in brackish habitats. Possible values: 0/1/NULL}
#'   \item{isFreshwater: a boolean flag indicating whether the taxon occurrs in freshwater habitats, i.e. can be found in/above rivers or lakes. Possible values: 0/1/NULL}
#'   \item{isTerrestrial: a boolean flag indicating the taxon is a terrestial organism, i.e. occurrs on land as opposed to the sea. Possible values: 0/1/NULL}
#'   \item{isExtinct: a flag indicating an extinct organism. Possible values: 0/1/NUL}
#'   \item{match_type: Type of match. Possible values: exact/like/phonetic/near_1/near_2}
#'   \item{modified: The most recent date-time in GMT on which the resource was changed}
#' }
#' @param aphiaID An integer value
#' @param long A boolean, if TRUE (default) a long table (21 rows and 2 rows). If
#' FALSE returns a wide table (21 columns and 1 row). The latter form is useful
#' when doing a for-loop.

get_worms_record <- function(aphiaID = 126436, long = TRUE) {

  getproducts.xml <-
    paste('
          <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
          <getAphiaRecordByID xmlns="http://marinespecies.org/aphia.php?p=soap">
          <AphiaID>',aphiaID,'</AphiaID>
          </getAphiaRecordByID>
          </soap:Body>
          </soap:Envelope>',sep = "")


  header.fields <- c('Content-Type' = "text/xml; charset=utf-8",
                     SOAPAction="")
  reader <- RCurl::basicTextGatherer()
  header <- RCurl::basicTextGatherer()
  RCurl::curlPerform(url = "http://marinespecies.org/aphia.php?p=soap",
                     httpheader = header.fields,
                     postfields = getproducts.xml,
                     writefunction = reader$update,
                     verbose = FALSE,
                     cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

  # Check the server is not down by insepcting the XML response for internal server error message.
  ##  This should be moved further up
  if(grepl("Internal Server Error", reader$value())) {
    stop("Web service failure: the server seems to be down, please try again later.")
  }
  res_character <- reader$value()

  # brute force
  i1 <- stringr::str_locate(res_character,"<return")[,1]
  i2 <- stringr::str_locate(res_character,"</return>")[,2]
  x <- stringr::str_sub(res_character,i1,i2)

  cn <- c("scientificname", "author", "rank", "status",
          "valid_AphiaID", "valid_name", "valid_author",
          "Kingdom","Phylum", "Class", "Order", "Family",
          "Genus", "citation", "lsid","isMarine", "isBrackish",
          "isFreshwater", "isTerrestrial", "match_type", "modified",
          "aphiaID")
  df <- data.frame(variable = cn, value = NA)
  #row.names(df) <- cn

  i1 <- stringr::str_locate(x,'<scientificname xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</scientificname>')[,1] - 1
  df[1,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<authority xsi:type=\"xsd:string\">')[,2] +1
  i2 <- stringr::str_locate(x,'</authority>')[,1] - 1
  df[2,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<rank xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</rank>')[,1] - 1
  df[3,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<status xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</status>')[,1] - 1
  df[4,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<valid_AphiaID xsi:type=\"xsd:int\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</valid_AphiaID>')[,1] - 1
  df[5,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<valid_name xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</valid_name>')[,1] - 1
  df[6,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<valid_authority xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</valid_authority>')[,1] - 1
  df[7,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<kingdom xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</kingdom>')[,1] - 1
  df[8,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<phylum xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</phylum>')[,1] - 1
  df[9,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<class xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</class>')[,1] - 1
  df[10,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<order xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</order>')[,1] - 1
  df[11,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<family xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</family>')[,1] - 1
  df[12,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<genus xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</genus>')[,1] - 1
  df[13,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<citation xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</citation>')[,1] - 1
  df[14,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<lsid xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</lsid>')[,1] - 1
  df[15,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<isMarine xsi:type=\"xsd:int\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</isMarine>')[,1] - 1
  df[16,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<isBrackish xsi:type=\"xsd:int\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</isBrackish>')[,1] - 1
  df[17,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<isFreshwater xsi:type=\"xsd:int\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</isFreshwater>')[,1] - 1
  df[18,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<isTerrestrial xsi:type=\"xsd:int\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</isTerrestrial>')[,1] - 1
  df[19,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<match_type xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</match_type>')[,1] - 1
  df[20,2] <- stringr::str_sub(x, i1, i2)

  i1 <- stringr::str_locate(x,'<modified xsi:type=\"xsd:string\">')[,2] + 1
  i2 <- stringr::str_locate(x,'</modified>')[,1] - 1
  df[21,2] <- stringr::str_sub(x, i1, i2)

  df[22,2] <- aphiaID

  if(!long) {
    df <- reshape2::dcast(df, . ~ variable, value.var = "value")
  }

  return(df)
}
