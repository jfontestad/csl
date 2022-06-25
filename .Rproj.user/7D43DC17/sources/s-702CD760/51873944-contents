#' @export
#'
getAtlasJson <- function(
  baseUrl,
  authMethod,
  webApiUsername,
  webApiPassword,
  conceptSetId
) {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  ROhdsiWebApi::authorizeWebApi(
    baseUrl = baseUrl,
    authMethod = authMethod,
    webApiUsername = webApiUsername,
    webApiPassword = webApiPassword
  )

  conceptSetJsonRepresentation <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = conceptSetId,
    baseUrl = baseUrl
  )
  return(conceptSetJsonRepresentation$expression)
}


#' @export
#'
jsonToCsv <- function(
  conceptSetJsonRepresentation,
  phenotypeName
) {
  targetJson <- conceptSetJsonRepresentation
  target <- list()
  targetDataFrame <- purrr::map_dfr(1:length(targetJson$items), function(i) {
    target$phenotype <- phenotypeName
    target$include_descendants <-
      ifelse(targetJson$items[[i]]$includeDescendants == TRUE, 1, 0)
    target$criteria <-
      ifelse(targetJson$items[[i]]$isExcluded == TRUE, 'exclusion', 'inclusion')
    target$mapped <- ifelse(targetJson$items[[i]]$includeMapped == TRUE, 1, 0)
    target$concept_class_id <-  targetJson$items[[i]]$concept$CONCEPT_CLASS_ID
    target$concept_code <-  targetJson$items[[i]]$concept$CONCEPT_CODE
    target$concept_id <-  targetJson$items[[i]]$concept$CONCEPT_ID
    target$concept_name <-  targetJson$items[[i]]$concept$CONCEPT_NAME
    target$domain_id <-  targetJson$items[[i]]$concept$CONCEPT_CLASS_ID
    target$standard_concept <-  targetJson$items[[i]]$concept$CONCEPT_CLASS_ID
    target$vocabulary_id <- targetJson$items[[i]]$concept$VOCABULARY_ID
    target$standard_concept <- data.table::fcase(
      targetJson$items[[i]]$concept$STANDARD_CONCEPT_CAPTION == "Standard", "S",
      targetJson$items[[i]]$concept$STANDARD_CONCEPT_CAPTION == "Classification", "C",
      targetJson$items[[i]]$concept$STANDARD_CONCEPT_CAPTION == "Non-Standard", ""
    )
    target$valid_start_date <- '0000-00-00'
    target$valid_end_date <- '0000-00-00'
    target$invalid_reason <- ''
    return(target)
  })
  return(
    data.frame(targetDataFrame)
  )
}


#' @export
#'
createNameFromConceptName <- function(
  sourceId,
  createMdFile = TRUE,
  analyst,
  jsons,
  validated = FALSE,
  usedBy = 'alexander.alexeyuk@odysseusinc.com'
) {
  for(json in jsons) {
    conceptSetJsonRepresentation <- rjson::fromJSON(file = json)
    name <- gsub('_NA', '', paste(
      "CS00",
      sourceId,
      paste(
        gsub('/', '_', unlist(
          strsplit(
            conceptSetJsonRepresentation$expression$items[[1]]$concept$CONCEPT_NAME, ' '
            )
          )[1:4]),
        collapse = '_'),
      sep = '_')
    )
    csvToPut <- csl::jsonToCsv(
      phenotypeName = name,
      conceptSetJsonRepresentation = conceptSetJsonRepresentation$expression
          )
    dir_ <- paste0(getwd(), "/", name)
    dir.create(path = dir_)
    data.table::fwrite(csvToPut, paste0(dir_, "/", name, '.csv'))
    write(rjson::toJSON(conceptSetJsonRepresentation,
                        indent = 2), paste0(dir_, "/", name, '.json'))

    label <- ifelse(validated == TRUE, 'Validated-green', 'Unknown-red')
    medicalAnalyst <- analyst
    labelingStr <- glue::glue('<img src="https://img.shields.io/badge/Status-{label}.svg" alt="Status: Validation"/><br />')
    Name <- gsub(".*_", "", name)
    projNumber <- unlist(strsplit(name, "_|-"))[1]
    nameStr <- glue::glue('<ul> Project Number: <em><strong>{projNumber}</em></strong><br />')
    headerName <- paste(
      stringr::str_to_title(
        unlist(strsplit(gsub("([[:upper:]])", " \\1", Name), " "))),
      collapse =  " "
    )
    headerNameStr <- glue::glue('<h2>{headerName}</h2> <br/>')
    medicalAnalistStr <- glue::glue('\n-\t\tMedical Analyst: <em><strong>{medicalAnalyst}</em></strong> <br />')
    usedBy <- usedBy
    usedByStr <- glue::glue('-\t\tConcept Set Used By: <em><strong>{usedBy}</em></strong> <br />')
    uploadDate <- paste0(strsplit(date(), " ")[[1]][c(3, 2, 5)], collapse = "-")
    uploadDateStr <- glue::glue('\n-\t\tUpload Date: <em><strong>{uploadDate}</em></strong> <br />')
    CdmVersionStr <- glue::glue('\n-\t\tCDM Version: <em><strong>5</em></strong> <br />')
    atlasLink <- ""
    atlasLinkStr <- glue::glue('\n-\t\tATLAS Link:  <em><strong>{atlasLink}</em></strong> </ul>')
    #notes <- forMdWrite$definition
    notesStr <- glue::glue('<h3>Notes</h3> <br/>')
    # comments <- forMdWrite$comments
    commentStr <- glue::glue('<h3>Comments</h3> <br/>')
    mdWrite <- paste0(
      headerNameStr,
      labelingStr, nameStr, medicalAnalistStr, usedByStr, uploadDateStr, CdmVersionStr,
      atlasLinkStr, notesStr, commentStr
    )
    write(mdWrite, paste0(dir_, "/", 'META.md'))
  }
}
