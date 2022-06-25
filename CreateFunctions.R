prepareConceptSetName <- function(
  badName
) {
  projNumber <- unlist(strsplit(badName, "_|-"))[1]
  betterName <- unlist(
    regmatches(badName, gregexpr("(?<=_).*", badName, perl=TRUE))
  )
  goodName <- paste(projNumber,
                    rapportools::tocamel(
                      betterName, delim = "[^[:alnum:]]", upper = FALSE, sep = ""
                    ),  sep = "_")
  return(goodName)
}

createConceptSetJsonExpression <- function(undergoingCsv) {
  pathToJson <- "inst/template.json"
  templateJson <- rjson::fromJSON(file = pathToJson)
  lenConceptSet <- nrow(undergoingCsv)
  targetJson <- list()
  targetJson$items <- sapply(1:lenConceptSet, function(it) templateJson$items)
  targetJsonToPut <- targetJson[!names(targetJson) %in% "items" == FALSE]
  for(i in 1:lenConceptSet) {
    targetJsonToPut$items[[i]]$includeDescendants <- ifelse(
      undergoingCsv$include_descendants[[i]] == 1 &
        undergoingCsv$standard_concept[[i]] %in% c("C", "S"), TRUE, FALSE
    )
    targetJsonToPut$items[[i]]$isExcluded <- ifelse(
      undergoingCsv$criteria[[i]] == "inclusion", FALSE, TRUE
    )
    targetJsonToPut$items[[i]]$includeMapped <- ifelse(
      undergoingCsv$mapped[[i]] == 0, FALSE, TRUE
    )
    targetJsonToPut$items[[i]]$concept$CONCEPT_CLASS_ID <- undergoingCsv$concept_class_id[[i]]
    targetJsonToPut$items[[i]]$concept$CONCEPT_CODE <- undergoingCsv$concept_code[[i]]
    targetJsonToPut$items[[i]]$concept$CONCEPT_ID <- undergoingCsv$concept_id[[i]]
    targetJsonToPut$items[[i]]$concept$CONCEPT_NAME <- undergoingCsv$concept_name[[i]]
    targetJsonToPut$items[[i]]$concept$DOMAIN_ID <- undergoingCsv$domain_id[[i]]
    targetJsonToPut$items[[i]]$concept$VOCABULARY_ID <- undergoingCsv$vocabulary_id[[i]]
    targetJsonToPut$items[[i]]$concept$STANDARD_CONCEPT_CAPTION <- data.table::fcase(
      undergoingCsv$standard_concept[[i]] == "S", "Standard",
      undergoingCsv$standard_concept[[i]] == "C", "Classification",
      undergoingCsv$standard_concept[[i]] == "", "Non-Standard"
    )
    targetJsonToPut$items[[i]]$concept$STANDARD_CONCEPT <- data.table::fcase(
      undergoingCsv$standard_concept[[i]] == "S", "S",
      undergoingCsv$standard_concept[[i]] == "C", "C",
      undergoingCsv$standard_concept[[i]] == "", "N"
    )
  }
  return(
    RJSONIO::toJSON(targetJsonToPut, pretty = TRUE)
  )
}


putCsvInFolder <- function (
  ) {
  pathToCsv <- paste0(
    "inst",
    "/concept_phenotypes.csv"
  )
  pathToCsvStatus <- paste0(
    "inst",
    "/phenotyping_status.csv"
  )
  pheno <- read.csv(pathToCsv)
  phenoStatus <- read.csv(pathToCsvStatus)
  folderNames <- unique(pheno$phenotype)
  for (folder in folderNames) {
    folder_ <- paste0("CS00", prepareConceptSetName(folder))
    dir_ <- paste0(getwd(), "/", folder_)
    dir.create(path = dir_)
    forMdWrite <- subset(phenoStatus, phenotype == folder)
    label <- ifelse(forMdWrite$validatedByRealData == "validated", 'Validated-green', 'Unknown-red')
    medicalAnalyst <- forMdWrite$medicalAnalyst
    labelingStr <- glue::glue('<img src="https://img.shields.io/badge/Status-{label}.svg" alt="Status: Validation"/><br />')
    Name <- gsub(".*_", "", folder_)
    projNumber <- unlist(strsplit(folder, "_|-"))[1]
    nameStr <- glue::glue('<ul> Project Number: <em><strong>{projNumber}</em></strong><br />')
    headerName <- paste(
      stringr::str_to_title(
        unlist(strsplit(gsub("([[:upper:]])", " \\1", Name), " "))),
      collapse =  " "
    )
    headerNameStr <- glue::glue('<h2>{headerName}</h2> <br/>')
    medicalAnalistStr <- glue::glue('\n-\t\tMedical Analyst: <em><strong>{medicalAnalyst}</em></strong> <br />')
    usedBy <- 'alexander.alexeyuk@odysseusinc.com'
    usedByStr <- glue::glue('-\t\tConcept Set Used By: <em><strong>{usedBy}</em></strong> <br />')
    uploadDate <- paste0(strsplit(date(), " ")[[1]][c(3, 2, 5)], collapse = "-")
    uploadDateStr <- glue::glue('\n-\t\tUpload Date: <em><strong>{uploadDate}</em></strong> <br />')
    CdmVersionStr <- glue::glue('\n-\t\tCDM Version: <em><strong>5</em></strong> <br />')
    atlasLink <- ""
    atlasLinkStr <- glue::glue('\n-\t\tATLAS Link:  <em><strong>{atlasLink}</em></strong> </ul>')
    notes <- forMdWrite$definition
    # <h3 id="1-this-is-my-header">1. This is my Header</h3>
    notesStr <- glue::glue('<h3>Notes</h3> <br/>{notes} ')
    comments <- forMdWrite$comments
    commentStr <- glue::glue('<h3>Comments</h3> <br/>{comments} ')
    mdWrite <- paste0(
      headerNameStr,
      labelingStr, nameStr, medicalAnalistStr, usedByStr, uploadDateStr, CdmVersionStr,
      atlasLinkStr, notesStr, commentStr
    )
     write(mdWrite, paste0(folder_, "/", 'META.md'))
    toWrite <- subset(pheno, phenotype == folder)
    write.csv(toWrite, paste0(folder_, "/", folder_, '.csv'))
    jsonToWrite <- createConceptSetJsonExpression(toWrite)
    write(jsonToWrite, paste0(folder_, "/", folder_, '.json'))
  }
}
putCsvInFolder()



prepareConceptSetName(unique(concept_phenotypes$phenotype)[1])
t <- unique(concept_phenotypes$phenotype)[1]
createFolders(folderNames = c('asd',
                'fsad'))
phenotyping_status <- read.csv("inst/phenotyping_status.csv")
