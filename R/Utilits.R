createFolders <- function(
  folderNames
) {
  for (folder in folderNames) {
    dir_ <- paste0(getwd(), "/", folder)
    dir.create(path = dir_)
    write(dir_, paste0(folder, "/", 'META.md'))
  }
}

pcg <- function() {'csl'}


putCsvInFolder <- function (
) {
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
