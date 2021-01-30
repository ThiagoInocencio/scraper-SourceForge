# ----------------------------------------------------------------------------
# |
# | Utilitary function to create a path.
# |
# ----------------------------------------------------------------------------
createPath <- function (strPath = "") {

  if (!file.exists('./files')){
    dir.create(file.path('./files'))
  }

  path <- sprintf('%s/%s' , './files', strPath)

  if (!file.exists(path) && strPath != ""){
    dir.create(file.path(path))
  }

  path
}

# ----------------------------------------------------------------------------
# |
# | Function to load a HTML file verifying if its was already downloaded.
# |
# ----------------------------------------------------------------------------
getHTML <- function (url, folder = "", archive_name = '') {

  path <- createPath(folder)

  if(archive_name == '')
    archive_name <- url %>%  stringr::str_extract('[^=]*$')
  else
    archive_name <- archive_name

  arq <- sprintf('%s/%s.html' , path, archive_name)

  if(file.exists(arq)) {

    # Getting page from local folder
    codHTML <- xml2::read_html(arq)

  } else {

    # Getting page from url
    if(folder != "")
      httr::GET(url, httr::write_disk(arq, overwrite = TRUE))

    codHTML <-  url %>%
      httr::GET() %>%
      httr::content('text', encoding = 'latin1') %>%
      xml2::read_html()

  }
}

# ----------------------------------------------------------------------------
# |
# | Getting system data from system's URL
# |
# ----------------------------------------------------------------------------
getSystemInfo <- function (sysName, url) {

  cat("Extracting data of ");
  cat(sysName);
  cat("\n");

  sysName <- (gsub("/", "", sysName))
  sysName <- (gsub("projects", "", sysName))

  codeHTML <- getHTML(url, "systems", (gsub("/", "-", sysName)))

  systName <- sysName

  # Variables
  systName <- codeHTML %>%
    rvest::html_nodes('div.title h1') %>%
    xml2::xml_text()

  systemDescription <- codeHTML %>%
    rvest::html_nodes('h3.summary') %>%
    xml2::xml_text()

  systemDescription <- (gsub("\n", "", systemDescription))

  status <- codeHTML %>%
    rvest::html_node('span.status-value') %>%
    xml2::xml_text()

  operationSystems <- tolower(codeHTML %>%
                                rvest::html_nodes('div.platforms span') %>%
                                xml2::xml_text())

  reviews <- codeHTML %>%
    rvest::html_nodes('div.rating a') %>%
    xml2::xml_text()

  category <- codeHTML %>%
    rvest::html_nodes('[itemprop=applicationCategory]') %>%
    xml2::xml_text()

  #category <- paste(category,collapse="/")
  category <- (gsub("/", "-", category))
  category <- (gsub(" ", "-", category))
  category <- tolower(category)

  rating <- codeHTML %>%
    rvest::html_nodes('[itemprop=ratingValue]') %>%
    xml2::xml_text()

  translations <- codeHTML %>%
    rvest::html_nodes('[itemprop=inLanguage]') %>%
    xml2::xml_text()

  for (translat in translations){
    translat <- (gsub("/", "-", translat))
    translat <- (gsub(" ", "-", translat))
    translat <- tolower(translat)
  }

  lastUpdate <- codeHTML %>%
    rvest::html_nodes('.dateUpdated') %>%
    xml2::xml_text()

  downloads <- "NA"

  download_content <- codeHTML %>%
    rvest::html_nodes('div.stats h2 a')

  for (content in download_content){

    if(grepl("Downloads", content, fixed=TRUE)) {
      downloads <- content %>%  xml2::xml_text()
    }
  }

  license <- c()
  audience <- c()
  programming_language <- c()
  user_interface <- c()

  projectInfo <- codeHTML %>%
    rvest::html_nodes('section.project-info a')

  for (content in projectInfo){

    if(grepl("license", content, fixed=TRUE)) {

      licen <- content %>%  xml2::xml_text()
      licen <- (gsub("/", "-", licen))
      licen <- (gsub(" ", "-", licen))
      licen <- tolower(licen)


      if(length(license) == 0)
        license <- licen
      else
        license <- c(license, (licen))
    }

    # programming languages
    if(grepl("language", content, fixed=TRUE)) {

      prog_lan <- content %>%  xml2::xml_text()
      prog_lan <- (gsub("/", "-", prog_lan))
      prog_lan <- (gsub(" ", "-", prog_lan))
      prog_lan <- tolower(prog_lan)

      translations_obj <- paste(translations, collapse="/")

      if(length(programming_language) == 0) {
        p_language <- prog_lan
        if(p_language %nin% translations_obj)
          programming_language <- p_language
      }
      else
        p_language <- prog_lan
      if(p_language %nin% translations_obj) {
        programming_language <- c(programming_language, p_language)
      }
    }

    if(grepl("audience", content, fixed=TRUE)) {
      audi <- content %>%  xml2::xml_text()
      audi <- (gsub("/", "-", audi))
      audi <- (gsub(" ", "-", audi))
      audi <- tolower(audi)

      if(length(audience) == 0)
        audience <- audi
      else
        audience <- c(audience, audi)
    }

    #user interface
    if(grepl("environment", content, fixed=TRUE)) {

      us_in <- content %>%  xml2::xml_text()
      us_in <- (gsub("/", "-", us_in))
      us_in <- (gsub(" ", "-", us_in))
      us_in <- tolower(us_in)

      if(length(user_interface) == 0)
        user_interface <- us_in
      else
        user_interface <- c(user_interface, (us_in))
    }
  }

  # creating the data frame
  data <- data.frame(
    #"SystemURL" = systemUrl,
    "SystemName" = systName,
    #"SystemDesc" = systemDescription,
    "status" = status,
    "OperationSystems" = paste(operationSystems,collapse="/"),
    "Audience" = paste(audience,collapse="/"),
    "Category" = paste(category,collapse="/"),
    "Language" = paste(programming_language, collapse="/"),
    "Translations" = paste(translations, collapse="/"),
    "ProgrammingLan" = paste(programming_language, collapse="/"),
    "UserInterface" = paste(user_interface, collapse="/"),
    "LastUpdate" = lastUpdate,
    #"Reviews" = reviews,
    #"Rating" = rating
    "License" = paste(license, collapse="/")
  )
  data
}

# ----------------------------------------------------------------------------
# |
# | Getting all systems names from a given URL
# |
# ----------------------------------------------------------------------------
getSystemByPage <- function (url) {
  codeHTML <- getHTML(url, "pages")
  systems <- codeHTML %>% rvest::html_nodes('[itemprop=itemListElement]') %>% rvest::html_nodes('[itemprop=url]') %>% rvest::html_attr("content")
}
