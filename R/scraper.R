# Program: scraper.R
# Autor: Thiago Inocêncio
# Descrição: Web scraper to extract systems informations from sourceforge.net
# Last update: 30/01/2021

# ----------------------------------------------------------------------------
# |
# | Loading packages
# |
# ----------------------------------------------------------------------------
source("packages.R")
installPackages()
loadPackages()


# ----------------------------------------------------------------------------
# |
# | Loading Useful functions
# |
# ----------------------------------------------------------------------------
source("functions.R")
`%nin%` = Negate(`%in%`)

# ----------------------------------------------------------------------------
# |
# | Scrapper begins here
# |
# ----------------------------------------------------------------------------

# loading some values
source("../.Rprofile")

# ----------------------------------------------------------------------------
# |
# | creates a page URL vector from 1 to pagesCount
# | https://sourceforge.net/directory/?page=1
# | https://sourceforge.net/directory/?page=pagesCount
# |
# ----------------------------------------------------------------------------
pagesURL <- sprintf("https://sourceforge.net/directory/?page=%d", fromPage:toPage)


# ----------------------------------------------------------------------------
# |
# | Gets a vector of systems names. (May take a long time to run depending on the number of systems)
# |
# ----------------------------------------------------------------------------
systems <- c()
for(page in pagesURL) {
  systems <- c(systems, getSystemByPage(page))
}

# creating a data.frame to handle systems data
data <- data.frame()

# ----------------------------------------------------------------------------
# |
# | Load all systems data and insert it to data.frame
# |
# ----------------------------------------------------------------------------
for(system in systems) {
  sysURL <- sprintf("https://sourceforge.net%s", system)

  # If any error occurs it ignore the system and continues
  try(data <- rbind(data, getSystemInfo(system, sysURL)))
}

# ----------------------------------------------------------------------------
# |
# | Finally, it saves all systems data to a .csv file
# |
# ----------------------------------------------------------------------------
write.csv(data,'dataset.csv', row.names = FALSE)
