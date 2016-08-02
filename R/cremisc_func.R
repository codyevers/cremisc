#' Copy tabular data from excel
#'
#' \code{to_excel} copies a data.frame to the clipboard, ready to be copied into excel
#' This function makes it easier to transfer data from R into excel.
#' @export
to_excel <- function(x){
  os <- Sys.info()['sysname']
  if(os == 'Windows') write.table(x, "clipboard-500", sep="\t", row.names=FALSE)
}

#' Copy tabular data from excel
#'
#' \code{from_excel} reads tabular data copied from excel,
#' This function makes it easier to transfer data from excel into R.
#' @export
from_excel <- function(){
  os <- Sys.info()['sysname']
  if(os == 'Windows') x <- read.delim("clipboard")
  return(x)
}


#' Load packages
#'
#' \code{loadPackages} loads packages and installs it if needed.
#' This function is meant to replace the tedious task of loading packages and
#' loading them individual. Here, everything is handled in a single step.
#' @param ... sequence of packages to load
#' @examples
#' loadPackages(ape, xtable)
#' @export
#'
load_packages <- function(...){

  # prepare list of packages to load
  length(match.call())
  pkg_lst <- as.character(match.call())
  pkg_lst <- pkg_lst[-1]

  # show what is installed and loaded
  pkg_df <- data.frame(package=pkg_lst,
                       installed=pkg_lst %in% rownames(installed.packages()),
                       loaded=pkg_lst %in% loadedNamespaces())
  print(pkg_df)

  # check if package is installed
  for(p in pkg_lst){

    isNotInstalled <- suppressWarnings(
      require(p, character.only = TRUE, quietly = TRUE) == FALSE
    )

    if(isNotInstalled == TRUE) {

      message(paste0(p, ' not found; now installing'))
      install.packages(p, dep=TRUE, quiet = TRUE)

      if(require(p, character.only = TRUE, quietly = TRUE)){
        message(paste0(p, ' successfully loaded'))
      } else {
        stop("Package not found")
      }
    }
  }

  # show what is installed and loaded now that function has run
  pkg_df <- data.frame(package=pkg_lst,
                       installed=pkg_lst %in% rownames(installed.packages()),
                       loaded=pkg_lst %in% loadedNamespaces())
  print(pkg_df)
}
