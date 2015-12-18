# These are functions pertaining to getting annotation, converting names for an R-usable format, and displaying parameters of interest

#' Get annotations for a set of FCS files
#'
#' If some files are not annotated, it will return a warning and only return the fcs files and annotations only
#' @param FCS_files object containing FCS files from get_FCS_files()
#' @export


get_annotations <- function(FCS_files){

  if (sum(sapply(FCS_files$annotations, nrow) == 0) != 0){
    FCS_files <- FCS_files[sapply(FCS_files$annotations, nrow) != 0, ]
    print("Warning: missing annotations in experiment, returning truncated file frame")
  } else {}

  annotations <- t(sapply(FCS_files$annotations, "[[", "value"))
  colnames(annotations) <- FCS_files$annotations[[1]]$type

  return(data.frame(file_ID = FCS_files$`_id`, filename = FCS_files$filename, annotations))
}

#' Display populations and reagents
#'
#' Provides a list of population names, channel names, and reagent names that can be used to select which are desired features
#' for subsequent analysis
#' @param FCS_files Object containing FCS files
#' @param experimentID the experiment ID as a string
#' @param access_key An object containing server URL and authentication info: see "authenticate"
#' @export

display_parameters <- function(FCS_files, experimentID, access_key){

  populations <- get_populations(experimentID, access_key)$name

  get_reagents <- function(FCS_files){
    FCS_files$panel[[1]]$reagent
  }
  reagents <- get_reagents(FCS_files)

  get_channels <- function(FCS_files){
    FCS_files$panel[[1]]$channel
  }
  channels <- get_channels(FCS_files)

  parameters <- list(populations, channels, reagents)
  names(parameters) <- c("populations", "channels", "reagents")
  return(parameters)
}

#' Get channel names from reagent names
#'
#' Returns a vector of channel names that corresponds to the reagent names from an FCS_files set (see get FCS_files)
#' @param reagent_names a vector of reagent names
#' @param FCS_files a set of FCS files from get_FCS_files()
#' @examples
#' reagent_names <- c("pSTAT1", "pSTAT3", "pMAPKAPK2")
#' FCS_files <- get_FCS_files(experimentID, access_key)
#' channel_names <- convert_reagents_to_channels(reagent_names, FCS_files)
#' @export
#'

convert_reagents_to_channels <- function(reagent_names, FCS_files){

  df <- FCS_files$panel[[1]]
  channel_names <- df[df$reagent %in% reagent_names, "channel"]
  return(channel_names)

}

#' Convert names
#'
#' Convert names for compatibility in R, removing promblematic characters such as "+" of spaces
#' @param x A vector of names
#' @export

convert_names <- function(x){
  new_names <- make.names(x)
}

