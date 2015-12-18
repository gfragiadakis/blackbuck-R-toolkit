#' Retrieve statistic from a population
#'
#' Returns a desired statistic from a desired feature
#' @param experimentID the experiment ID as a string
#' @param FCS_fileID the FCS file ID as a string
#' @param access_key An object containing server URL and authentication info: see "authenticate"
#' @param channel_name Channel name
#' @param statistic_type Statistic desired, "mean", "median", "quantile", "eventcount
#' @param q required for statistic "quantile", number from 0.0 to 1.0
#' @param populationID Population ID of population of interest
#' @examples
#' access_key <- authenticate("my_username", "my_password",
#'                            baseURL =  "https://www.immuneatlas.org/api/v1")
#' experiments <- get_experiments(access_key)
#' experimentID <- experiments[experiments$name == "experiment_name", '_id']
#' FCS_fileID <- get_FCS_files(experimentID, access_key)[1, "_id"]
#' get_statistic(experimentID, FCS_fileID, access_key, channel_name = "La139Di",
#'                statistic_type = "median", k=NULL, populationID=NULL)
#' @export

get_statistic <- function(experimentID, FCS_fileID, access_key, channel_name, statistic_type, q=NULL, populationID=NULL) {
  opts <- access_key$opts
  baseURL <- access_key$baseURL
  url <- paste(
    paste(baseURL, "experiments", experimentID, "statistics", sep="/"),
    paste(
      paste("fcsFileId", FCS_fileID, sep="="),
      paste("channel", channel_name, sep="="),
      paste("statistic", statistic_type, sep="="),
      paste("q", q, sep="="),
      paste("populationId", populationID, sep="="), sep="&"), sep="?")
  return(fromJSON(getURL(url, .opts = opts)))
}

#' Retrieve statistic URL from a population
#'
#' Returns a desired statistic from a desired feature
#' @param experimentID the experiment ID as a string
#' @param FCS_fileID the FCS file ID as a string
#' @param access_key An object containing server URL and authentication info: see "authenticate"
#' @param channel_name Channel name
#' @param statistic_type Statistic desired, "mean", "median", "quantile", "eventcount
#' @param k required for statistic "quantile", number from 0.0 to 1.0
#' @param populationID Population ID of population of interest
#' @examples
#' access_key <- authenticate("my_username", "my_password",
#'                            baseURL =  "https://www.immuneatlas.org/api/v1")
#' experiments <- get_experiments(access_key)
#' experimentID <- experiments[experiments$name == "experiment_name", '_id']
#' FCS_fileID <- get_FCS_files(experimentID, access_key)[1, "_id"]
#' get_statistic_url(experimentID, FCS_fileID, access_key, channel_name = "La139Di",
#'                statistic_type = "median", k=NULL, populationID=NULL)
#' @export

get_statistic_url <- function(experimentID, FCS_fileID, access_key, channel_name, statistic_type, k=NULL, populationID=NULL) {
  opts <- access_key$opts
  baseURL <- access_key$baseURL
  url <- paste(
    paste(baseURL, "experiments", experimentID, "statistics", sep="/"),
    paste(
      paste("fcsFileId", FCS_fileID, sep="="),
      paste("channel", channel_name, sep="="),
      paste("statistic", statistic_type, sep="="),
      paste("k", k, sep="="),
      paste("populationId", populationID, sep="="), sep="&"), sep="?")
  return(url)
}


#' Make a bulk statistics request
#'
#' Makes a bulk statistics request for a set of FCS files, populations, channels, and statistics types
#' Note limitations may exist on the number of queries in a bulk request (queries = # files x # populations x # channels)
#' @param experimentID the experiment ID as a string
#' @param FCS_fileIDs vector of FCS file IDs as strings
#' @param access_key An object containing server URL and authentication info: see "authenticate"
#' @param channel_names A vector of channel names as strings (note: not reagent names; for conversion of reagents to channels, see function convert_reagents_to_channels())
#' @param statistic_types A vector of statistic types returned including "mean", "median", "quantile", "eventcount"
#' @param q specified quantile (numeric)
#' @param populationIDs vector of population IDs as strings
#' @export

get_bulk_statistics <- function(experimentID, FCS_fileIDs, access_key, channel_names, statistic_types, q=NULL, populationIDs = NULL) {
  opts <- access_key$opts
  baseURL <- access_key$baseURL

  url <- paste(
    paste(baseURL, "experiments", experimentID, "bulkstatistics", sep="/"),
    paste(
      paste("fcsFileIds", toJSON(FCS_fileIDs), sep="="),
      paste("channels", toJSON(channel_names), sep="="),
      paste("statistics", toJSON(statistic_types), sep="="),
      paste("q", q, sep="="),
      paste("populationIds", toJSON(populationIDs), sep="="), sep="&"), sep="?")

  return(fromJSON(getURL(url, .opts = opts)))
}

#' Get set of statistics using bulk requests
#'
#' @param experimentID the experiment ID as a string
#' @param FCS_files FCS files to take statistics from that contains IDs; can be either a data frame (e.g. output from get_FCS_files) or a vector of IDs as strings
#' @param access_key An object containing server URL and authentication info: see "authenticate"
#' @param channel_names A vector of channel names as strings (can specify either reagents or channels; leave the other as NULL)
#' @param reagent_names A vector of reagent names as strings (can specify either reagents or channels; leave the other as NULL; if using reagent_names, FCS_files must be an output of get_FCS_files())
#' @param statistic_types A vector of statistic types returned including "mean", "median", "quantile", "eventcount"
#' @param q specified quantile (numeric)
#' @param population_names vector of population names as strings
#' @param query_limit the number of queries the server can handle at once (if exceeds query limit, it will batch requests)
#' @export
#' @examples
#' population_names <- c("CD4+T cells", "CD8+T cells", "CD14+ Monocytes")
#' reagent_names <- c("pSTAT1", "pSTAT3", "pMAPKAPK2")
#' statistics_frame <- get_statistics_set_bulk(experimentID, FCS_files, access_key, reagent_names = reagent_names,
#'                      statistic_types = c("median"), q=NULL, population_names = population_names, query_limit = 500)

get_statistics_set_bulk <- function(experimentID, FCS_files, access_key, channel_names = NULL, reagent_names = NULL,
                                    statistic_types, q=NULL, population_names = NULL, query_limit = 2500){

  # FCS_fileIDs
  if (is.data.frame(FCS_files)){
    FCS_fileIDs <- FCS_files$`_id`
  } else {
    FCS_fileIDs <- FCS_files
  }
  # generate channel_names
  if (length(reagent_names) != 0 & length(channel_names) == 0){
    channel_names <- convert_reagents_to_channels(reagent_names, FCS_files)
  } else if (length(reagent_names) == 0 & length(channel_names) == 0){
    print("Error: Must specify channel_names or reagent_names")
  }

  # generate population IDs
  population_frame <- get_populations(experimentID, acess_key)
  populationIDs <- population_frame[population_frame$name %in% population_names, "_id"]

  # subdivide into queries of appropriate length
  query_size <- length(FCS_fileIDs)*length(populationIDs)*length(channel_names)
  print(query_size)

  if (query_size < query_limit){
    print("Query is small enough")
    stat_frame <- get_bulk_statistics(experimentID, FCS_fileIDs, access_key, channel_names,
                                      statistic_types, q=q, populationIDs = populationIDs)

  } else {
    print("Query is big!!")
    FCS_batch_size <- floor(query_limit/(length(populationIDs)*length(channel_names)))
    print(FCS_batch_size)
    d <- 1:length(FCS_fileIDs)
    ind_list <- split(d, ceiling(seq_along(d)/FCS_batch_size))
    print(length(ind_list))

    stat_frame = c()
    for (i in 1:length(ind_list)){
      FCS_fileID_set <- FCS_fileIDs[ind_list[[i]]]
      print(i)
      new_frame <- get_bulk_statistics(experimentID, FCS_fileID_set, access_key, channel_names,
                                       statistic_types, q=NULL, populationIDs = populationIDs)
      stat_frame <- rbind(stat_frame, new_frame)
    }

  }

  return(stat_frame)
}


  # reformat everything post query for output; can keep in same format but should subsitute reagents, populations, and filenames
  # fuse into features?? (probably somewhere else...)

  #' Generate fold change statistics object
  #'
  #' Takes a statistics object and presents statistics relative to designated baseline.
  #' Options exist for asinh ratio or fold change on raw counts.
  #' If using a statistics object that is an output of a get_statistics function, use statistics_object$statistics
  #' Must contain "Condition" column
  #' Unique identifier columns such as filename, File ID, etc must be specified as arguments for removal
  #'
  #'@param statistics_frame data frame of statistics
  #'@param basal_name name of the basal condition as string e.g. "Basal"
  #'@param fold_type either "asinh", which calculates asinh ratio, or "raw", which calculates the fold change in raw counts
  #'@param unique_IDs a vector of strings that are the names of the unique identifier columns (e.g. file_ID or filename)
  #'@param annotation_columns vector of strings that are the names of the annotation columns, specifically the remaining non-numeric column names (e.g. "Donor", "Species", "Gender", "Condition")
  #'@examples
  #' get_folds(statistics_frame = statistics_object$statistics, basal_name = "Basal", fold_type = "asinh", unique_IDs = c("file_ID", "filename"),
  #'              annotation_columns = c("Donor","Species", "Gender","Condition"))
  #'@export
  #'@import reshape2
  #'@import dplyr

  get_folds <- function(statistics_frame, basal_name, fold_type, unique_IDs = c("file_ID", "filename"),
                        annotation_columns = c("Donor","Species", "Gender","Condition")){

    df <- statistics_frame
    df <- df[, !(colnames(df) %in% unique_IDs)]

    formula <- as.formula(paste(paste(annotation_columns, collapse = " + "), "variable", sep = " ~ "))

    base_df <- df %>%
      dplyr::filter(Condition == basal_name) %>%
      dplyr::rename(Base_Condition = Condition) %>%
      reshape2::melt() %>%
      dplyr::rename(Base_Value = value)


    stim_df <- df %>%
      dplyr::filter(Condition != basal_name) %>%
      dplyr::rename(Stim_Condition = Condition) %>%
      reshape2::melt() %>%
      dplyr::rename(Stim_Value = value)

    if (fold_type == "asinh"){

      base_df$Base_Value <- asinh(base_df$Base_Value/5)
      stim_df$Stim_Value <- asinh(stim_df$Stim_Value/5)

      new_df <- merge(stim_df, base_df) %>%
        dplyr::mutate(fold_value = Stim_Value - Base_Value) %>%
        dplyr::select(-Base_Condition, -Base_Value, -Stim_Value) %>%
        dplyr::rename(Condition = Stim_Condition) %>%
        reshape2::dcast(formula)

    } else if (fold_type == "raw"){

      new_df <- merge(stim_df, base_df) %>%
        dplyr::mutate(fold_value = Stim_Value / Base_Value) %>%
        dplyr::select(-Base_Condition, -Base_Value, -Stim_Value) %>%
        dplyr::rename(Condition = Stim_Condition) %>%
        reshape2::dcast(formula)
    }
    return(new_df)

  }


