
# get all filenames in the specified directory that match the specified pattern
get_relevant_calib_filenames <- function(directory =  "/p/projects/landuse/data/input/calibration", 
                                         string_to_match = "calibtration_testing_DavidH", 
                                         recursive =FALSE) {
  # List all files in the directory
  all_files <- list.files(directory, full.names = TRUE, recursive = recursive)
  
  # Filter files that contain the string "DavidH" in the filename
  matched_files <- grep(string_to_match, all_files, value = TRUE)
  
  return(matched_files)
}

# extracts a shorter name from the filename
extract_run_name_from_filename <- function(filename) {
  
  # cut off the begiining of the filename
  run_name <- stringr::str_extract(filename, "(?<=DavidH_).*")

  # remove thes string indication that the run used yield calibration 
  run_name <- stringr::str_remove(run_name, "with_yld_calib_")

  # remove ending
  run_name <- stringr::str_remove(run_name, ".tgz")
  
  return(run_name)
}

# reads the calibration factors from the tar.gz files and returns a list of dataframes
read_calibration_factors_as_list_of_dataframes <- function(filename_list, keep_calibration_fac_tmp_dir =F, extract_shorter_names) {
  dir <- ".__calibration_factors_tmp/"
  if (file.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  dataframe_list <- NULL
  for(i in seq_along(filename_list)) {
    if(extract_shorter_names) {
      run_name <- extract_run_name_from_filename(filename_list[i])
    } else {
      run_name <- filename_list[i]
    }
    untar(filename_list[i], exdir = paste0("./", dir, run_name) )
    calib_factors <- read.csv(paste0("./", dir, run_name, "/f14_yld_calib.csv"),
                                    comment.char = "*")
    names(calib_factors)[1] <- "region"
    # add the entry to the list and have the name of the list entry be the run name
    dataframe_list[[run_name]] <- calib_factors
  }
  if(!keep_calibration_fac_tmp_dir) {
    unlink(dir, recursive = TRUE)
  }
  return(dataframe_list)
}

# prints a dataframe with colored calibration factors in a horizontal way
color_print_cal_factor_df_hor <- function(cal_factor_df, name, deviation_thresh = 0.1){
  define_col_based_on_val <- function(val){
    if(abs(val-1)< deviation_thresh){
    return(crayon::make_style( rgb(0.5 , 0.5, max(min(0.5-(val^2-1), 1),0)) ))
    }
    else{
    return(crayon::make_style( rgb(0.5 , 0.5, max(min(0.5-(val^2-1), 1),0)), bg=T ))
    }
  }

  # regions in first row
  cat(name, "\n")
  cat("REG   ")
  for(i in seq_along(cal_factor_df$crop)) {
    cat(cal_factor_df$region[i], "  ")
  }

  # crop values in sec row
  cat("\n")
  cat("CROP  ")
  for(i in seq_along(cal_factor_df$crop)) {
    val_crop <- cal_factor_df$crop[i]
    color <- define_col_based_on_val(val_crop)
    cat(color(format(as.numeric(val_crop), nsmall =2)), " ")
  }

  # pasture values in third row
  cat("\n")
  cat("PAST  ")
  for(i in seq_along(cal_factor_df$crop)) {
    val_past <- cal_factor_df$past[i]
    color <- define_col_based_on_val(val_past)
    cat(color(format(as.numeric(val_past), nsmall =2)), " ")
  }
  cat("\n \n")

}

# function to evaluate yield calibration factors
color_print_yield_calib_facs <- function(
  path_to_calibration_archive = "/p/projects/landuse/data/input/calibration",
  string_to_match = "calibtration_testing_DavidH",
  deviation_thresh = 0.1,
  keep_calibration_fac_tmp_dir = FALSE,
  extract_shorter_names = TRUE
) {
  filename_list <- get_relevant_calib_filenames(path_to_calibration_archive, string_to_match)
  calib_factors_list <- read_calibration_factors_as_list_of_dataframes(filename_list, 
                                            keep_calibration_fac_tmp_dir, 
                                            extract_shorter_names)

  for(calib_fac in seq_along(calib_factors_list)) {
    color_print_cal_factor_df_hor(calib_factors_list[[calib_fac]], 
                                  names(calib_factors_list[calib_fac]), 
                                  deviation_thresh = deviation_thresh)
  }
}

# 
# df_tc <- read_calib_faccs3(files)

color_print_cs3_fact <- function(){
    files <- get_relevant_calib_filenames("./output/with_yld_calib", "calib_tc_factor.*cs3", recursive=T)
    df_tc <- read_calib_faccs3(files)
    for(i in seq_along(df_tc)){
        color_print_cal_factor_df_hor(df_tc[[i]], names(df_tc[i]))
    }
}

read_calib_faccs3 <- function(filename) {
  calib_factors <- NULL
  for(i in seq_along(filename)) {
    calib_factors[[filename[i]]] <- read.csv(filename[i])
    names(calib_factors[[filename[i]]])[1] <- "region"
  }
  return(calib_factors)
}
