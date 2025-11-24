# the following functions are fairly generic to clean output
# none are critical to the analysis

paste_median_minmax <- function(var, digits = 2) {
  
  median_var <- round_away_0(median(var), digits, TRUE)
  lower_var <- round_away_0(min(var), digits, TRUE)
  upper_var <- round_away_0(max(var), digits, TRUE)
  
  glue("{median_var} [{lower_var}; {upper_var}]")
}

paste_range = function(x, collapse = " - ", digits = 2) paste(round(x, digits = digits),
                                                              collapse = collapse)

paste_list = function(x, digits = 1){
  paste0(round(x, digits), c( rep(", ", length(x) - 2), ", and ", ""), collapse = "")
}

# the following functions come from github: FredHutch/VISCfunctions
# remotes::install_github("FredHutch/VISCfunctions") to install and check help documentation

stat_paste = function (stat1,
                       stat2 = NULL,
                       stat3 = NULL,
                       digits = 0,
                       trailing_zeros = TRUE,
                       bound_char = c("(", "[", "{", "|"),
                       sep = ", ",
                       na_str_out = "---",
                       suffix = NULL)
{
  bound_char <- match.arg(bound_char)
  end_bound_char <- switch(
    bound_char,
    `(` = ")",
    `[` = "]",
    `{` = "}",
    `|` = "|"
  )
  stat1_pasted_obj <- ifelse(is.na(stat1), na_str_out, as.character(
    .round_if_numeric(stat1, digits = digits, trailing_zeros = trailing_zeros)
  ))
  if (!is.null(suffix))
    stat1_pasted_obj <- paste0(stat1_pasted_obj, suffix)
  if (is.null(stat2)) {
    pasted_output <- stat1_pasted_obj
  }
  else {
    stat2_pasted_obj <- ifelse(is.na(stat2),
                               na_str_out,
                               as.character(
                                 .round_if_numeric(stat2, digits = digits, trailing_zeros = trailing_zeros)
                               ))
    if (!is.null(suffix))
      stat2_pasted_obj <- paste0(stat2_pasted_obj, suffix)
    if (is.null(stat3)) {
      pasted_output <- ifelse(
        is.na(stat1) & is.na(stat2),
        na_str_out,
        paste0(
          stat1_pasted_obj,
          " ",
          bound_char,
          stat2_pasted_obj,
          end_bound_char
        )
      )
    }
    else {
      stat3_pasted_obj <- ifelse(is.na(stat3),
                                 na_str_out,
                                 as.character(
                                   .round_if_numeric(stat3, digits = digits, trailing_zeros = trailing_zeros)
                                 ))
      if (!is.null(suffix))
        stat3_pasted_obj <- paste0(stat3_pasted_obj, suffix)
      pasted_output <- ifelse(
        is.na(stat1) & is.na(stat2) &
          is.na(stat3),
        na_str_out,
        paste0(
          stat1_pasted_obj,
          " ",
          bound_char,
          stat2_pasted_obj,
          sep,
          stat3_pasted_obj,
          end_bound_char
        )
      )
    }
  }
  pasted_output
}

.round_if_numeric =function (x, digits = 0, trailing_zeros = FALSE) 
{
  if (is.numeric(x)) 
    round_away_0(x, digits = digits, trailing_zeros = trailing_zeros)
  else x
}


round_away_0 = function (x, digits = 0, trailing_zeros = FALSE) {

  rounded_vals <- sign(x) * round(abs(x) + 1e-15, digits)
  if (trailing_zeros) {
    rounded_vals[!is.na(rounded_vals)] <- formatC(rounded_vals[!is.na(rounded_vals)], 
                                                  digits, format = "f")
    neg_to_change <- paste0("-0.", paste0(rep(0, digits), 
                                          collapse = ""))
    if (any(rounded_vals == neg_to_change, na.rm = TRUE)) 
      rounded_vals[rounded_vals == neg_to_change] <- substr(neg_to_change, 
                                                            2, nchar(neg_to_change))
  }
  rounded_vals
}

get_output_type = function() {
  current_output_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  ifelse(!is.null(current_output_type) && current_output_type == 
           "latex", "latex", "pandoc")
}

insert_break = function() {
  ifelse(knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex", 
         "\\clearpage", "\\newpage")
}

get_session_info = function (libpath = FALSE) {

  my_session_info <- sessioninfo::session_info()
  platform <- my_session_info[[1]]
  packages <- my_session_info[[2]]
  my_session_info1 <- data.frame(name = names(platform), value = matrix(unlist(platform), 
                                                                        nrow = length(platform)), stringsAsFactors = FALSE)
  my_current_input <- ifelse(is.null(ci <- knitr::current_input()), 
                             "No Input File Detected", ci)
  my_current_input_w_dir <- ifelse(is.null(ci <- knitr::current_input(dir = TRUE)), 
                                   "No Input File Detected", ci)
  file_name <- data.frame(name = "file name", value = my_current_input, 
                          stringsAsFactors = FALSE)
  gitremoteorg <- tryCatch(system2("git", "remote -v", stdout = TRUE, 
                                   stderr = FALSE)[1], error = function(c) "", warning = function(c) "")
  gitremote <- substr(gitremoteorg, regexpr("\t", gitremoteorg) + 
                        1, regexpr(" \\(", gitremoteorg) - 1)
  if (is.na(gitremote) || gitremote == "" || grepl("fatal", 
                                                   gitremote)) {
    folder_info <- data.frame(name = "location", value = ifelse(my_current_input_w_dir != 
                                                                  "No Input File Detected", dirname(my_current_input_w_dir), 
                                                                getwd()), stringsAsFactors = FALSE)
    my_session_info1 <- rbind(my_session_info1, folder_info, 
                              file_name)
  }
  else {
    if (my_current_input_w_dir != "No Input File Detected") {
      all_git_files <- system2("git", "ls-files -co --no-empty-directory --full-name", 
                               stdout = TRUE, stderr = FALSE)
      folder_info_in <- dirname(all_git_files[unlist(lapply(all_git_files, 
                                                            function(xx) grepl(xx, my_current_input_w_dir)))])
    }
    else {
      folder_info_in <- "No Input File Location Detected"
    }
    folder_info <- data.frame(name = "location", value = folder_info_in, 
                              stringsAsFactors = FALSE)
    url_info <- data.frame(name = "repo", value = gitremote, 
                           stringsAsFactors = FALSE)
    my_session_info1 <- rbind(my_session_info1, url_info, 
                              file_name, folder_info)
  }
  my_session_info2 <- packages[packages$attached, ]
  my_session_info2 <- with(my_session_info2, {
    data.frame(package = package, version = loadedversion, 
               data.version = purrr::map_chr(package, utils::packageDescription, 
                                             fields = "DataVersion"), date = date, source = source, 
               libpath = library)
  })
  if (!libpath) 
    my_session_info2$libpath <- NULL
  if (any(!is.na(my_session_info2$data.version))) 
    my_session_info2$data.version[is.na(my_session_info2$data.version)] <- ""
  else my_session_info2 <- my_session_info2[, -match("data.version", 
                                                     colnames(my_session_info2))]
  list(platform_table = my_session_info1, packages_table = my_session_info2)
}



