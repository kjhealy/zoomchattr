### Utility functions

#' Check if in and out folders exist
#'
#' Support function for reading and writing files
#'
#' @param indir Input directory path
#' @param outdir Output directory path
#'
#' @return Stops if they don't
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
check_in_out <- function(indir, outdir) {
  if(!fs::dir_exists(indir)) {
    stop("The input directory does not exist.")
  }

  if(!fs::dir_exists(outdir)) {
    stop("The output directory does not exist.")
  }
}

#' Get vector of filepaths
#'
#' Get filepaths of given type, possibly recursively
#'
#' @param ftype Filetype glob, defaults to '*.Rmd'
#' @param indir Directory to begin search in
#' @param depth Recursion depth, defaults to 1 (ie inside subfolders)
#'
#' @return Vector of file paths
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
get_files_of_type <- function(ftype = "*.txt", indir, depth = 1){
  tibble::tibble(
    inpath =
      fs::dir_ls(
        path = here::here(indir),
        recurse = depth,
        glob = ftype
      ))
}



#' Check chat format of a single file
#'
#' Is the Zoom chat file in one-line (older) or two-line (newer) format?
#'
#' @param file File path to a Zoom chat file
#'
#' @return Character vector, "v1" if in one-line format, "v2" if in two-line format
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
check_chat_format <- function(file) {
  tmp <- readr::read_lines(file = file, n_max = 20)
  ifelse(any(stringr::str_detect(tmp, "^\\t")), "v2", "v1")
}

#' Get a vector of Zoom chat files
#'
#' Point at directory, get chat txt file paths and names
#'
#' @param path The director to look in, defaults to ~/Documents/Zoom
#' @param regexp The regexp to search for
#' @param recurse Look in all the subdirectories? Defaults to TRUE
#' @param ... Other arguments passed to fs::dir_ls
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
get_zoom_names <- function(path = "~/Documents/Zoom",
                           regexp = "meeting_saved_chat.txt|chat.txt",
                           recurse = TRUE,
                           ...){
  fs::dir_ls(path = path,
             regexp = regexp,
             recurse = recurse,
             ...
  )

}



#' Check the chat format of all given files
#'
#' Are these Zoom chat files in one-line (older) or two-line (newer) format?
#'
#' @param files A vector of file paths to Zoom chat files
#'
#' @return A two column tibble with columns `file` (the file path) and `type` ("One Liner" or "Two Liner")
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
check_all_chat_format <- function(files) {
  files |>
    map_chr(check_chat_format) |>
    enframe() |>
    dplyr::rename(file = name, type = value)
}


#' Get file dates
#'
#' Extract YYYY-MM-DD dates from file names
#'
#' @param fnames Vector of Zoom Chat file names
#'
#' @return Vector of YYYY-MM-DD dates the length of `fnames`
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
get_fdates <- function(fnames) {
  stringr::str_extract(fnames, "\\d{4}-\\d{2}-\\d{2}")
}



#' Parse a Version 1 chat file into a tibble
#'
#' @param file Path to a version 1 Zoom chat file
#'
#' @return A tibble with columns `time` (dttm), `from` (chr), `to` (chr), and `message` (chr)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_v1_chat <- function(file) {
  infile <- readr::read_lines(file) |>
    str_remove_all("\\t")
  infile_date <- stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}")

  chat_tibble <- tibble(
    time = lubridate::as_datetime(paste(lubridate::ymd(infile_date),
                                        str_extract(infile, "\\d{2}:\\d{2}:\\d{2}")), tz = "EST"),
    from = stringr::str_replace(infile,
                                "(^\\d{2}:\\d{2}:\\d{2} From )(.+?)( : )(.+?$)", "\\2"),
    message = stringr::str_replace(infile,
                          "(\\d{2}:\\d{2}:\\d{2} From )(.+?)( : )(.+?$)", "\\4"))

  chat_tibble |>
    dplyr::mutate(dplyr::across(where(rlang::is_character), stringr::str_squish)) |>
    process_from_column()
}



#' Parse a series of version 1 chat files
#'
#' @param files A vector of file paths to version 1 Zoom chat files
#'
#' @return A tibble of parsed chats with columns `file` ((chr) source file name), `time` ((dttm) date and time of chat), `from` (chr), `to` (chr), `message` ((chr) Message content)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_all_v1_chat <- function(files) {
  files |>
    purrr::set_names() |>
    purrr::map_dfr(parse_v1_chat, .id = "file")

}

#' Parse a version 2 chat file
#'
#' @param file File path for a version 2 Zoom chat file
#'
#' @return A tibble of the chat with columns `time` (dttm), `from` (chr), `to` (chr), and `message` (chr)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_v2_chat <- function(file) {
  infile <- readr::read_lines(file) |>
    str_remove_all("\\t")

  infile_date <- stringr::str_extract(file, "\\d{4}-\\d{2}-\\d{2}")

  line_1 <- infile[c(TRUE,FALSE)]
  line_2 <- infile[c(FALSE, TRUE)]

  full_record <- paste(line_1, line_2)

  chat_tibble <- tibble(
    time = lubridate::as_datetime(paste(lubridate::ymd(infile_date),
                                        str_extract(full_record, "\\d{2}:\\d{2}:\\d{2}")), tz = "EST"),
    from = str_replace(full_record,
                       "(\\d{2}:\\d{2}:\\d{2} )(From .+?:)(.+?$)", "\\2"),
    message = str_replace(full_record,
                          "(\\d{2}:\\d{2}:\\d{2} )(From .+?:)(.+?$)", "\\3"))

  chat_tibble |>
    dplyr::mutate(dplyr::across(where(rlang::is_character), stringr::str_squish)) |>
    process_from_column()

}


#' Parse a series of version 2 Zoom chat files
#'
#' @param files A vector of version 2 Zoom chat files
#'
#' @return A tibble of parsed chats with columns `file` ((chr) source file name), `time` ((dttm) date and time of chat), `from` ((chr) From and To), `message` ((chr) Message content)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_all_v2_chat <- function(files) {
  files |>
    purrr::set_names() |>
    purrr::map_dfr(parse_v2_chat, .id = "file")

}


#' Parse a Zoom Chat file
#'
#' Turn a Zoom chat file into a tibble with one chat message per row
#'
#' @param file Path to a Zoom chat file (v1 or v2)
#'
#' @return A tibble of the chat with columns `time` (dttm), `from` (chr), `to` (chr), and `message` (chr)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_zoom_chat <- function(file) {

  ver <- check_chat_format(file)
  # message(paste("This appears to be a ", ver, "chat file"))

  switch(ver,
         "v1" = parse_v1_chat(file),
         "v2" = parse_v2_chat(file)
  )
}


#' Parse a series of Zoom chats
#'
#' Turn a vector of file paths to Zoom chat files into a tibble of chats with one chat message per row.
#'
#' @param files A vector of file paths to Zoom chat files
#'
#' @return A tibble of parsed chats with columns `file` ((chr) source file name), `time` ((dttm) date and time of chat), `from` ((chr) From and To), `message` ((chr) Message content)
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
parse_all_zoom_chat <- function(files) {
  files |>
    purrr::set_names() |>
    purrr::map_dfr(parse_zoom_chat, .id = "file")
}


#' Process from column
#'
#' Split and clean the from column into from and to components
#'
#' @param df A tibble of chat messages
#'
#' @return A tibble with from and to columns split out
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
process_from_column <- function(df) {
  df |>
    tidyr::separate(from, into = c("from", "to"), sep = " to ",
                    extra = "merge", fill = "right") %>%
    mutate(from = stringr::str_remove(from, "^From "),
           to = stringr::str_remove(to, ":"),
           to = stringr::str_remove(to, "(Direct Message)"))
}
