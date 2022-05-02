#' Make a safety copy of a file
#'
#' Makes a safety copy of a file with "_OLD01" in the file name, before you are
#' overwriting an existing file. For instance, say that you have made a file
#' called 'my_precious_figure.png' in the 'figures' directory. If you call
#' \code{make_safety_copy("figures/my_precious_figure.png")}, a safety copy named
#' \code{'my_precious_figure_OLD1.png'} will be made. However, if the 'OLD01' name
#' already has been used, the copy will be named 'my_precious_figure_OLD2.png',
# etcetera.
#'
#'
#' @param fn The file name
#' @param length_file_extension Length of the file extension, by default 3 (for
#' instance .png, .jpg, .txt, .csv). If the file extension is shorter or longer
#' (e.g. .html), specify that here. A couple of simple tests is done to make sure
#' that length_file_extension has been set correctly.
#'
#' @return Nothing (NULL)
#' @export
#'
#' @examples
#'
#' # Will make a copy named 'Figures/07_tbt_vs_year_FIG7A_OLD01.png'
#' fn <- "Figures/07_tbt_vs_year_FIG7A.png"
#' make_safety_copy(fn)
#' # (if you run it again, the copy will be called
#' 'Figures/07_tbt_vs_year_FIG7A_OLD02.png')
#'
#' Too long or short file extension will throw a warning (and make a file copy with
#' a strange name):
#' make_safety_copy("Figures/My_test_file.html")
#' make_safety_copy("Figures/My_test_file.tx")
#'
#' # Adjust length_file_extension to get a copy with a better name:
#' make_safety_copy("Figures/My_test_file.html", length_file_extension = 4)
#' make_safety_copy("Figures/My_test_file.tx", length_file_extension = 2)

make_safety_copy <- function(fn, length_file_extension = 3){

  if (file.exists(fn)){
    i <- 0
    file_exists <- TRUE
    filename_first_part <- stringr::str_sub(fn, end = -length_file_extension - 2)
    filename_extension <- stringr::str_sub(fn, start = -length_file_extension)
    dot_position <- stringr::str_sub(fn, start = -length_file_extension-1,
                                     end = -length_file_extension-1)
    if (grepl(".", filename_extension, fixed = TRUE)){
      warning("The last ", length_file_extension, " characters contains a '.' Please change 'length_file_extension'")
    }
    if (!grepl(".", dot_position, fixed = TRUE)){
      warning("Character number ", length_file_extension+1, " from the end is not a '.' Please change 'length_file_extension'")
    }
    # Search for free names to use
    while(file_exists){
      i <- i + 1
      new_name <- paste0(filename_first_part,
                         "_OLD", sprintf("%02i", i), ".",
                         filename_extension)
      file_exists <- file.exists(new_name)
    }
    file.copy(fn, new_name)
    cat("A copy of the file has been made, named ", new_name, " .\n", sep = "")
  } else {
    cat("File doesn't exist - no need to make safety copy. \n")
  }
  invisible(NULL)
}
