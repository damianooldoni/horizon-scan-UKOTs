#' Check presence in longlist of species in shortlist
#'
#' This function is a tool to check whether all species selected by experts are
#' present in the longlist. This function provides an useful tool to find
#' species that experts don't find in longlist, although they consider them
#' important to be added to their shortlist. This function can also find
#' (human-related) errors like mistyping.
#'
#' @param longlist_filename (character) a single string with the name of the
#'   Googlesheet containing the longlist.
#' @param merged_species_df (data.frame) a data.frame with at least the
#'   following columns: "species" and "experts".
#'   all shortlists. We will call it merged shortlist or combined shortlist.
#' @param ws_longlist (single character or number) name or number of the
#'   worksheet containing the longlist.
#' @param row_number_longlist (single integer number) Row number in longlist
#'   spreadsheet containing column names.
#' @param col_name_species_longlist (single character) Name of the column
#'   containing species in longlist. Default: "species".
#' @return A data.frame containing species not present in longlist and the
#'   experts who added them. An on-screen message is also given.
#' @importFrom googlesheets gs_ls gs_key gs_read
#' @importFrom dplyr %>% filter slice select_
#' @importFrom stringr str_detect
#' @importFrom clean_names
check_completeness_longlist <- function(longlist_filename,
                                        merged_species_df,
                                        ws_longlist,
                                        row_number_longlist = 1,
                                        col_name_species_longlist = "species") {
  my_sheets <- gs_ls()
  
  # retrieve longlist
  longlist_ssheet <- my_sheets %>% 
    filter(str_detect(my_sheets$sheet_title, 
                      pattern = longlist_filename))
  # take the most recent version of longlist
  longlist_ssheet <- longlist_ssheet %>% 
    filter(updated == max(longlist_ssheet$updated))
  longlist <- gs_key(longlist_ssheet$sheet_key) %>% gs_read(ws = ws_longlist)
  
  merged_species_df
  
  if (row_number_longlist > 1) {
    colnames(longlist) <- shortlist[row_number_longlist-1,]
    longlist <- longlist %>% slice(row_number_longlist:n())
  }

  longlist <- janitor::clean_names(longlist)
  
  # get comma separated unique list of experts
  merged_species_df %>%
    select(species, experts) %>%
    group_by(species) %>%
    summarize(experts = str_c(unique(word(experts, end = -1, sep = ",")), 
                              collapse = ","))
  
  species_to_add <- anti_join(
    merged_species_df,
    longlist %>% select_(col_name_species_longlist),
    by_ = c(species = col_name_species_longlist)) %>%
    group_by(species) %>%
    summarize(experts = str_c(unique(word(experts, end = -1, sep = ",")), 
                             collapse = ","))
  species_to_add
}