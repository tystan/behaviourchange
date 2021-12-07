

# ---- print_log ----


# change this to enable/disable the verbatim log
audit_output <- TRUE




# ---- constants ----

audit_output_js <- c("false", "true")[audit_output + 1]

non_select <- c("<Please select>" = "null")


behav_list <-
  c(
    "Sleep" = "sleep" ,
    "Self-Care" = "self_care",
    "Screen Time" = "screen_time",
    "Quiet Time" = "quiet_time",
    "Physical Activity" = "physical_activity" ,
    "School-Related" = "school_related",
    "Domestic/Social" = "domestic_social",
    "Fruit and Veg" = "fruit_veg",
    "Discretionary Foods" = "discretionary_food",
    "Sugar-sweetended bevarages" = "ssb"
  )


# ---- funcs ----


get_phys_func_score <- function() {

  poss_scores <- sort(unique(equiv$difference_physical_score))
  return(c(non_select, poss_scores))

}

get_behav_list <- function() {

  return(c(non_select, behav_list))

}



changes_one_filter <- function(behav_col, col1, val1) {

  if (non_select %in% c(behav_col, col1, val1)) {
    return(non_select)
  }

  filter_ii <- (equiv[[col1]] == val1)
  filter_vals <- unique(equiv[filter_ii, behav_col])
  filter_vals <- filter_vals[order(as.numeric(filter_vals))]

  return(c(non_select, filter_vals))

}

changes_two_filter <- function(behav_col, col1, val1, col2, val2) {

  if (non_select %in% c(behav_col, col1, val1, col2, val2)) {
    return(non_select)
  }

  filter_ii <- (equiv[[col1]] == val1) & (equiv[[col2]] == val2)
  filter_vals <- unique(equiv[filter_ii, behav_col])
  filter_vals <- filter_vals[order(as.numeric(filter_vals))]

  return(c(non_select, filter_vals))

}

changes_three_filter <- function(behav_col, col1, val1, col2, val2, col3, val3) {

  if (non_select %in% c(behav_col, col1, val1, col2, val2, col3, val3)) {
    return(non_select)
  }

  filter_ii <- (equiv[[col1]] == val1) & (equiv[[col2]] == val2) & (equiv[[col3]] == val3)
  filter_vals <- unique(equiv[filter_ii, behav_col])
  filter_vals <- filter_vals[order(as.numeric(filter_vals))]

  return(c(non_select, filter_vals))

}



# ---- load ----


### NOTE: as lazyloading == TRUE, data `equiv` auto loaded
# data(equiv) # load data
# load("data/equiv.RData")

phys_func_scores <- get_phys_func_score()
behavs <- get_behav_list()



rm_elements_behav <- function(rm_behav) {

  return(behavs[!(behavs %in% rm_behav)])

}

get_behav_nm <- function(behav_var) {
  return(names(behavs)[behavs == behav_var])
}



