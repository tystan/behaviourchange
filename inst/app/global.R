

# ---- print_log ----


# change this to enable/disable the verbatim log
audit_output <- FALSE




# ---- load ----


### NOTE: as lazyloading == TRUE, data `equiv` auto loaded
# data(equiv) # load data
# cat(getwd())
# load("../../data/equiv.RData")


# ---- funcs ----


get_phys_func_score <- function() {

  poss_scores <- sort(unique(equiv$difference_physical_score))
  names(poss_scores) <- add_prefix(poss_scores)
  return(c(non_select, poss_scores))

}

get_behav_list <- function() {

  return(c(non_select, behav_list))

}



rm_elements_behav <- function(rm_behav) {

  return(behavs[!(behavs %in% rm_behav)])

}

get_behav_nm <- function(behav_var) {
  return(names(behavs)[behavs == behav_var])
}


selections_df_init <-
  data.frame(
    var = character(0),
    val = character(0),
    stringsAsFactors = FALSE
  )

reac_list <- reactiveValues(selections_df = selections_df_init)


add_selections_df <- function(var, val) {

  selections_df_tmp <- reac_list$selections_df

  if (var == "null") {

    # nothing to update
    dev_null <- NULL

  } else if (val == "null") {

    row_to_update <- which(selections_df_tmp$var == var)
    if (length(row_to_update) != 1) {
      selections_df_tmp <- rbind(selections_df_tmp, data.frame(var = var, val = "null"))
    } else {
      selections_df_tmp[row_to_update, "val"] <- val
    }

  } else {

    row_to_update <- which(selections_df_tmp$var == var)
    if (length(row_to_update) < 1) {
      selections_df_tmp <- rbind(selections_df_tmp, data.frame(var = var, val = val))
    } else if (length(row_to_update) == 1) {
      selections_df_tmp[row_to_update, "val"] <- val
    } else {
      stop("more than one var = '", var, "' record")
    }

  }

  reac_list$selections_df <- selections_df_tmp
  return(invisible(NULL))

}


get_selections_df <- function() {
  return(reac_list$selections_df)
}

get_nrow <- function(exclude_null_vals = FALSE) {

  selections_df_tmp <- get_selections_df()
  if (exclude_null_vals) {
    selections_df_tmp <- selections_df_tmp[selections_df_tmp$val != "null", ]
  }
  return(nrow(selections_df_tmp))
}

englishify_number <- function(x) {
  # starting at 0th, 1st, ...
  suffix_000_009 <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  suffix_010_019 <- c("th", "th", "th", "th", "th", "th", "th", "th", "th", "th")
  suffix_000_099 <- c(suffix_000_009, suffix_010_019, rep(suffix_000_009, 8))
  return(paste0(x, suffix_000_099[(x %% 100) + 1]))
}
# englishify_number(0:99) # test


reintialise_selections_df <- function() {
  reac_list$selections_df <- selections_df_init
  return(invisible(NULL))
}



get_filtered_vars <- function() {

  selections_df_tmp <- reac_list$selections_df
  selections_df_tmp <- selections_df_tmp[selections_df_tmp$val != "null", ]
  in_use_vars <- selections_df_tmp$var
  remain_vars <- behav_list[!(behav_list %in% in_use_vars)]
  return(c(non_select, remain_vars))

}


get_filtered_values <- function(col_of_interest) {

  if (col_of_interest == "null") {
    return(non_select)
  }
  selections_df_tmp <- reac_list$selections_df
  selections_df_tmp <- selections_df_tmp[selections_df_tmp$val != "null", ]
  n <- nrow(selections_df_tmp)
  filter_ii <- rep(TRUE, nrow(equiv))
  for (i in 1:n) {
    filter_ii <-
      filter_ii &
      (equiv[[selections_df_tmp$var[i]]] == selections_df_tmp$val[i])
  }
  filter_vals <- unique(equiv[filter_ii, col_of_interest])
  filter_vals <- filter_vals[order(as.numeric(filter_vals))]
  names(filter_vals) <-
    add_prefix_suffix(rep(col_of_interest, length(filter_vals)), filter_vals)
  return(c(non_select, filter_vals))

}

get_behav_label <- function() {
  return(
    paste(
      "Please select the",
      englishify_number(get_nrow()),
      "behaviour you would like to change"
    )
  )
}


add_prefix <- function(val) {
  plus_req <- !grepl("^(0$|-|null$)", val)
  prefixes <- c("", "+")
  paste0(prefixes[plus_req + 1], val)
}
# test_df <- data.frame(
#   var = c("sleep", "screen_time", "fruit_veg", "ssb"),
#   val = c("10", "-20", "0.5", "2.0")
# )
# add_prefix(test_df$val)
# add_prefix(c("-20", "0", "0.5", "null"))

add_suffix <- function(var, val) {
  minut_ii <-
    var %in% c(
      "sleep", "self_care", "screen_time", "quiet_time",
      "physical_activity", "school_related", "domestic_social"
    )
  serve_ii <-
    var %in% c(
      "fruit_veg", "discretionary_food", "ssb"
    )
  # the remainder of options for var are "null" so don't add suffix
  suffixes <- c("", " minutes", " serves")[minut_ii + 2 * serve_ii + 1]
  # special case when val is "null" so don't add suffix
  val_null <- (val == "null")
  if (sum(val_null) > 0) {
    suffixes[val_null] <- ""
  }
  paste0(val, suffixes)
}
# add_suffix(test_df$var, test_df$val)
# add_suffix(c("ssb", "physical_activity"), c("0", "null"))

add_prefix_suffix <- function(var, val) {
  add_suffix(var, add_prefix(val))
}
# add_prefix_suffix(test_df$var, test_df$val)
# add_prefix_suffix(c("ssb", "physical_activity"), c("0", "null"))


mk_html_text_selections <- function() {

  selections_df_tmp <- reac_list$selections_df
  selections_df_tmp <- selections_df_tmp[selections_df_tmp$val != "null", ]
  n_sel <- nrow(selections_df_tmp)

  if (n_sel < 1) {
    return("")
  }

  # check phys score selected
  out_str_vec <- NULL
  phys_row <- selections_df_tmp$var %in% "difference_physical_score"
  if (!any(phys_row)) {
    return("") # no phys score selected
  } else {
    out_str_vec <-
      c(
        out_str_vec,
        paste(
          "You have chosen a",
          mk_col_html_text(add_prefix(selections_df_tmp$val[phys_row])),
          "difference in <strong>Physical Functioning Score:</strong><br><br>"
        )
      )
    selections_df_tmp <- selections_df_tmp[!phys_row, ]
  }

  if (n_sel > 1) {
    out_str_vec <- c(out_str_vec, "<ul>")

    out_str_vec <-
      c(
        out_str_vec,
        paste0(
          "<li>and to change <strong>",
          get_behav_nms(selections_df_tmp$var),
          "</strong> by ",
          mk_col_html_text(add_prefix_suffix(selections_df_tmp$var, selections_df_tmp$val)),
          "</li>"
        )
      )

    out_str_vec <- c(out_str_vec, "</ul>")
  }

  return(out_str_vec)
  # return(paste(out_str_vec, collapse = "\n"))

}

mk_col_html_text <- function(x) {
  neg_vals <- grepl('^-', x)
  x[neg_vals] <- paste0('<strong style="color:red;">', x[neg_vals], "</strong>")
  pos_vals <- grepl('^\\+', x)
  x[pos_vals] <- paste0('<strong style="color:green;">', x[pos_vals], "</strong>")
  return(x)
}

get_behav_nms <- function(behav) {
  list_to_check <- c(non_select, behav_list)
  found_in_list <- list_to_check %in% behav
  if (sum(found_in_list) < 1) {
    stop("Behavour name not found")
  } else {
    sub_list <- list_to_check[found_in_list]
    return(names(sub_list))
  }
}

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

phys_func_scores <- get_phys_func_score()
behavs <- get_behav_list()







