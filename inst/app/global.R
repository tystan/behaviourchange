

### NOTE: as lazyloading == TRUE, data equiv auto loaded
# data(equiv) # load data
# load("data/equiv.RData")
non_select <- "<Please select>"

get_phys_func_score <- function() {

  poss_scores <- as.character(sort(unique(equiv$difference_physical_score)))
  return(c(non_select, poss_scores))

}

phys_func_scores <- get_phys_func_score()

get_behav_list <- function() {

  behav_list <-
    c(
      "Sleep",
      "Self-Care",
      "Screen Time",
      "Quiet Time",
      "Physical Activity",
      "School-Related",
      "Domestic/Social",
      "Fruit and Veg",
      "Discretionary Foods",
      "Sugar-sweetended bevarages"
    )
  return(c(non_select, behav_list))

}

behavs <- get_behav_list()
