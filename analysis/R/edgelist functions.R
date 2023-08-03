#####################################################
# Functions to make edgelists for various data types#
#####################################################
library(dplyr)

# 1.function to make a percentage table from presence/absence data

make_presabs_prop <- function(data, col_node) {
  col_node <- enquo(col_node)

  data %>%
    tidyr::pivot_longer(!(!!col_node), names_to = "type", values_to = "count") %>%
    group_by((!!col_node), type) %>%
    count(count) %>%
    mutate(freq = n / sum(n) * 100) %>%
    filter(count == 1) %>%
    select((!!col_node), type, freq) %>%
    tidyr::pivot_wider(names_from = type, values_from = freq) %>%
    tibble::as_tibble()
}

##############

# 2.Make a proportions table from raw counts, to feed to the edgelist function:

make_count_prop <- function(x) {
  x_gather <- x %>%
    tidyr::gather(key = "type", value = "type_count", -Site)

  x_sum <- x_gather %>%
    group_by(Site) %>%
    summarise(Site_sum = sum(type_count))

  x_prop <- x_gather %>%
    left_join(x_sum, by = "Site") %>%
    mutate(prop = type_count / Site_sum * 100) %>%
    select(-type_count, -Site_sum) %>%
    mutate(type = paste0(type)) %>%
    tidyr::spread(key = type, value = prop)

  return(x_prop)
}

###############
# 3. make edgelist from percentage data listed by site and variable

make_edgelist <- function(data) {
  data <- data %>% select_if(function(x) {
    !all(is.na(x))
  }) # only select those coumns that have values in them

  data_long <- data %>%
    tidyr::pivot_longer(-1, names_to = "name") %>%
    dplyr::rename(Var1 = 1) # make data long

  sites <- data %>% # get all pairs of sites
    tidyr::expand(x = .[[1]], y = .[[1]]) %>%
    filter(x != y) %>%
    rowwise() %>%
    mutate(id = paste0(sort(c(x, y)), collapse = " ")) %>%
    distinct(id, .keep_all = TRUE) %>%
    dplyr::select(-id)

  Type <- tibble::tibble(name = colnames(data[-1])) # get all edge types

  merge(sites, Type) %>%
<<<<<<< HEAD
    left_join(data_long, by = c("x" = "Var1", "name" = "name")) %>%
    left_join(data_long, by = c("y" = "Var1", "name" = "name")) %>% # merge the data
    rowwise() %>%
    mutate(weight = min(value.x, value.y)) %>% # because of rowwise we now get the minimum perc as weight
=======
    left_join(data_long, by = c("x" = "Var1", "name" = "name"))%>%
    left_join(data_long, by = c("y" = "Var1", "name" = "name")) %>%    #merge the data
    rowwise() %>%
    mutate(weight = min(value.x, value.y)) %>%                            #because of rowwise we now get the minimum perc as weight
>>>>>>> d07b969c8b97784bee246785892543c639c2e6af
    select(-c(value.x, value.y)) %>%
    tidyr::drop_na() %>%
    filter(weight > 0)
}


make_network <- function(edgelist) {
  a <- edgelist %>%
    distinct(x) %>%
    rename(label = x)

  b <- edgelist %>%
    distinct(y) %>%
    rename(label = y)

  nodes <- full_join(a, b, by = "label") %>% tibble::rowid_to_column("id")
  rm(a, b)

  edges <- edgelist %>%
    left_join(nodes, by = c("x" = "label")) %>%
    rename(from = id)

  edges <- edges %>%
    left_join(nodes, by = c("y" = "label")) %>%
    rename(to = id)

  edges <- dplyr::select(edges, from, to, weight, name)

  edges$weight <- as.numeric(edges$weight)

  network.decor <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
}
