# ========== gamete_genotypes ==========
#
#' @title Gamete and gametic frequency of polyploid genotypes for a single locus (for "tetraploid" population)
#' @description Taking a string input of genotype of an individual, function finds the gamete types possible
#' and its probability in a random meiosis event.
#'
#' @param x A character vector of length 4 indicating allelic composition of the locus in the tetraploid genotype
#'
#' @importFrom tibble enframe
#' @importFrom combinat permn
#' @importFrom dplyr mutate ungroup count group_by
#' @importFrom tidyr unnest
#' @importFrom purrr map map_chr
#' @importFrom utils combn
#'
#' @return A tbl
#' @export
#'
#' @examples
#' require(tidyverse)
#' gamete_genotypes(c("a+", "a+", "a", "a")) %>%
#'   # since a+a and aa+ are indistinguishible in phenotype, these two classes can be merged
#'   mutate(v_comb = fct_recode(v_comb, "aa+" = "a+a")) %>%
#'   group_by(v_comb) %>%
#'   summarise_at(c("n", "prop_n"), sum)
#'
#' \dontrun{
#' # notice how quickly homolog permutation increases as ploidy level increase
#' # with just 8 homologues there are ~40k permutations,
#' combinat::permn(c("a+", "a+", "a", "a", "a", "a+", "a+", "a+"), c) %>% length()
#' # but note that unique combinations are far less: 56
#' combinat::permn(c("a+", "a+", "a", "a", "a", "a+", "a+", "a+"), c) %>% unique() %>% length()
#' }
#'
gamete_genotypes <- function(x){
  ggenotypes <- combinat::permn(x, c) %>%
    unique() %>%
    tibble::enframe() %>%
    dplyr::mutate(v_comb = purrr::map(value, ~utils::combn(.x, 2, function(x)paste0(x, collapse = "")))) %>%
    dplyr::mutate(value = purrr::map_chr(value, ~paste0(.x, collapse = ""))) %>%
    tidyr::unnest(cols = v_comb) %>%
    dplyr::group_by(v_comb) %>%
    dplyr::count(sort = T) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop_n = n/sum(n))
  return(ggenotypes)
}
