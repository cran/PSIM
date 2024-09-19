#' Preference Selection Index Method PSI
#'
#' Implementation of An PREFERENCE SELECTION INDEX METHOD - PSI
#' More information about the method at https://doi.org/10.1016/j.matdes.2009.11.020
#' More information about the implementation at https://github.com/luana1909/PSIM/blob/main/DESCRIPTION
#' The goal is to determine the weights of criteria

#' @importFrom utils count.fields read.csv read.csv2
#' @importFrom dplyr mutate ungroup across select
#' @importFrom matrixStats rowMins rowMaxs
#' @import magrittr
#' @import tidyverse
#' @param data A numeric data matrix, columns are the criteria, rows are the alternatives
#' @param optimization A character vector with definition of minimization or maximization for each criterion, expected 'min' or 'max' only
#' @returns dataframe with 3 columns: critério, phi_j and psi_j
#' @export
#' @examples
#' optimizations <- c("min","min", "max", "max") #"min" and "max" should be all lowercase
#' decision_matrix <- data.frame(criterio1 = c(7000, 15000, 20000),
#'                               criterio2 = c(700, 800, 1000),
#'                               criterio3 = c(280, 300, 180),
#'                               criterio4 = c(120, 880, 1200))
#' result <- psicalc(decision_matrix, optimizations)
#'@name psicalc
# Include global variables to avoid NOTE warnings
utils::globalVariables(c("id_criterio", "criterio", "media_criterio", "PV_j", "phi_j", "psi_j","%>%","mutate","select","ungroup","across"))

psicalc <- function(data, optimization){



  data <- as.data.frame(data)
  # Transpõe matriz de decisão e transforma em dataframe
  matriz_decisao <- t(data) %>% as.data.frame()
  # Cria coluna identificando se cada critério é de máximo ou mínimo
  matriz_decisao$criterio <- optimization

  # Cria coluna com o id de cada critério
  matriz_decisao$id_criterio <- seq_along(matriz_decisao$criterio)
  #matriz_decisao$id_criterio <- 1:nrow(matriz_decisao)

  # Calcula máximo/mínimo de cada critério e normaliza valores
  matriz_pad <- matriz_decisao %>%
    dplyr::mutate(min = rowMins(matriz_decisao %>% select(-c(id_criterio, criterio)) %>% as.matrix()),
           max = rowMaxs(matriz_decisao %>% select(-c(id_criterio, criterio)) %>% as.matrix())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(-c(id_criterio, criterio, min, max),
                  ~ifelse(criterio == 'max', . / max, min / .)))

  # Calcula o desvio de cada observação em relação à média do critério
  matriz_desvios <- matriz_pad %>%
    dplyr::mutate(media_criterio = rowMeans(matriz_pad %>% select(-c(id_criterio, criterio, min, max)))) %>%
    dplyr::mutate(across(-c(id_criterio, criterio, min, max, media_criterio),
                  ~(. - media_criterio)^2))




  # Calcula a soma de cada critério (PV_j), subtrai 1 (phi_j) e normaliza para calcular o psi_j
  matriz_desvios %>%
    dplyr::mutate(PV_j = rowSums(matriz_desvios %>% select(-c(id_criterio, criterio, min, max, media_criterio)))) %>%
    dplyr::mutate(phi_j = 1 - PV_j) %>%
    dplyr::mutate(psi_j = phi_j/sum(phi_j)) %>%
    dplyr::select(criterio, phi_j, psi_j) %>%
    return()
}

