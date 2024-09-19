# tests/testthat/test_psicalc.R


library(testthat)
library(matrixStats)
library(dplyr)
library(magrittr)
# Dados de entrada para o teste
data_test <- data.frame(
  criterio1 = c(7000, 15000, 20000),
  criterio2 = c(700, 800, 1000),
  criterio3 = c(280, 300, 180),
  criterio4 = c(120, 880, 1200)
)

# Tipo de otimização para cada critério
optimization_test <- c("min", "min", "max", "max")

# Resultado esperado#result
expected_result <- data.frame(
  criterio = c("min", "min", "max", "max"),
  phi_j = c(0.759814815, 0.954583333,	0.908148148,	0.572592593),  # valores calculados manualmente ou com uma referência conhecida
  psi_j = c(0.237803376, 0.298761139, 0.284228066, 0.179207419)   # valores calculados manualmente ou com uma referência conhecida
)

# Garantir que os nomes das linhas e dos vetores de valores estejam no formato correto
rownames(expected_result) <- NULL
names(expected_result$phi_j) <- NULL
names(expected_result$psi_j) <- NULL



# Função para execução do teste
test_that("Função psicalc produz o resultado esperado", {
  result <- psicalc(data_test, optimization_test)
  # Garantir que os nomes das linhas e dos vetores de valores estejam no formato correto
  rownames(result) <- NULL
  names(result$phi_j) <- NULL
  names(result$psi_j) <- NULL

    expect_equal(result, expected_result, tolerance = 1e-8)
})

