# Implementation in R of the PSI (PREFERENCE SELECTION INDEX METHOD PSI)

https://github.com/luana1909/PSIM/blob/main/README.md


This project consists of the implementation of the method in R, its tests, the CRAN package and a web application written in R Shiny.

If you just want to run the method by reading the data contained in a excelfile, use the application hosted at shinyapps.io.
[Open rwisp on shinyapps.io] ( https://luanadeazevedo.shinyapps.io/criteriosdedecisao/)

The web application repository is 
[github.com/luana1909/PSIM] (https://github.com/luana1909/PSIM)

## Reference

MANIYA, Kalpesh; BHATT, M. G. A selection of material using a novel type decision-making method: Preference selection index method. Materials & Design, [S. l.], v. 31, n. 4, p. 1785–1789, 2010. DOI: 10.1016/j.matdes.2009.11.020.

Abstract: The Preference Selection Index (PSI) method is a multicriteria approach (MCDA and MCDM) that stands out for not requiring the assignment of relative importance between attributes. This method is compared with other established methods such as GTMA and TOPSIS.
As an innovative and suitable tool for multicriteria problems, PSI simplifies the decision-making process by eliminating the need to determine the relative importance of attributes. The PSI method calculates an overall preference value for each attribute and uses these values to determine a preference selection index for each alternative. The alternative with the highest index is then selected as the best option.

## For Use

### Install option 1 - from github
```
library("devtools");
install_github("luana1909/PSIM");
library("PSIM")
...
```

### Install option 2 - from CRAN
```
install.packages("PSIM")
library("PSIM")
...
```

### Calculation option 1 - from vars
```
optimizations <- c("min","min", "max", "max") 
#"min" and "max" should be all lowercase
decision_matrix <- data.frame(criterio1 = c(7000, 15000, 20000),
criterio2 = c(700, 800, 1000),
criterio3 = c(280, 300, 180),
criterio4 = c(120, 880, 1200))
 result <- psicalc(data, optimizations)


