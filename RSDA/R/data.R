#' @name abalone
#' @title SODAS XML data file.
#' @description Example of SODAS XML data file converted in a CSV file in RSDA format.
#' @usage data(abalone)
#' @source http://www.info.fundp.ac.be/asso/sodaslink.htm
#' @references Bock H-H. and Diday E. (eds.) (2000).Analysis of Symbolic Data. Exploratory methods
#' for extracting statistical information fromcomplex data. Springer, Germany.
#' @examples
#' data(abalone)
#' res <- sym.pca(abalone, 'centers')
#' plot(res, choix = "ind")
#' plot(res, choix = "var")
#' @keywords datasets
"abalone"

#' @name Cardiological
#' @title Cardiological data example
#' @description Cardiological interval data example.
#' @usage data(Cardiological)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(Cardiological)
#' res.cm <- sym.lm(formula = Pulse~Syst+Diast, sym.data = Cardiological, method = 'cm')
#' pred.cm <- sym.predict(res.cm, Cardiological)
#' RMSE.L(Cardiological$Pulse, pred.cm$Fitted)
#' RMSE.U(Cardiological$Pulse,pred.cm$Fitted)
#' R2.L(Cardiological$Pulse,pred.cm$Fitted)
#' R2.U(Cardiological$Pulse,pred.cm$Fitted)
#' deter.coefficient(Cardiological$Pulse,pred.cm$Fitted)
#' @keywords datasets
"Cardiological"

#' @name cardiologicalv2
#' @title Cardiological data example
#' @description Cardiological interval data example.
#' @usage data(Cardiological)
#' @references Billard L. and  Diday E. (2006).Symbolic data analysis:
#' Conceptual statistics and data mining. Wiley, Chichester.
#' @keywords datasets
"cardiologicalv2"

#' @name ex1_db2so
#' @aliases ex1_db2so
#' @title Data example to generate symbolic objets
#' @description This is a small data example to generate symbolic objets.
#' @usage data(ex1_db2so)
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @examples
#' data(ex1_db2so)
#' ex1 <- ex1_db2so
#' result <- classic.to.sym(
#'   x = ex1_db2so,
#'   concept = c(state, sex),
#'   variables = c(county, group, age),
#'   county = mean(county),
#'   age_hist = sym.histogram(age, breaks = pretty(ex1_db2so$age, 5))
#' )
#' result
#' @keywords datasets
"ex1_db2so"

#' @name example1
#' @aliases example1
#' @title Data Example 1
#' @description This a symbolic data table with variables of continuos, interval,
#' histogram and set types.
#' @usage data(example1)
#' @format The labels $C means that follows a continuous variable, $I means an interval
#' variable, $H means a histogram variables and $S means set variable. In the
#' first row each labels should be follow of a name to variable and to the case
#' of histogram a set variables types the names of the modalities (categories).
#' In data rows for continuous variables we have just one value, for interval
#' variables we have the minimum and the maximum of the interval, for histogram
#' variables we have the number of modalities and then the probability of each
#' modality and for set variables we have the cardinality of the set and next
#' the elements of the set. \cr

#' The format is the *.csv file is: \cr

#'       $C   F1 $I F2 F2 $M F3  M1  M2  M3 $S F4 e a 2 3 g b 1 4 i k c d \cr
#' Case1 $C  2.8 $I  1  2 $M  3 0.1 0.7 0.2 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 \cr
#' Case2 $C  1.4 $I  3  9 $M  3 0.6 0.3 0.1 $S 12 0 1 0 0 0 1 0 0 0 0 1 1 \cr
#' Case3 $C  3.2 $I -1  4 $M  3 0.2 0.2 0.6 $S 12 0 0 1 0 0 1 1 0 0 0 1 0 \cr
#' Case4 $C -2.1 $I  0  2 $M  3 0.9 0.0 0.1 $S 12 0 1 0 1 0 0 0 1 0 0 1 0 \cr
#' Case5 $C -3.0 $I -4 -2 $M  3 0.6 0.0 0.4 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 \cr

#' The internal format is:\cr

#'   $N \cr

#' [1] 5 \cr

#' $M \cr

#' [1] 4 \cr

#' $sym.obj.names \cr

#' [1] 'Case1' 'Case2' 'Case3' 'Case4' 'Case5' \cr

#' $sym.var.names \cr

#' [1] 'F1' 'F2' 'F3' 'F4' \cr

#' $sym.var.types

#' [1] '$C' '$I' '$H' '$S' \cr

#' $sym.var.length \cr

#' [1] 1 2 3 4 \cr

#' $sym.var.starts \cr

#' [1]  2  4  8 13 \cr

#' $meta \cr
#'
#'       $C   F1 $I F2 F2 $M F3  M1  M2  M3 $S F4 e a 2 3 g b 1 4 i k c d
#' Case1 $C  2.8 $I  1  2 $M  3 0.1 0.7 0.2 $S 12 1 0 0 0 1 0 0 0 1 1 0 0
#' Case2 $C  1.4 $I  3  9 $M  3 0.6 0.3 0.1 $S 12 0 1 0 0 0 1 0 0 0 0 1 1
#' Case3 $C  3.2 $I -1  4 $M  3 0.2 0.2 0.6 $S 12 0 0 1 0 0 1 1 0 0 0 1 0
#' Case4 $C -2.1 $I  0  2 $M  3 0.9 0.0 0.1 $S 12 0 1 0 1 0 0 0 1 0 0 1 0
#' Case5 $C -3.0 $I -4 -2 $M  3 0.6 0.0 0.4 $S 12 1 0 0 0 1 0 0 0 1 1 0 0

#' $data \cr
#'         F1 F2 F2.1  M1  M2  M3 e a 2 3 g b 1 4 i k c d
#' Case1  2.8  1    2 0.1 0.7 0.2 1 0 0 0 1 0 0 0 1 1 0 0
#' Case2  1.4  3    9 0.6 0.3 0.1 0 1 0 0 0 1 0 0 0 0 1 1
#' Case3  3.2 -1    4 0.2 0.2 0.6 0 0 1 0 0 1 1 0 0 0 1 0
#' Case4 -2.1  0    2 0.9 0.0 0.1 0 1 0 1 0 0 0 1 0 0 1 0
#' Case5 -3.0 -4   -2 0.6 0.0 0.4 1 0 0 0 1 0 0 0 1 1 0 0
#'
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @examples
#' data(example1)
#' example1
#' @keywords datasets
"example1"

#' @name example2
#' @aliases example2
#' @title Data Example 2
#' @description This a symbolic data table with variables of continuos, interval,
#' histogram and set types.
#' @usage data(example2)
#' @format
#'       $C   F1 $I F2 F2 $M F3  M1  M2  M3 $C   F4 $S F5 e a 2 3 g b 1 4 i k c d\cr
#' Case1 $C  2.8 $I  1  2 $M  3 0.1 0.7 0.2 $C  6.0 $S 12 1 0 0 0 1 0 0 0 1 1 0 0\cr
#' Case2 $C  1.4 $I  3  9 $M  3 0.6 0.3 0.1 $C  8.0 $S 12 0 1 0 0 0 1 0 0 0 0 1 1\cr
#' Case3 $C  3.2 $I -1  4 $M  3 0.2 0.2 0.6 $C -7.0 $S 12 0 0 1 0 0 1 1 0 0 0 1 0\cr
#' Case4 $C -2.1 $I  0  2 $M  3 0.9 0.0 0.1 $C  0.0 $S 12 0 1 0 1 0 0 0 1 0 0 1 0\cr
#' Case5 $C -3.0 $I -4 -2 $M  3 0.6 0.0 0.4 $C -9.5 $S 12 1 0 0 0 1 0 0 0 1 1 0 0\cr
#'
#' @examples
#' data(example2)
#' example2
#' @keywords datasets
"example2"


#' @name example3
#' @aliases example3
#' @title Data Example 3
#' @description This a symbolic data table with variables of continuos, interval,
#' histogram and set types.
#' @usage data(example3)
#' @format
#'       $C   F1 $I F2 F2 $M F3  M1  M2  M3 $C   F4 $S F5 e a 2 3 g b 1 4 i k c d $I     F6    F6 $I F7 F7
#' Case1 $C  2.8 $I  1  2 $M  3 0.1 0.7 0.2 $C  6.0 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 $I   0.00 90.00 $I  9 24
#' Case2 $C  1.4 $I  3  9 $M  3 0.6 0.3 0.1 $C  8.0 $S 12 0 1 0 0 0 1 0 0 0 0 1 1 $I -90.00 98.00 $I -9  9
#' Case3 $C  3.2 $I -1  4 $M  3 0.2 0.2 0.6 $C -7.0 $S 12 0 0 1 0 0 1 1 0 0 0 1 0 $I  65.00 90.00 $I 65 70
#' Case4 $C -2.1 $I  0  2 $M  3 0.9 0.0 0.1 $C  0.0 $S 12 0 1 0 1 0 0 0 1 0 0 1 0 $I  45.00 89.00 $I 25 67
#' Case5 $C -3.0 $I -4 -2 $M  3 0.6 0.0 0.4 $C -9.5 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 $I  20.00 40.00 $I  9 40
#' Case6 $C  0.1 $I 10 21 $M  3 0.0 0.7 0.3 $C -1.0 $S 12 1 0 0 0 0 0 1 0 1 0 0 0 $I   5.00  8.00 $I  5  8
#' Case7 $C  9.0 $I  4 21 $M  3 0.2 0.2 0.6 $C  0.5 $S 12 1 1 1 0 0 0 0 0 0 0 0 0 $I   3.14  6.76 $I  4  6
#'
#' @examples
#' data(example3)
#' example3
#' @keywords datasets
"example3"

#' @name example4
#' @aliases example4
#' @title Data Example 4
#' @description
#' data(example4)
#' example4
#' @usage data(example4)
#' @format
#'       $C  2.8 $I  1  2 $M 3 0.1 0.7 0.2 $C    6 $S F4 e a 2 3 g b 1 4 i k c d $I      0     90
#' Case2 $C  1.4 $I  3  9 $M 3 0.6 0.3 0.1 $C  8.0 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 $I -90.00  98.00
#' Case3 $C  3.2 $I -1  4 $M 3 0.2 0.2 0.6 $C -7.0 $S 12 0 1 0 0 0 1 0 0 0 0 1 1 $I  65.00  90.00
#' Case4 $C -2.1 $I  0  2 $M 3 0.9 0.0 0.1 $C  0.0 $S 12 0 0 1 0 0 1 1 0 0 0 1 0 $I  45.00  89.00
#' Case5 $C -3.0 $I -4 -2 $M 3 0.6 0.0 0.4 $C -9.5 $S 12 0 1 0 1 0 0 0 1 0 0 1 0 $I  90.00 990.00
#' Case6 $C  0.1 $I 10 21 $M 3 0.0 0.7 0.3 $C -1.0 $S 12 1 0 0 0 1 0 0 0 1 1 0 0 $I   5.00   8.00
#' Case7 $C  9.0 $I  4 21 $M 3 0.2 0.2 0.6 $C  0.5 $S 12 1 1 0 0 0 0 1 0 0 0 0 1 $I   3.14   6.76
#'
#' @examples
#' data(example4)
#' example4
#' @keywords datasets
"example4"

#' @name example5
#' @aliases example5
#' @title Data Example 5
#' @description
#' This a symbolic data matrix wint continuos, interval, histograma a set data types.
#' @usage data(example5)
#' @format
#' $H F0 M01 M02 $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4 \cr

#' Case1 $H  2 0.1 0.9 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i \cr

#' Case2 $H  2 0.7 0.3 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d \cr

#' Case3 $H  2 0.0 1.0 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c \cr

#' Case4 $H  2 0.2 0.8 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a \cr

#' Case5 $H  2 0.6 0.4 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k \cr
#'
#' @examples
#' data(example5)
#' example5
#' @keywords datasets
"example5"

#' @name example6
#' @aliases example6
#' @title Data Example 6
#' @description
#' This a symbolic data matrix wint continuos, interval, histograma a set data types.
#' @usage data(example6)
#' @format
#'
#'       $C   F1 $M F2  M1  M2  M3  M4  M5 $I F3 F3 $M F4  M1  M2  M3 $C   F5 $S F4 e a 2 3 g b 1 4 i k c d
#' Case1 $C  2.8 $M  5 0.1 0.1 0.1 0.1 0.6 $I  1  2 $M  3 0.1 0.7 0.2 $C  6.0 $S 12 1 0 0 0 1 0 0 0 1 1 0 0
#' Case2 $C  1.4 $M  5 0.1 0.1 0.1 0.1 0.6 $I  3  9 $M  3 0.6 0.3 0.1 $C  8.0 $S 12 0 1 0 0 0 1 0 0 0 0 1 1
#' Case3 $C  3.2 $M  5 0.1 0.1 0.1 0.1 0.6 $I -1  4 $M  3 0.2 0.2 0.6 $C -7.0 $S 12 0 0 1 0 0 1 1 0 0 0 1 0
#' Case4 $C -2.1 $M  5 0.1 0.1 0.1 0.1 0.6 $I  0  2 $M  3 0.9 0.0 0.1 $C  0.0 $S 12 0 1 0 1 0 0 0 1 0 0 1 0
#' Case5 $C -3.0 $M  5 0.1 0.1 0.1 0.1 0.6 $I -4 -2 $M  3 0.6 0.0 0.4 $C -9.5 $S 12 1 0 0 0 1 0 0 0 1 1 0 0
#'
#' @examples
#' data(example6)
#' example6
#' @keywords datasets
"example6"

#' @name example7
#' @aliases example7
#' @title Data Example 7
#' @description
#' This a symbolic data matrix wint continuos, interval, histograma a set data types.
#' @usage data(example6)
#' @format
#' $C   F1 $H F2  M1  M2  M3  M4  M5 $I F3 F3 $H F4  M1  M2  M3 $C   F5 \cr

#' Case1 $C  2.8 $H  5 0.1 0.2 0.3 0.4 0.0 $I  1  2 $H  3 0.1 0.7 0.2 $C  6.0 \cr

#' Case2 $C  1.4 $H  5 0.2 0.1 0.5 0.1 0.2 $I  3  9 $H  3 0.6 0.3 0.1 $C  8.0 \cr

#' Case3 $C  3.2 $H  5 0.1 0.1 0.2 0.1 0.5 $I -1  4 $H  3 0.2 0.2 0.6 $C -7.0 \cr

#' Case4 $C -2.1 $H  5 0.4 0.1 0.1 0.1 0.3 $I  0  2 $H  3 0.9 0.0 0.1 $C  0.0 \cr

#' Case5 $C -3.0 $H  5 0.6 0.1 0.1 0.1 0.1 $I -4 -2 $H  3 0.6 0.0 0.4 $C -9.5 \cr
#'
#' @examples
#' data(example7)
#' example7
#' @keywords datasets
"example7"

#' @name ex_cfa1
#' @aliases ex_cfa1
#' @title Correspondence Analysis Example
#' @description Correspondence Analysis for Symbolic MultiValued Variables example.
#' @usage data(ex_cfa1)
#' @references Rodriguez, O. (2011).
#' Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic
#' Data Analysis Namur, Belgium
#' @keywords datasets
"ex_cfa1"

#' @name ex_cfa2
#' @aliases ex_cfa2
#' @title Correspondence Analysis Example
#' @description Correspondence Analysis for Symbolic MultiValued Variables example.
#' @usage data(ex_cfa2)
#' @references Rodriguez, O. (2011).
#' Correspondence Analysis for Symbolic MultiValued Variables. Workshop in Symbolic
#' Data Analysis Namur, Belgium
#' @keywords datasets
"ex_cfa2"

#' @name ex_mcfa1
#' @title Multiple Correspondence Analysis Example
#' @description example for the sym.mcfa function.
#' @usage data(ex_mcfa1)
#' @examples
#' data("ex_mcfa1")
#' sym.table <- classic.to.sym(ex_mcfa1,
#'                             concept = suspect,
#'                             hair = sym.set(hair),
#'                             eyes = sym.set(eyes),
#'                             region = sym.set(region))
#'
#' res <- sym.mcfa(sym.table, c(1,2))
#' mcfa.scatterplot(res[,1], res[,2], sym.data = sym.table, pos.var = c(1,2))
#'
#' @keywords datasets
"ex_mcfa1"

#' @name ex_mcfa2
#' @title Multiple Correspondence Analysis Example
#' @description example for the sym.mcfa function.
#' @usage data(ex_mcfa2)
#' @examples
#' data("ex_mcfa2")
#'
#'ex <- classic.to.sym(ex_mcfa2,
#'                     concept = employee_id,
#'                      variables = c(employee_id, salary, region, evaluation, years_worked),
#'                      salary = sym.set(salary),
#'                      region = sym.set(region),
#'                      evaluation = sym.set(evaluation),
#'                      years_worked = sym.set(years_worked))
#'
#'res <- sym.mcfa(ex, c(1,2,3,4))
#'mcfa.scatterplot(res[,1], res[,2], sym.data = ex, pos.var = c(1,2,3,4))
#'
#' @keywords datasets
"ex_mcfa2"

#' @name lynne1
#' @aliases lynne1
#' @title Symbolic interval data example.
#' @description
#' Symbolic data matrix with all the variables of interval type.
#' @usage data(lynne1)
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(lynne1)
#' lynne1
#' @keywords datasets
"lynne1"

#' @name oils
#' @aliases olils
#' @title Ichino Oils example data.
#' @description
#' Symbolic data matrix with all the variables of interval type.
#' @usage data(oils)
#' @format
#' $I   GRA   GRA $I FRE FRE $I IOD IOD $I SAP SAP \cr

#' L  $I 0.930 0.935 $I -27 -18 $I 170 204 $I 118 196 \cr

#' P  $I 0.930 0.937 $I  -5  -4 $I 192 208 $I 188 197 \cr

#' Co $I 0.916 0.918 $I  -6  -1 $I  99 113 $I 189 198 \cr

#' S  $I 0.920 0.926 $I  -6  -4 $I 104 116 $I 187 193 \cr

#' Ca $I 0.916 0.917 $I -25 -15 $I  80  82 $I 189 193 \cr

#' O  $I 0.914 0.919 $I   0   6 $I  79  90 $I 187 196 \cr

#' B  $I 0.860 0.870 $I  30  38 $I  40  48 $I 190 199 \cr

#' H  $I 0.858 0.864 $I  22  32 $I  53  77 $I 190 202 \cr
#'
#' @references
#' Cazes P., Chouakria A., Diday E. et Schektman Y. (1997).  Extension de l'analyse en
#' composantes principales a des donnees de type intervalle, Rev. Statistique Appliquee,
#' Vol. XLV Num. 3 pag. 5-24, France.
#' @examples
#' data(oils)
#' oils
#' @keywords datasets
"oils"



#' @name ex_mcfa1
#' @title Multiple Correspondence Analysis Example
#' @description example for the sym.mcfa function.
#' @examples
#' data("ex_mcfa1")
#' sym.table <- classic.to.sym(
#'   x = ex_mcfa1,
#'   concept = "suspect",
#'   variables = c(hair, eyes, region),
#'   hair = sym.set(hair),
#'   eyes = sym.set(eyes),
#'   region = sym.set(region)
#' )
#' sym.table
#' @keywords datasets
"ex_mcfa1"

#' @name USCrime
#' @aliases USCrime
#' @title Us crime classic data table
#' @description
#' Us crime classic data table that can be used to generate symbolic data tables.
#' @usage data(USCrime)
#' @source http://archive.ics.uci.edu/ml/
#' @references
#' HASTIE, T., TIBSHIRANI, R. and FRIEDMAN, J. (2008). The Elements of Statistical Learning:
#' Data Mining, Inference and Prediction. New York: Springer.
#' @examples
#' \dontrun{
#' data(USCrime)
#' us.crime <- USCrime
#' dim(us.crime)
#' head(us.crime)
#' summary(us.crime)
#' names(us.crime)
#' nrow(us.crime)
#' result <- classic.to.sym(us.crime,
#'   concept = "state",
#'   variables = c(NumInShelters, NumImmig),
#'   variables.types = c(
#'     NumInShelters = type.histogram(),
#'     NumImmig = type.histogram()
#'   )
#' )
#' result
#' }
#' @keywords datasets
"USCrime"

#' @name int_prost_test
#' @aliases int_prost_test
#' @title Linear regression model data example.
#' @description
#' Linear regression model interval-valued data example.
#' @usage data(int_prost_test)
#' @references
#' HASTIE, T., TIBSHIRANI, R. and FRIEDMAN, J. (2008). The Elements of Statistical Learning:
#' Data Mining, Inference and Prediction. New York: Springer.
#' @keywords datasets
"int_prost_test"


#' @name int_prost_train
#' @aliases int_prost_train
#' @title Linear regression model data example.
#' @description
#' Linear regression model interval-valued data example.
#' @usage data(int_prost_train)
#' @references
#' HASTIE, T., TIBSHIRANI, R. and FRIEDMAN, J. (2008). The Elements of Statistical Learning:
#' Data Mining, Inference and Prediction. New York: Springer.
#' @keywords datasets
"int_prost_train"

#' @name uscrime_int
#' @aliases uscrime_int
#' @title Us crime interval data table.
#' @description
#' Us crime classic data table genetated from uscrime data.
#' @usage data(uscrime_int)
#' @references
#' Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear
#' regression model to symbolic interval data using Ridge Regression, Lasso
#' and Elastic Net methods. The IFCS2013 conference of the International Federation of
#' Classification Societies, Tilburg University Holland.
#' @examples
#' data(uscrime_int)
#' car.data <- uscrime_int
#' res.cm.lasso <- sym.glm(
#'   sym.data = car.data, response = 102, method = "cm", alpha = 1,
#'   nfolds = 10, grouped = TRUE
#' )
#' plot(res.cm.lasso)
#' plot(res.cm.lasso$glmnet.fit, "norm", label = TRUE)
#' plot(res.cm.lasso$glmnet.fit, "lambda", label = TRUE)
#'
#' pred.cm.lasso <- sym.predict(res.cm.lasso, response = 102, car.data)
#' RMSE.L(car.data$ViolentCrimesPerPop, pred.cm.lasso)
#' RMSE.U(car.data$ViolentCrimesPerPop, pred.cm.lasso)
#' R2.L(car.data$ViolentCrimesPerPop, pred.cm.lasso)
#' R2.U(car.data$ViolentCrimesPerPop, pred.cm.lasso)
#' deter.coefficient(car.data$ViolentCrimesPerPop, pred.cm.lasso)
#' @keywords datasets
"uscrime_int"

#' @name uscrime_intv2
#' @aliases uscrime_intv2
#' @title Us crime interval data table.
#' @description
#' Us crime classic data table genetated from uscrime data.
#' @usage data(uscrime_int)
#' @references
#' Rodriguez O. (2013). A generalization of Centre and Range method for fitting a linear
#' regression model to symbolic interval data using Ridge Regression, Lasso
#' and Elastic Net methods. The IFCS2013 conference of the International Federation of
#' Classification Societies, Tilburg University Holland.
#'
#' @keywords datasets
"uscrime_intv2"

#' @name facedata
#' @aliases facedata
#' @title Face Data Example
#' @description
#' Symbolic data matrix with all the variables of interval type.
#' @usage data('facedata')
#' @format
#' $I;AD;AD;$I;BC;BC;......... \cr
#'
#' HUS1;$I;168.86;172.84;$I;58.55;63.39;.........\cr
#' HUS2;$I;169.85;175.03;$I;60.21;64.38;.........\cr
#' HUS3;$I;168.76;175.15;$I;61.4;63.51;.........\cr
#' INC1;$I;155.26;160.45;$I;53.15;60.21;.........\cr
#' INC2;$I;156.26;161.31;$I;51.09;60.07;.........\cr
#' INC3;$I;154.47;160.31;$I;55.08;59.03;.........\cr
#' ISA1;$I;164;168;$I;55.01;60.03;.........\cr
#' ISA2;$I;163;170;$I;54.04;59;.........\cr
#' ISA3;$I;164.01;169.01;$I;55;59.01;.........\cr
#' JPL1;$I;167.11;171.19;$I;61.03;65.01;.........\cr
#' JPL2;$I;169.14;173.18;$I;60.07;65.07;.........\cr
#' JPL3;$I;169.03;170.11;$I;59.01;65.01;.........\cr
#' KHA1;$I;149.34;155.54;$I;54.15;59.14;.........\cr
#' KHA2;$I;149.34;155.32;$I;52.04;58.22;.........\cr
#' KHA3;$I;150.33;157.26;$I;52.09;60.21;.........\cr
#' LOT1;$I;152.64;157.62;$I;51.35;56.22;.........\cr
#' LOT2;$I;154.64;157.62;$I;52.24;56.32;.........\cr
#' LOT3;$I;154.83;157.81;$I;50.36;55.23;.........\cr
#' PHI1;$I;163.08;167.07;$I;66.03;68.07;.........\cr
#' PHI2;$I;164;168.03;$I;65.03;68.12;.........\cr
#' PHI3;$I;161.01;167;$I;64.07;69.01;.........\cr
#' ROM1;$I;167.15;171.24;$I;64.07;68.07;.........\cr
#' ROM2;$I;168.15;172.14;$I;63.13;68.07;.........\cr
#' ROM3;$I;167.11;171.19;$I;63.13;68.03;.........\cr
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' \dontrun{
#' data(facedata)
#' res.vertex.ps <- sym.interval.pc(facedata,'vertex',150,FALSE,FALSE,TRUE)
#' class(res.vertex.ps$sym.prin.curve) <- c('sym.data.table')
#' sym.scatterplot(res.vertex.ps$sym.prin.curve[,1], res.vertex.ps$sym.prin.curve[,2],
#'                 labels=TRUE,col='red',main='PSC Face Data')
#'                 }
#' @keywords datasets
"facedata"

#' @name VeterinaryData
#' @aliases VeterinaryData
#' @title Symbolic interval data example
#' @description
#' Symbolic data matrix with all the variables of interval type.
#' @usage data(VeterinaryData)
#' @format
#' $I Height Height $I Weight Weight \cr
#'
#' 1  $I  120.0  180.0 $I  222.2  354.0\cr
#'
#' 2  $I  158.0  160.0 $I  322.0  355.0\cr
#'
#' 3  $I  175.0  185.0 $I  117.2  152.0\cr
#'
#' 4  $I   37.9   62.9 $I   22.2   35.0\cr
#'
#' 5  $I   25.8   39.6 $I   15.0   36.2\cr
#'
#' 6  $I   22.8   58.6 $I   15.0   51.8\cr
#'
#' 7  $I   22.0   45.0 $I    0.8   11.0\cr
#'
#' 8  $I   18.0   53.0 $I    0.4    2.5\cr
#'
#' 9  $I   40.3   55.8 $I    2.1    4.5\cr
#'
#' 10 $I   38.4   72.4 $I    2.5    6.1\cr
#'
#'
#'
#' @references
#' Billard L. and  Diday E. (2006).
#' Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
#' @examples
#' data(VeterinaryData)
#' VeterinaryData
#' @keywords datasets
"VeterinaryData"


#' @name hardwoodBrito
#' @aliases hardwoodBrito
#' @title Hard Wood Data Example
#' @description
#' Symbolic Histogram matrix.
#' @usage data('hardwoodBrito')
#' @references
#' Brito P. and Dias S. (2022).
#' Analysis of Distributional Data. CRC Press, United States of America.
#' @examples
#' \dontrun{
#' data(hardwoodBrito)
#' hardwoodBrito
#' }
#' @keywords datasets
"hardwoodBrito"
