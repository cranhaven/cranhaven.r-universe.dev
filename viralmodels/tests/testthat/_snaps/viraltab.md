# viraltab() works

    Code
      viraltab(traindata, semilla, target, viralvars, logbase, pliegues, repeticiones,
        rejilla, rank_output = TRUE)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry
    Condition
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
    Output
                          wflow_id              .config .metric   mean std_err n
      1             normalized_KNN Preprocessor1_Model1    rmse 134.01   30.07 2
      2             normalized_KNN Preprocessor1_Model1     rsq   0.74    0.03 2
      3  normalized_neural_network Preprocessor1_Model1    rmse 145.47    5.50 2
      4  normalized_neural_network Preprocessor1_Model1     rsq   0.74    0.05 2
      5         simple_CART_bagged Preprocessor1_Model1    rmse 148.07    8.72 2
      6         simple_CART_bagged Preprocessor1_Model1     rsq   0.72    0.06 2
      7                  simple_rf Preprocessor1_Model1    rmse 155.20   38.74 2
      8                  simple_rf Preprocessor1_Model1     rsq   0.81    0.06 2
      9              full_quad_KNN Preprocessor1_Model1    rmse 157.13   35.85 2
      10             full_quad_KNN Preprocessor1_Model1     rsq   0.65    0.01 2
      11             simple_Cubist Preprocessor1_Model1    rmse 164.21   13.83 2
      12             simple_Cubist Preprocessor1_Model1     rsq   0.68    0.03 2
      13       normalized_SVM_poly Preprocessor1_Model1    rmse 183.17   17.60 2
      14       normalized_SVM_poly Preprocessor1_Model1     rsq   0.52    0.01 2
      15     normalized_SVM_radial Preprocessor1_Model1    rmse 231.96   36.40 2
      16     normalized_SVM_radial Preprocessor1_Model1     rsq   0.67    0.01 2
      17      full_quad_linear_reg Preprocessor1_Model1    rmse 469.64  293.02 2
      18      full_quad_linear_reg Preprocessor1_Model1     rsq   0.29    0.26 2
               preprocessor            model rank
      1              recipe nearest_neighbor    1
      2              recipe nearest_neighbor    1
      3              recipe              mlp    2
      4              recipe              mlp    2
      5  workflow_variables         bag_tree    3
      6  workflow_variables         bag_tree    3
      7  workflow_variables      rand_forest    4
      8  workflow_variables      rand_forest    4
      9              recipe nearest_neighbor    5
      10             recipe nearest_neighbor    5
      11 workflow_variables     cubist_rules    6
      12 workflow_variables     cubist_rules    6
      13             recipe         svm_poly    7
      14             recipe         svm_poly    7
      15             recipe          svm_rbf    8
      16             recipe          svm_rbf    8
      17             recipe       linear_reg    9
      18             recipe       linear_reg    9

