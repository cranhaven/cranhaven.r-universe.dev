#' Selected results table by P value
#'
#' \code{select_output} returns selected SNP information by P value.
#'
#' @param input Input results from xlink fit.
#' @param pv_thold P value threshold for output.
#' @return It returns estimated parameters, confidence interval, P value, MAF and Best model information.
#' @examples
#' Covars<-c("Age","Smoking","Treatment")
#' SNPs<-c("snp_1","snp_2","snp_3")
#' result<-xlink_fit(os="OS",ostime ="OS_time",snps=SNPs,gender ="gender",covars=Covars,
#'                   option =list(type="all",MAF_v=0.05), model="survival", data = Rdata)
#' select_output(input=result,pv_thold=10^-5)
#' @seealso \code{\link{xlink_fit}{xlink}} for input results.
#' @export
select_output <- function(input, pv_thold = 1) {

    var_name <- rownames(input[1][[1]][[1]])

    if (length(var_name) != 0) {

        model_def <- ("(Intercept)" %in% var_name) * 1
        snp_num <- length(input)

        if (model_def == 0) {
            p_value_ouput <- function(x) {
                R <- input[x][[1]][[1]][1, 3]
                return(R)
            }

            snp_name <- function(x) {
                R <- rownames(input[x][[1]][[1]][1, ])
                return(R)
            }

            all_P_vec <- unlist(lapply(1:snp_num, p_value_ouput))
            if (sum(all_P_vec <= pv_thold) !=0){
            select_snp_vec <- (which(all_P_vec <= pv_thold) - 1) * length(var_name) + 1
            snp_name_vec <- unlist(lapply(1:snp_num, snp_name))
            }  else {
              return("No SNP's P value satisfies pv_thold requirement.")
            }

        } else {
            p_value_ouput <- function(x) {
                R <- input[x][[1]][[1]][2, 3]
                return(R)
            }

            snp_name <- function(x) {
                R <- rownames(input[x][[1]][[1]][2, ])
                return(R)
            }
            all_P_vec <- unlist(lapply(1:snp_num, p_value_ouput))

            if (sum(all_P_vec <= pv_thold) !=0){
            select_snp_vec <- (which(all_P_vec <= pv_thold) - 1) * length(var_name) + 2
            snp_name_vec <- unlist(lapply(1:snp_num, snp_name))
            } else {
              return("No SNP's P value satisfies pv_thold requirement.")
            }

        }

        table_out <- do.call(Map, c(rbind, input))[1][[1]][select_snp_vec, ]
        rownames(table_out) <- snp_name_vec [which(all_P_vec <= pv_thold)]
        results <- table_out


    } else {
      var_name <- rownames(input[1][[1]][[1]][[1]])
      model_def <- ("(Intercept)" %in% var_name) * 1
      snp_num <- length(input)

      if (model_def == 0) {

        best_model_vec <- function(x) {
          R1 <- input[x][[1]][[4]][[1]]
          R2 <- which((names(input[1][[1]]) == R1))

          P_v_b <- input[x][[1]][[R2]][[1]][1, 3]

          if (P_v_b <= pv_thold) {
            if (R2 == 3) {
              gamma <- input[x][[1]][[3]][[3]]
            } else {
              gamma <- NA
            }
            R4 <- P_v_b
            R5 <- cbind(input[x][[1]][[R2]][[1]][1, ], R1, gamma)

            return(R5)

          } else {
            return(NULL)
          }
        }

        all_best_model_vec <- lapply(1:snp_num, best_model_vec)
        table_out = as.data.frame(all_best_model_vec[1])
        for (i in 2:length(all_best_model_vec)) {
          table_out <- rbind(table_out, as.data.frame(all_best_model_vec[i]))
         }
        table_out <- cbind(rownames(table_out), table_out)

        if ( dim(table_out)[1]!=0) {
          colnames(table_out) <- c("SNP", "Hazard Ratio", "Confidence Interval (95%)", "P Value", "MAF", "Best model", "Gamma")
          rownames(table_out) <- c()
          results <- table_out
        } else {
          return("No SNP's P value satisfies pv_thold requirement.")
        }

      } else {

        best_model_vec <- function(x) {
          R1 <- input[x][[1]][[4]][[1]]
          R2 <- which((names(input[1][[1]]) == R1))

          P_v_b <- input[x][[1]][[R2]][[1]][2, 3]
          if (P_v_b <= pv_thold) {
            if (R2 == 3) {
              gamma <- input[x][[1]][[3]][[3]]
            } else {
              gamma <- NA
            }
            R4 <- P_v_b
            R5 <- cbind(input[x][[1]][[R2]][[1]][2, ], R1, gamma)
            return(R5)

          } else {
            return(NULL)
          }
        }

        all_best_model_vec <- lapply(1:snp_num, best_model_vec)
        table_out = as.data.frame(all_best_model_vec[1])
        for (i in 2:length(all_best_model_vec)) {
          table_out <- rbind(table_out, as.data.frame(all_best_model_vec[i]))
        }
        table_out <- cbind(rownames(table_out), table_out)
        if ( dim(table_out)[1]!=0) {
        colnames(table_out) <- c("SNP", "Estimate", "Confidence Interval (95%)", "P Value", "MAF", "Best model", "Gamma")
        rownames(table_out) <- c()
        results <- table_out
        } else {
          return("No SNP's P value satisfies pv_thold requirement.")
        }

      }

    }

    return(results)
}



