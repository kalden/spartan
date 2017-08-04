#' Internal function used to calculate the Partial Rank Correlation Coefficient
#' @keywords internal
pcor.mat <- function (x, y, z, cor_method = "p", na.rm = TRUE) {

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z, check.names = FALSE)

  if (dim(z)[2] == 0){
    stop("There should be given data\n")
  }

  data <- data.frame(x, y, z, check.names = FALSE)

  if (na.rm == TRUE) {
    data <- na.omit(data)
  }

  xdata <- na.omit(data.frame(data[, c(1, 2)]), check.names = FALSE)
  Sxx <- cov(xdata, xdata, method = cor_method)

  xzdata <- na.omit(data)
  xdata <- data.frame(xzdata[, c(1, 2)], check.names = FALSE)
  zdata <- data.frame(xzdata[, -c(1, 2)], check.names = FALSE)
  Sxz <- cov(xdata, zdata, method = cor_method)

  zdata <- na.omit(data.frame(data[, -c(1, 2)]), check.names = FALSE)
  Szz <- cov(zdata, zdata, method = cor_method)

  # is Szz positive definite?
  zz.ev <- eigen(Szz)$values
  if (min(zz.ev)[1] < 0) {
    stop("\'Szz\' is not positive definite!\n")
  }

  # partial correlation
  Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)

  rxx.z <- cov2cor(Sxx.z)[1, 2]

  rxx.z
}

#' Internal function used to calculate the Partial Rank Correlation Coefficient
#' @keywords internal
pcor.rec <- function(x, y, z, cor_method = "p", na.rm = TRUE) {

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z, check.names = FALSE)

  if (dim(z)[2] == 0) {
    stop("There should be given data\n")
  }

  data <- data.frame(x, y, z, check.names = FALSE)

  if (na.rm == TRUE) {
    data <- na.omit(data)
  }

  # recursive formula
  if (dim(z)[2] == 1) {
    tdata <- na.omit(data.frame(data[, 1], data[, 2]), check.names = FALSE)
    rxy <- cor(tdata[, 1], tdata[, 2], cor_method)

    tdata <- na.omit(data.frame(data[, 1], data[, -c(1, 2)]),
                     check.names = FALSE)
    rxz <- cor(tdata[, 1], tdata[, 2], cor_method)

    tdata <- na.omit(data.frame(data[, 2], data[, -c(1, 2)]),
                     check.names = FALSE)
    ryz <- cor(tdata[, 1], tdata[, 2], cor_method)

    rxy.z <- (rxy - rxz * ryz) / ( sqrt(1 - rxz ^ 2) *
                                     sqrt(1 - ryz ^ 2) )

    return(rxy.z)
  } else {
    x <- c(data[, 1])
    y <- c(data[, 2])
    z0 <- c(data[, 3])
    zc <- as.data.frame(data[, -c(1, 2, 3)], check.names = FALSE)

    rxy.zc <- pcor.rec(x, y, zc, cor_method = cor_method, na.rm = na.rm)
    rxz0.zc <- pcor.rec(x, z0, zc, cor_method = cor_method, na.rm = na.rm)
    ryz0.zc <- pcor.rec(y, z0, zc, cor_method = cor_method, na.rm = na.rm)

    rxy.z <- (rxy.zc - rxz0.zc * ryz0.zc) /
      ( sqrt(1 - rxz0.zc ^ 2) * sqrt(1 - ryz0.zc ^ 2) )
    return(rxy.z)
  }
}

#' Calculates the PRCC for each parameter at each timepoint, storeing PRCC
#' and P-Value in two different files to make the plot function easier
#'
#' @param FILEPATH Directory containing all PRCC containing result files
#' @param CORCOEFFSOUTPUTFILE Name of the file containing the PRCCS for
#' one timepoint. Assume that the timepoint is appended on to the end (i.e.
#' results_12.csv for hour 12 - the timepoint is appended by spartan so
#' only specify results.csv
#' @param TIMEPOINTS Simulation timepoints to analyse
#' @param MEASURES Simulation output measures being analysed, in a vector
#'
#' @export
lhc_calculatePRCCForMultipleTimepoints <- function(FILEPATH,
                                                   CORCOEFFSOUTPUTFILE,
                                                   TIMEPOINTS, MEASURES) {
  # Calculates the PRCC for each parameter at each timepoint
  # Unlike former Spartan, this stores PRCC and P-Value in 2
  # different files to make the plot function easier

  for (m in 1:length(MEASURES)) {

    MEASURE <- MEASURES[m]
    PRCC_LABEL <- paste(MEASURE, "_Estimate", sep = "")
    PVAL_LABEL <- paste(MEASURE, "_PValue", sep = "")

    PRCCS_OVER_TIME <- NULL
    PVALS_OVER_TIME <- NULL
    PRCC_HEADERS <- c("Parameter Name")
    PVALS_HEADERS <- c("Parameter Name")

    for (t in 1:length(TIMEPOINTS)) {
      TIMEPOINTPROCESSING <- TIMEPOINTS[t]
      CORCOEFFSOUTPUTFILE_FORMAT <- check_file_extension(CORCOEFFSOUTPUTFILE)
      CORCOEFFSOUTPUTFILE_FULL <- paste(substr(CORCOEFFSOUTPUTFILE, 0,
                                               nchar(CORCOEFFSOUTPUTFILE) - 4),
                                        "_", TIMEPOINTPROCESSING, ".",
                                        CORCOEFFSOUTPUTFILE_FORMAT, sep = "")

      if (file.exists(paste(FILEPATH, "/", CORCOEFFSOUTPUTFILE_FULL,
                            sep = ""))) {

        COEFFS_TIMEPOINT <- read.csv(paste(FILEPATH, "/",
                                           CORCOEFFSOUTPUTFILE_FULL,
                                           sep = ""), header = T)

        if (t == 1) {
          # Copy over the parameter name in this instance and  the result
          PRCCS_OVER_TIME <- COEFFS_TIMEPOINT["X"]
          PRCCS_OVER_TIME <- cbind(PRCCS_OVER_TIME,
                                   COEFFS_TIMEPOINT[PRCC_LABEL])
          PVALS_OVER_TIME <- COEFFS_TIMEPOINT["X"]
          PVALS_OVER_TIME <- cbind(PVALS_OVER_TIME,
                                   COEFFS_TIMEPOINT[PVAL_LABEL])
        } else {
          PRCCS_OVER_TIME <- cbind(PRCCS_OVER_TIME,
                                   COEFFS_TIMEPOINT[PRCC_LABEL])
          PVALS_OVER_TIME <- cbind(PVALS_OVER_TIME,
                                   COEFFS_TIMEPOINT[PVAL_LABEL])
        }

        PRCC_HEADERS <- cbind(PRCC_HEADERS, paste(PRCC_LABEL, "_",
                                                  TIMEPOINTPROCESSING,
                                                  sep = ""))
        PVALS_HEADERS <- cbind(PVALS_HEADERS, paste(PVAL_LABEL, "_",
                                                    TIMEPOINTPROCESSING,
                                                    sep = ""))
      } else {
        print(paste("Correlation Coefficients file for Timepoint ",
                    TIMEPOINTPROCESSING, " does not exist", sep = ""))
      }
    }

    # Write the summary to file, if not empty
    if (!is.null(PRCCS_OVER_TIME)){
      # ADD HEADERS TO THE PRCC RESULTS
      colnames(PRCCS_OVER_TIME) <- PRCC_HEADERS
      colnames(PVALS_OVER_TIME) <- PVALS_HEADERS

      RESULTSFILE <- paste(FILEPATH, "/All_Timepoint_PRCCS_", MEASURE, ".csv",
                           sep = "")
      write.csv(PRCCS_OVER_TIME, RESULTSFILE, quote = FALSE, row.names = FALSE)

      RESULTSFILE <- paste(FILEPATH, "/All_Timepoint_PVALS_", MEASURE, ".csv",
                           sep = "")
      write.csv(PVALS_OVER_TIME, RESULTSFILE, quote = FALSE, row.names = FALSE)
    } else {
      print("No Correlation Coefficients to write to file")
    }
  }
}

#' Utility function used to create data structure for coefficient output
#' @keywords internal
lhc_constructcoeff_dataset <-
  function(LHCRESULTFILE, PARAMNAME, PARAMETERS) {
    coeff_data <- NULL
    coeff_headers <- NULL

    for (m in 1:length(PARAMETERS)) {
      if (PARAMETERS[m] != PARAMNAME) {
        coeff_data <- cbind(coeff_data, LHCRESULTFILE[, PARAMETERS[m]])
        coeff_headers <- cbind(coeff_headers, PARAMETERS[m])
      }
      colnames(coeff_data) <- c(coeff_headers)
    }
    return(coeff_data)
  }

#' Deprecated. Use \code{lhc_generatePRCoEffs} instead
#'
#' @inheritParams lhc_generatePRCoEffs
#'
#' @export
lhc_generate_netlogo_PRCoEffs <- function(FILEPATH, PARAMETERS, MEASURES,
                                          LHCSUMMARYFILENAME,
                                          CORCOEFFSOUTPUTFILE) {
  # Call the spartan function
  lhc_generatePRCoEffs(FILEPATH, PARAMETERS, MEASURES, LHCSUMMARYFILENAME,
                       CORCOEFFSOUTPUTFILE)
}


#' Internal function used to calculate the Partial Rank Correlation Coefficient
#' @keywords internal
pcor.test <- function(x, y, z, use = "mat", calc_method ="p", na.rm = TRUE) {
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  #
  # use: There are two methods to calculate the partial correlation coefficient
  #	 One is by using variance-covariance matrix ("mat") and the other is by
  # using recursive formula ("rec").
  #	 Default is "mat".
  #
  # method: There are three ways to calculate the correlation coefficient,
  #	which are Pearson's ("p"), Spearman's ("s"), and Kendall's ("k") methods.
  # 	    The last two methods which are Spearman's and Kendall's coefficient
  # are based on the non-parametric analysis.
  #	    Default is "p".
  #
  # na.rm: If na.rm is T, then all the missing samples are deleted from the
  # whole dataset, which is (x,y,z).
  #        If not, the missing samples will be removed just when the
  # correlation coefficient is calculated.
  #	   However, the number of samples for the p-value is the number of
  # samples after removing
  #	   all the missing samples from the whole dataset.
  #	   Default is "T".

  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z, check.names = FALSE)

  if (use == "mat") {
    p.use <- "Var-Cov matrix"
    pcor <- pcor.mat(x, y, z, cor_method = calc_method, na.rm = na.rm)
  } else if (use == "rec") {
    p.use <- "Recursive formula"
    pcor <- pcor.rec(x, y, z, cor_method = calc_method, na.rm = na.rm)
  } else {
    stop("\'use\' should be either \"rec\" or \"mat\"!\n")
  }

  # print the method
  if (gregexpr("p", calc_method)[[1]][1] == 1){
    p.method <- "Pearson"
  } else if (gregexpr("s", calc_method)[[1]][1] == 1){
    p.method <- "Spearman"
  }else if (gregexpr("k", calc_method)[[1]][1] == 1){
    p.method <- "Kendall"
  }else{
    stop("\'method\' should be \"pearson\" or \"spearman\" or \"kendall\"!\n")
  }

  # sample number
  n <- dim(na.omit(data.frame(x, y, z, check.names = FALSE)))[1]

  # given variables' number
  gn <- dim(z)[2]

  if (p.method == "Kendall"){
    statistic <- pcor / sqrt(2 * (2 * (n - gn) + 5) / (9 * (n - gn) *
                                                         (n - 1 - gn)))
    p.value <- 2 * pnorm(-abs(statistic))

  } else {
    val_to_sqrt_l <- n - 2 - gn
    val_to_sqrt_r <- 1 - pcor ^ 2
    statistic <- pcor * sqrt(val_to_sqrt_l / val_to_sqrt_r)
    p.value <- 2 * pnorm(-abs(statistic))
  }

  data.frame(estimate = pcor, p.value = p.value, statistic = statistic,
             n = n, gn = gn, Method = p.method, Use = p.use,
             check.names = FALSE)
}
