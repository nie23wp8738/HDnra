#' @title
#' Print Method for S3 Class "NRtest"
#'
#' @description
#' Prints the details of the \pkg{NRtest} object in a user-friendly manner. This method
#' provides a clear and concise presentation of the test results contained within
#' the \pkg{NRtest} object, including all relevant statistical metrics and test details.
#'
#' @usage
#' ## S3 method for class \pkg{NRtest}
#' \method{print}{NRtest}(x, ...)
#'
#' @param x an \pkg{NRtest} object.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' The `print.NRtest` function formats and presents the contents of the \pkg{NRtest}
#' object, which includes statistical test results and related parameters. This
#' function is designed to provide a user-friendly display of the object's
#' contents, making it easier to understand the results of the analysis.
#'
#' @section Value:
#' Invisibly returns the input `x`.
#'
#' @seealso \code{\link{NRtest.object}}
#'
#' @author
#' Pengfei Wang \email{nie23.wp8738@e.ntu.edu.sg}

#' @concept object
#' @export

print.NRtest <- function(x, ...) {
  coll.string <- paste("\n", strrep(" ", 33), sep = "")
  cat("\nResults of Hypothesis Test\n")
  cat("--------------------------\n\n")

  cat("Test name:", strrep(" ", 23), x$method, "\n\n", sep = "")

  alt.string <- x$alternative
  if (!is.null(nv <- x$null.value)) {
    nnv <- names(nv)
    if (length(nv) == 1) {
      if (is.null(nnv) || nnv == "") {
        cat("Null Hypothesis:", strrep(" ", 17), nv, "\n\n", sep = "")
      } else {
        cat("Null Hypothesis:", strrep(" ", 17), paste(nnv, "is", nv), "\n\n", sep = "")
      }
      if (!is.na(match(alt.string, c("two.sided", "less", "greater")))) {
        alt.string <- switch(alt.string, two.sided = "not", less = "less than", greater = "greater than")
        alt.string <- paste(nnv, "is", alt.string, nv)
      }
    } else {
      cat("Null Hypothesis:", strrep(" ", 17), paste("All", length(nv), "values of", nnv[1], "=", format(nv[1], ...)), "\n\n", sep = "")
    }
  } else {
    cat("Null Hypothesis:", strrep(" ", 17), "NULL", "\n\n", sep = "")
  }

  cat("Alternative Hypothesis:", strrep(" ", 10), alt.string, "\n\n", sep = "")



  if (is.null(names(x$data.name)))
    cat("Data:", strrep(" ", 28), x$data.name, "\n\n", sep = "")
  else cat("Data:", strrep(" ", 28), paste(paste(format(names(x$data.name),
                                                        justify = "left"), format(x$data.name, ...), sep = " = "),
                                           collapse = coll.string), "\n\n", sep = "")
  # if (!is.null(x$grouping.variable))
  #   cat("Grouping Variable:", strrep(" ", 11), x$grouping.variable,
  #       "\n\n", sep = "")
  # if (!is.null(x$subset.expression))
  #   cat("Subset With:", strrep(" ", 17), x$subset.expression, "\n\n",
  #       sep = "")

  if (!is.null(x$parent.of.data))
    cat("Data Source:", strrep(" ", 21), x$parent.of.data, "\n\n",
        sep = "")
  if (!is.null(x$sample.size)) {
    if (length(x$sample.size) > 1) {
      cat("Sample Sizes:", strrep(" ", 20), paste(paste(format(names(x$sample.size),
                                                               justify = "left"), format(x$sample.size, nsmall = 0,
                                                                                         ...), sep = " = "), collapse = coll.string),
          "\n\n", sep = "")
    }
    else {
      cat("Sample Size:", strrep(" ", 21), x$sample.size, "\n\n",
          sep = "")
    }
  }
  if (!is.null(x$sample.dimension)) {
    if (length(x$sample.dimension) > 1) {
      # Handles the case where sample.dimension is a vector of multiple values
      cat("Sample Dimensions:", strrep(" ", 15), paste(paste(format(names(x$sample.dimension),
                                                                    justify = "left"), format(x$sample.dimension, nsmall = 0,
                                                                                              ...), sep = " = "), collapse = ", "),
          "\n\n", sep = "")
    }
    else {
      # Handles the case where sample.dimension is a single value
      cat("Sample Dimension:", strrep(" ", 16), x$sample.dimension, "\n\n",
          sep = "")
    }
  }
  if (!is.null(x$statistic)) {
    string <- ifelse(length(x$statistic) == 1, paste("Test Statistic:",
                                                     strrep(" ", 18), sep = ""),
                     paste("Test Statistics:", strrep(" ", 17),  sep = ""))
    cat(string, paste(paste(format(names(x$statistic), justify = "left"),
                            format(x$statistic, nsmall = 0, ...), sep = " = "),
                      collapse = coll.string), "\n\n", sep = "")
  }


    if (!is.null(x$estimation.method)){
      line1 <- "Approximation method to the"
      line2 <- "null distribution of "

      cat(format(line1, justify = "left"),  strrep(" ", 6), x$estimation.method,"\n",
          format(line2,justify = "left"),
          format(names(x$statistic), justify = "left"),": ",

          "\n\n", sep = "")
    }

  # if (!is.null(x$estimate)) {
  #   cat("Approximation parameter(s):", strrep(" ", 6), paste(paste(format(names(x$estimate),
  #                                                                         justify = "left"), format(x$estimate, nsmall = 0,
  #                                                                                                   ...), sep = " = "), collapse = coll.string), "\n\n",
  #       sep = "")
  # }
  #


  # if (!is.null(x$bad.obs) && any(x$bad.obs > 0)) {
  #   if (length(x$bad.obs) > 1)
  #     cat("Number NA/NaN/Inf's:", strrep(" ", 9), paste(paste(format(names(x$bad.obs),
  #                                                                    justify = "left"), format(x$bad.obs, nsmall = 0,
  #                                                                                              ...), sep = " = "), collapse = coll.string),
  #         "\n\n", sep = "")
  #   else cat("Number NA/NaN/Inf's:", strrep(" ", 9), x$bad.obs,
  #            "\n\n", sep = "")
  # }

  if (!is.null(x$parameter)) {
    string <- ifelse(length(x$parameter) > 1, paste("Approximation parameter(s):",
                                                    strrep(" ", 6), sep = ""), paste("Approximation parameter(s):",
                                                                                     strrep(" ", 6), sep = ""))
    cat(string, paste(paste(format(names(x$parameter), justify = "left"),
                            format(x$parameter, nsmall = 0, ...), sep = " = "),
                      collapse = coll.string), "\n\n", sep = "")
  }

  if (length(x$p.value) == 1)
    cat("P-value:", strrep(" ", 25), format(x$p.value, ...), "\n\n",
        sep = "")
  else {
    if (!is.null(names(x$p.value)))
      cat("P-value:", strrep(" ", 25), paste(paste(format(names(x$p.value),
                                                           justify = "left"), format(x$p.value, ...), sep = " = "),
                                              collapse = coll.string), "\n\n", sep = "")
    else cat("P-value:", strrep(" ", 25), paste(format(x$p.value,
                                                        ...), collapse = coll.string), "\n\n", sep = "")
  }
  # if (!is.null(x$conf.level)) {
  #   cat("Confidence Level:", strrep(" ", 12),format(x$conf.level), "\n\n", sep = "")
  # }
  #
  # if (!is.null(x$conf.int)) {
  #   ci.pct <- format(100 * attr(x$conf.int,"conf.level"))
  #   cat(ci.pct, "Confidence Interval:", strrep(" ", 9),strrep(" ", 29 - nchar(ci.pct) -
  #                                                  18), paste(paste(c("LCL", "UCL"), format(x$conf.int,
  #                                                                                           nsmall = 0, ...), sep = " = "), collapse = coll.string),
  #       "\n\n", sep = "")
  # }
  # if (!is.null(x$interval)) {
  #   print.intervalEstimate(x$interval)
  # }
  invisible(x)
}
