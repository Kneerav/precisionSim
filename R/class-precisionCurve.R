#' Validate a precisionCurve object
#'
#' @param x Object to validate.
#'
#' @return `x`, invisibly, or an error if validation fails.
#'
#' @noRd
validate_precision_curve <- function(x) {

  required_components <- c(
    "call",
    "results",
    "summary",
    "diagnostics",
    "settings",
    "model",
    "elapsed"
  )

  missing_components <- setdiff(required_components,names(x))

  if (length(missing_components) > 0) {
    stop("Invalid precisionCurve object. Missing components: ", paste(missing_components, collapse = ", "), call. = FALSE)
  }

  if (!is.data.frame(x$results)) {
    stop( "The results component must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(x$summary)) {
    stop("The summary component must be a data frame.", call. = FALSE
    )
  }

  if (!is.data.frame(x$diagnostics)) {
    stop("The diagnostics component must be a data frame.", call. = FALSE
    )
  }

  invisible(x)
}

#' Construct a precisionCurve object
#'
#' @param results Simulation-level interval results.
#' @param summary Assurance summary.
#' @param diagnostics Simulation diagnostics.
#' @param settings Simulation settings.
#' @param model Model metadata.
#' @param elapsed Elapsed computation time.
#'
#' @return An object of class `"precisionCurve"`.
#'
#' @noRd
new_precision_curve <- function(
    call,
    results,
    summary,
    diagnostics,
    settings,
    model,
    elapsed
) {

  object <- structure(
    list(
      call = call,
      results = results,
      summary = summary,
      diagnostics = diagnostics,
      settings = settings,
      model = model,
      elapsed = elapsed
    ),
    class = "precisionCurve"
  )

  validate_precision_curve(object)
}

#' Print a precision curve
#'
#' @param x An object of class `"precisionCurve"`.
#' @param digits Number of digits used when formatting numerical results.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionCurve
#' @method print precisionCurve
#' @export
print.precisionCurve <- function(x, digits = 2, ...) {

  validate_precision_curve(x)

  settings <- x$settings
  diagnostics <- x$diagnostics

  cat("Precision analysis by simulation\n")
  cat("===============================\n\n")

  cat("Model: ")
  cat(paste(deparse(x$model$formula),collapse = ""), "\n")
  cat("Simulations requested:",settings$nsim, "\n")
  cat("Target probability:", format(100 * settings$prob,digits = digits),"%\n")
  cat("Monte Carlo confidence level:",format(100 * settings$mc_conf_level,digits = digits),"%\n\n")
  cat("nlevels along variable: ",settings$along, "\n")

  #create table
  output <- x$summary

  display_output <- data.frame(
    Estimand = output$estimand,
    nlevels = output$nlevels,
    nrow = output$nrow,
    Width = output$width_target,
    Assurance = paste0(round(100*output$probability_valid, digits = digits), "%"),
    `Monte Carlo CI` = paste0(round(100*output$mc_valid_lower, digits = digits),
                              "%, ",
                              round(100*output$mc_valid_upper, digits = digits), "%"),
    check.names = F
  )

  print(display_output, row.names = FALSE, digits = digits, right = FALSE)

  cat("\nDiagnostics:\n")
  cat("  Failed fits:",sum(diagnostics$fit_error, na.rm = TRUE),"\n")
  cat("  Non-converged fits:",sum(diagnostics$converged == FALSE, na.rm = TRUE),"\n")
  cat("  Singular fits:", sum(diagnostics$singular, na.rm = TRUE),"\n")
  cat( "  Interval failures:",sum(diagnostics$interval_error, na.rm = TRUE),"\n")

  if(!is.null(x$elapsed)) {cat("\nElapsed time:",format(x$elapsed[3]), "\n")}

  invisible(x)
}

#' Summary a precision curve
#'
#' @param object An object of class `"precisionCurve"`.
#' @param digits Number of digits used when formatting numerical results.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionCurve
#' @method summary precisionCurve
#' @export
summary.precisionCurve <- function(object, digits = 2, ...) {

  validate_precision_curve(object)

  settings <- object$settings
  diagnostics <- object$diagnostics

  cat("Precision analysis by simulation\n")
  cat("===============================\n\n")

  cat("Model: ")
  cat(paste(deparse(object$model$formula),collapse = ""), "\n")
  cat("Simulations requested:",settings$nsim, "\n")
  cat("Target probability:", format(100 * settings$prob,digits = digits),"%\n")
  cat("Monte Carlo confidence level:",format(100 * settings$mc_conf_level,digits = digits),"%\n\n")
  cat("nlevels along variable: ",settings$along, "\n")

  #create table
  output <- object$summary

  display_output <- data.frame(
    Estimand = output$estimand,
    nlevels = output$nlevels,
    nrow = output$nrow,
    Width = output$width_target,
    Assurance = paste0(round(100*output$probability_valid, digits = digits), "%"),
    `Monte Carlo CI` = paste0(round(100*output$mc_valid_lower, digits = digits),
                              "%, ",
                              round(100*output$mc_valid_upper, digits = digits), "%"),
    check.names = F
  )

  print(display_output, row.names = FALSE, digits = digits, right = FALSE)

  cat("\nDiagnostics:\n")
  cat("  Failed fits:",sum(diagnostics$fit_error, na.rm = TRUE),"\n")
  cat("  Non-converged fits:",sum(diagnostics$converged == FALSE, na.rm = TRUE),"\n")
  cat("  Singular fits:", sum(diagnostics$singular, na.rm = TRUE),"\n")
  cat( "  Interval failures:",sum(diagnostics$interval_error, na.rm = TRUE),"\n")

  if(!is.null(object$elapsed)) {cat("\nElapsed time:",format(object$elapsed[3]), "\n")}

  invisible(object)
}

#' Convert a precision curve to data frame
#'
#' @param x An object of class `"precisionCurve"`.
#' @param row.names Default is NULL.
#' @param optional Default is FALSE.
#' @param ... Additional arguments. Currently unused.
#'
#' @return Data frame with precisionSim results
#'
#' @rdname precisionCurve
#' @method as.data.frame precisionCurve
#' @export
as.data.frame.precisionCurve <- function(x, row.names = NULL, optional = FALSE, ...){

  #validate
  validate_precision_curve(x)

  #return df (change order of columns)
  as.data.frame(x$summary, row.names = row.names, optional = optional)
}

#' Confidence-interval width quantiles
#'
#' Calculate quantiles of the simulated confidence-interval width
#' distribution for each estimand.
#'
#' @param x An object of class `"precisionCurve"`.
#' @param prob Numeric vector of probabilities between 0 and 1.
#' @param ... Additional arguments. Currently unused.
#'
#' @return A data frame containing the estimand, requested probability,
#'   and corresponding confidence-interval width.
#'
#' @details
#' A quantile at probability 0.80 is the width that 80 percent of the
#' simulated confidence intervals do not exceed.
#'
#' @rdname precisionCurve
#' @method quantile precisionCurve
#' @importFrom stats quantile
#' @export
quantile.precisionCurve <- function(x, prob = x$settings$prob, ...){

  #validate
  validate_precision_curve(x)

  #fix
  if(any(!is.finite(prob)) || length(prob) == 0L || any(prob < 0 | prob > 1)){
    stop("prob must be a non-empty vector with finite values between 0 and 1")
  }

  #split
  group_indices <- split(seq_len(nrow(x$results)), list(x$results$estimand, x$results$nlevels), drop=T)

  #get quantile
  quantile_results <- lapply(
    group_indices,
    function(index) {

      current_data <- x$results[index,,drop = FALSE]

      quantiles <- stats::quantile(
        current_data$width,
        probs = prob,
        na.rm = TRUE,
        names = FALSE
      )

      data.frame(
        estimand = current_data$estimand[1],
        nlevels = current_data$nlevels[1],
        prob = prob,
        width = quantiles,
        row.names = NULL
      )
    }
  )

  #bind
  quantile_results <- do.call(rbind,quantile_results)

  #fix
  rownames(quantile_results) <- NULL

  #print
  quantile_results

}

#' Plot a precision curve
#'
#' @param x An object of class `"precisionCurve"`.
#' @param type The type of plot, where `"assurance"` displays the
#' empirical cumulative distribution of confidence-interval widths,
#' and `"distribution"` shows a histogram of the simulated
#' confidence-interval widths. Default is `"curve"` which shows the
#' assurance probability at each simulated nlevel (sample size).
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionCurve
#' @method plot precisionCurve
#' @importFrom rlang .data
#' @export
plot.precisionCurve <- function(x, type = "curve", ...){

  #validate
  validate_precision_curve(x)

  #check input
  if(!type %in% c("curve", "assurance", "distribution")){
    stop("type must be one of: ",
         "'curve', 'assurance' or 'distribution'")
  }

  #data
  data_plot <- x$results
  data_plot_curve <- x$summary

  #plot
  if(type == "curve"){

    g <-
      ggplot2::ggplot(data = data_plot_curve)+
      ggplot2::geom_line(ggplot2::aes(.data$nlevels, .data$probability_valid), col="lightblue")+
      ggplot2::geom_errorbar(ggplot2::aes(.data$nlevels,
                                          ymin = .data$mc_valid_lower,
                                          ymax = .data$mc_valid_upper), col="lightblue", width=0)+
      ggplot2::geom_point(ggplot2::aes(.data$nlevels, .data$probability_valid), pch=21, col="white", fill="lightblue", size=2)+
      ggplot2::facet_grid(.data$width_target~.data$estimand)+
      ggplot2::theme_minimal()+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightblue"))+
      ggplot2::xlab("Sample size")+
      ggplot2::ylab("Assurance probability")


    plot(g)

  }

  #plot
  if(type == "distribution"){

    g <-
      ggplot2::ggplot(data = data_plot, ggplot2::aes(.data$width))+
      ggplot2::geom_histogram(col = "white", fill = "lightblue")+
      ggplot2::facet_grid(.data$nlevels~.data$estimand)+
      ggplot2::theme_minimal()+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightblue"))+
      ggplot2::xlab("Confidence interval width")+
      ggplot2::ylab("Count")

    plot(g)

  }

  #plot
  if(type == "assurance"){

    g <-
      ggplot2::ggplot(data = data_plot, ggplot2::aes(.data$width))+
      ggplot2::stat_ecdf(geom = "step", col="lightblue", lwd=1)+
      ggplot2::facet_grid(.data$nlevels~.data$estimand)+
      ggplot2::theme_minimal()+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightblue"))+
      ggplot2::xlab("Confidence interval width")+
      ggplot2::ylab("Empericial cumulative distribution")

    plot(g)

  }

}
