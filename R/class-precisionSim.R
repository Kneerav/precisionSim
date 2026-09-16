#' Validate a precisionSim object
#'
#' @param x Object to validate.
#'
#' @return `x`, invisibly, or an error if validation fails.
#'
#' @noRd
validate_precision_sim <- function(x) {

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
    stop("Invalid precisionSim object. Missing components: ", paste(missing_components, collapse = ", "), call. = FALSE)
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

#' Construct a precisionSim object
#'
#' @param results Simulation-level interval results.
#' @param summary Assurance summary.
#' @param diagnostics Simulation diagnostics.
#' @param settings Simulation settings.
#' @param model Model metadata.
#' @param elapsed Elapsed computation time.
#'
#' @return An object of class `"precisionSim"`.
#'
#' @noRd
new_precision_sim <- function(
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
    class = "precisionSim"
  )

  validate_precision_sim(object)
}

#' Print a precision simulation
#'
#' @param x An object of class `"precisionSim"`.
#' @param digits Number of digits used when formatting numerical results.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionSim
#' @method print precisionSim
#' @export
print.precisionSim <- function(x, digits = 2, ...) {

  validate_precision_sim(x)

  settings <- x$settings
  diagnostics <- x$diagnostics

  cat("Precision analysis by simulation\n")
  cat("===============================\n\n")

  cat("Model: ")
  cat(paste(deparse(x$model$formula),collapse = ""), "\n")
  cat("Simulations requested:",settings$nsim, "\n")
  cat("Target probability:", format(100 * settings$prob,digits = digits),"%\n")
  cat("Monte Carlo confidence level:",format(100 * settings$mc_conf_level,digits = digits),"%\n\n")

  #create table
  output <- x$summary

  display_output <- data.frame(
    Estimand = output$estimand,
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

#' Summary of a precision simulation
#'
#' @param object An object of class `"precisionSim"`.
#' @param digits Number of digits used when formatting numerical results.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionSim
#' @method summary precisionSim
#' @export
summary.precisionSim <- function(object, digits = 2, ...) {

  validate_precision_sim(object)

  settings <- object$settings
  diagnostics <- object$diagnostics

  cat("Precision analysis by simulation\n")
  cat("===============================\n\n")

  cat("Model: ")
  cat(paste(deparse(object$model$formula),collapse = ""), "\n")
  cat("Simulations requested:",settings$nsim, "\n")
  cat("Target probability:", format(100 * settings$prob,digits = digits),"%\n")
  cat("Monte Carlo confidence level:",format(100 * settings$mc_conf_level,digits = digits),"%\n\n")

  #create table
  output <- object$summary

  display_output <- data.frame(
    Estimand = output$estimand,
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

#' Convert a precision simulation to data frame
#'
#' @param x An object of class `"precisionSim"`.
#' @param row.names Default is NULL.
#' @param optional Default is FALSE
#' @param ... Additional arguments. Currently unused.
#'
#' @return Data frame with precisionSim results
#'
#' @rdname precisionSim
#' @method as.data.frame precisionSim
#' @export
as.data.frame.precisionSim <- function(x, row.names = NULL, optional = FALSE, ...){

  #validate
  validate_precision_sim(x)

  #return df (change order of columns)
  as.data.frame(x$summary, row.names = row.names, optional = optional)
}

#' Confidence-interval width quantiles
#'
#' Calculate quantiles of the simulated confidence-interval width
#' distribution for each estimand.
#'
#' @param x An object of class `"precisionSim"`.
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
#' @rdname precisionSim
#' @method quantile precisionSim
#' @importFrom stats quantile
#' @export
quantile.precisionSim <- function(x, prob = x$settings$prob, ...){

  #validate
  validate_precision_sim(x)

  #fix
  if(any(!is.finite(prob)) || length(prob) == 0L || any(prob < 0 | prob > 1)){
    stop("prob must be a non-empty vector with finite values between 0 and 1")
  }

  #split
  width_split <- split(x$results$width, x$results$estimand)

  #get quantile
  do.call(rbind, lapply(width_split, stats::quantile, probs = prob, na.rm=T))

}


#' Plot a precision simulation
#'
#' @param x An object of class `"precisionSim"`.
#' @param type The type of plot, where `"assurance"` displays the
#' empirical cumulative distribution of confidence-interval widths,
#' and `"distribution"` shows a histogram of the simulated
#' confidence-interval widths.
#' @param ... Additional arguments. Currently unused.
#'
#' @return The input object `x`, invisibly.
#'
#' @rdname precisionSim
#' @method plot precisionSim
#' @importFrom rlang .data
#'
#' @export
plot.precisionSim <- function(x, type = "assurance", ...){

  #validate
  validate_precision_sim(x)

  #check input
  if(!type %in% c("assurance", "distribution")){
    stop("type must be one of: ",
         "'assurance' or 'distribution'")
  }

  #data
  data_plot <- x$results

  #plot
  if(type == "distribution"){

    g <-
      ggplot2::ggplot(data = data_plot, ggplot2::aes(.data$width))+
      ggplot2::geom_histogram(col = "white", fill = "lightblue")+
      ggplot2::facet_wrap(.~estimand)+
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
      ggplot2::facet_wrap(.~estimand)+
      ggplot2::theme_minimal()+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="lightblue"))+
      ggplot2::xlab("Confidence interval width")+
      ggplot2::ylab("Empericial cumulative distribution")

    plot(g)

  }


}
