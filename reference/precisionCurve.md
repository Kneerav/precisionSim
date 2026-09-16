# Print a precision curve

Calculate quantiles of the simulated confidence-interval width
distribution for each estimand.

\`precisionCurve()\` estimates the probability that confidence intervals
for one or more user-defined estimands are narrower than specified
target widths and specified sample sizes.

## Usage

``` r
# S3 method for class 'precisionCurve'
print(x, digits = 2, ...)

# S3 method for class 'precisionCurve'
summary(object, digits = 2, ...)

# S3 method for class 'precisionCurve'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'precisionCurve'
quantile(x, prob = x$settings$prob, ...)

# S3 method for class 'precisionCurve'
plot(x, type = "curve", ...)

precisionCurve(
  fit,
  interval_fun,
  along,
  breaks,
  target_width,
  prob = 0.8,
  mc_conf_level = 0.95,
  nsim = 1000,
  seed = 123456,
  singular_action = "include",
  nonconverged_action = "exclude",
  tol = 1e-04
)
```

## Arguments

- x:

  An object of class \`"precisionCurve"\`.

- digits:

  Number of digits used when formatting numerical results.

- ...:

  Additional arguments. Currently unused.

- object:

  An object of class \`"precisionCurve"\`.

- row.names:

  Default is NULL.

- optional:

  Default is FALSE.

- prob:

  A single numeric value between 0 and 1 giving the target probability
  of achieving the requested interval width.

- type:

  The type of plot, where \`"assurance"\` displays the empirical
  cumulative distribution of confidence-interval widths, and
  \`"distribution"\` shows a histogram of the simulated
  confidence-interval widths. Default is \`"curve"\` which shows the
  assurance probability at each simulated nlevel (sample size).

- fit:

  A fitted linear mixed-effects model inheriting from \[lme4::lmerMod\]
  or extended from \[simr::extend\]

- interval_fun:

  A function that accepts a fitted model and returns a data frame
  containing the columns \`estimand\`, \`estimate\`, \`lower\`, and
  \`upper\`.

- along:

  A character value indicating the column name at which the breaks are
  specified.

- breaks:

  A positive numeric vector specifying the sample sizes at which to
  perform the simulations.

- target_width:

  A positive numeric vector giving the maximum acceptable total
  confidence-interval widths.

- mc_conf_level:

  A single numeric value between 0 and 1 giving the confidence level
  used for Monte Carlo confidence intervals.

- nsim:

  A positive whole number giving the number of simulations.

- seed:

  An optional integer seed for reproducible simulation.

- singular_action:

  A character value specifying how singular fits are handled. Available
  options are \`"include"\`, \`"exclude"\`, and \`"failure"\`.

- nonconverged_action:

  A character value specifying how non-converged fits are handled.
  Available options are \`"include"\`, \`"exclude"\`, and \`"failure"\`.

- tol:

  Tolerance for determining singular fit. Default is 1e-4.

## Value

The input object \`x\`, invisibly.

The input object \`x\`, invisibly.

Data frame with precisionSim results

A data frame containing the estimand, requested probability, and
corresponding confidence-interval width.

The input object \`x\`, invisibly.

An object of class \`"precisionSim"\`. The object contains:

\* \`results\`: simulation-level interval results; \* \`summary\`:
precision-assurance estimates; \* \`diagnostics\`: fitting and interval
diagnostics; \* \`settings\`: simulation settings; \* \`model\`: model
metadata; \* \`elapsed\`: elapsed computation time.

## Details

A quantile at probability 0.80 is the width that 80 percent of the
simulated confidence intervals do not exceed.

The function repeatedly simulates a response from a fitted linear
mixed-effects model, refits the model, calculates confidence intervals
using a user-supplied function, and compares the resulting interval
widths with the requested targets.

The fitted model should ordinarily be extended to the maximum requested
sample size before being supplied to this function. This can be done
using \[simr::extend()\].

For each value in \`breaks\`, the corresponding levels of \`along\` are
selected and the precision simulation is performed on that design.

## Interval function

\`interval_fun\` must return a data frame with one row per estimand and
the following columns:

\* \`estimand\`: a unique character label; \* \`estimate\`: the point
estimate; \* \`lower\`: the lower confidence limit; \* \`upper\`: the
upper confidence limit.

## Failed and excluded simulations

Simulations classified as \`"failure"\` are treated as not achieving the
target. Simulations classified as \`"exclude"\` are removed from the
analysable denominator. The returned object reports both the requested
and analysable denominators.

## See also

\[precisionSim()\], \[simr::extend()\], \[stats::quantile()\]

## Examples

``` r
if (FALSE) { # \dontrun{
library(lme4)

fit <- lmer(
  response ~ task + velocity + (1 | participant),
  data = example_data
)

fit_extended <- simr::extend(fit,  along="participant", n=50)

my_interval <- function(fitted_model) {
  # Calculate and return estimand, estimate, lower, and upper.
}

result <- precisionCurve(
  fit = fit,
  interval_fun = my_interval,
  along = "participant",
  breaks = c(10, 20, 50),
  target_width = c(10, 20, 50),
  nsim = 1000,
  seed = 123
)

print(result)
summary(result)
plot(result)
} # }
```
