# Re-evaluate precision assurance

\`assurance_test()\` re-evaluates stored confidence-interval widths
against new assurance settings without rerunning the original model
simulations.

## Usage

``` r
assurance_test(
  x,
  prob = x$settings$prob,
  target_width = x$settings$target_width,
  mc_conf_level = x$settings$mc_conf_level,
  singular_action = x$settings$singular_action,
  nonconverged_action = x$settings$nonconverged_action,
  ...
)

# Default S3 method
assurance_test(
  x,
  prob = NULL,
  target_width = NULL,
  mc_conf_level = NULL,
  singular_action = NULL,
  nonconverged_action = NULL,
  ...
)

# S3 method for class 'precisionSim'
assurance_test(
  x,
  prob = x$settings$prob,
  target_width = x$settings$target_width,
  mc_conf_level = x$settings$mc_conf_level,
  singular_action = x$settings$singular_action,
  nonconverged_action = x$settings$nonconverged_action,
  ...
)

# S3 method for class 'precisionCurve'
assurance_test(
  x,
  prob = x$settings$prob,
  target_width = x$settings$target_width,
  mc_conf_level = x$settings$mc_conf_level,
  singular_action = x$settings$singular_action,
  nonconverged_action = x$settings$nonconverged_action,
  ...
)
```

## Arguments

- x:

  An object of class \`"precisionSim"\` or \`"precisionCurve"\`.

- prob:

  A single numeric value between 0 and 1 specifying the target assurance
  probability. By default, the value stored in \`x\` is used.

- target_width:

  A positive numeric vector containing the confidence-interval width
  targets. By default, the targets stored in \`x\` are used.

- mc_conf_level:

  A single numeric value between 0 and 1 specifying the confidence level
  for the Monte Carlo confidence intervals. By default, the value stored
  in \`x\` is used.

- singular_action:

  A character value specifying how singular fits are handled. Available
  options are \`"include"\`, \`"exclude"\`, and \`"failure"\`. By
  default, the value stored in \`x\` is used.

- nonconverged_action:

  A character value specifying how non-converged fits are handled.
  Available options are \`"include"\`, \`"exclude"\`, and \`"failure"\`.
  By default, the value stored in \`x\` is used.

- ...:

  Additional arguments passed to class-specific methods.

## Value

An object of class \`"precisionSim"\` or \`"precisionCurve"\`. The
object contains:

\* \`results\`: simulation-level interval results; \* \`summary\`:
precision-assurance estimates; \* \`diagnostics\`: fitting and interval
diagnostics; \* \`settings\`: simulation settings; \* \`model\`: model
metadata; \* \`elapsed\`: elapsed computation time.

## Details

This is an S3 generic with methods for objects of class
\`"precisionSim"\` and \`"precisionCurve"\`.

\`assurance_test()\` uses the simulation-level intervals already stored
in \`x\`. It does not simulate new responses or refit the original
model.

For each estimand and width target, assurance is calculated as the
proportion of simulations whose confidence-interval width is less than
the target width, after applying the requested rules for singular and
non-converged fits.

## See also

\[precisionSim()\], \[precisionCurve()\]

## Examples

``` r
if (FALSE) { # \dontrun{
sim_result <- precisionSim(...)

assurance_test(
  sim_result,
  target_width = c(10, 20, 50),
  prob = 0.80
)

curve_result <- precisionCurve(...)

assurance_test(
  curve_result,
  target_width = c(10, 20, 50)
)
} # }
```
