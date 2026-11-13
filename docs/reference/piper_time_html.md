# Save Animated Piper Diagram

Creates a Piper diagram animation through time and saves the animation
as an HTML file.

## Usage

``` r
piper_time_html(df, ...)
```

## Arguments

- df:

  An \`env_schema\` object.

- ...:

  Arguments passed on to
  [`piper_plot`](https://jentjr.github.io/manager/reference/piper_plot.md)

  `location`

  : Location identifier column.

  `analyte`

  : Analyte column.

  `result`

  : Analytical result column.

  `unit`

  : Reporting unit column.

  `sample_date`

  : Sample date column.

  `analyte_matching`

  : Analyte matching strategy used to resolve analyte names. One of
    \`"exact"\`, \`"strict"\`, or \`"normalized"\`.

  `x_cation`

  : Character vector identifying analytes representing calcium.

  `y_cation`

  : Character vector identifying analytes representing magnesium.

  `z_cation`

  : Character vector identifying analytes representing sodium and
    potassium.

  `x_anion`

  : Character vector identifying analytes representing chloride and
    fluoride.

  `y_anion`

  : Character vector identifying analytes representing alkalinity.

  `z_anion`

  : Character vector identifying analytes representing sulfate.

  `x_cation_label`

  : Label for the calcium axis.

  `y_cation_label`

  : Label for the magnesium axis.

  `z_cation_label`

  : Label for the sodium plus potassium axis.

  `x_anion_label`

  : Label for the chloride plus fluoride axis.

  `y_anion_label`

  : Label for the bicarbonate/carbonate axis.

  `z_anion_label`

  : Label for the sulfate axis.

  `x_z_anion_label`

  : Label displayed on the upper-left side of the Piper diamond.

  `x_y_cation_label`

  : Label displayed on the upper-right side of the Piper diamond.

  `scale_analyte`

  : Optional analyte used to scale point size.

  `group_col`

  : Optional column used to assign point shape.

  `transparency`

  : Point transparency.

  `pnt_size`

  : Point size when size scaling is not used.

  `label_size`

  : Axis label size.

  `title`

  : Optional plot title.

## Value

Invisibly returns the output HTML filename.
