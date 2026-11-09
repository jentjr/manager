# Generate fraction-specific analyte aliases

Creates common environmental laboratory reporting variants for analyte
names by appending fraction descriptors.

## Usage

``` r
.add_fraction_aliases(
  alias,
  fractions = c("dissolved", "total", "filtered", "unfiltered")
)
```

## Arguments

- x:

  Character vector of analyte names or abbreviations.

## Value

A character vector containing the original values and common
fraction-specific variants.

## Details

The following suffixes are generated:

\- \`"dissolved"\` - \`"total"\` - \`"filtered"\` - \`"unfiltered"\`

This helper is primarily used when constructing \[analyte_tbl\] and
\[analyte_alias_lookup\].

## Examples

``` r
.add_fraction_aliases(c("ca", "calcium"))
#> Error in .add_fraction_aliases(c("ca", "calcium")): could not find function ".add_fraction_aliases"

# Returns aliases such as:
# "ca"
# "ca dissolved"
# "ca total"
# "calcium dissolved"
```
