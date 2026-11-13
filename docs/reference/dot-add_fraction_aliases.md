# Generate fraction-specific analyte aliases

Creates common environmental laboratory reporting variants for analyte
names by appending fraction descriptors.

## Usage

``` r
.add_fraction_aliases(alias)
```

## Arguments

- alias:

  Character vector of analyte names or abbreviations.

## Value

A tibble containing:

- alias:

  Generated alias.

- fraction:

  Resolved fraction associated with the alias.

## Details

The following suffixes are generated:

\- \`"dissolved"\` - \`"diss"\` - \`"dis"\` - \`"total"\` - \`"tot"\` -
\`"filtered"\` - \`"filt"\` - \`"unfiltered"\` - \`"unfilt"\`

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
