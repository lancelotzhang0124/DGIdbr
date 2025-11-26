# DGIdbr

Helpers to query [DGIdb](https://dgidb.org/) via GraphQL for gene sets (case-control or subtype) and write aggregated drug–gene interaction tables.

## Features
- Group mode: up/down gene sets from case–control differential table.
- Subtype mode: auto-detect all subtypes and up/down sets from one table.
- Aggregates interaction scores, keeps per-gene lists, filters to approved drugs.
- Override DGIdb endpoint via `DGIDB_URL`.

## Installation
```r
# install.packages("remotes")
remotes::install_github("lancelotzhang0124/DGIdbr")


## Installation

From a local clone:

```r
# install.packages("devtools")
devtools::install_local("DGIdbr")  # or setwd to the cloned folder and use "."
```

From GitHub:

```r
# install.packages("remotes")
remotes::install_github("lancelotzhang0124/DGIdbr")
```

## Usage

```r
library(DGIdbr)

# Case-control (group) mode — input needs columns: gene, direction (up/down)
DGIdbr(
  mode = "group",
  base_tables = "path/to/input_dir",
  group_filename = "dep_group_sig.csv",
  base_out = "path/to/output_dir"
)

# Subtype mode — input needs columns: gene, direction (up/down), subtype
DGIdbr(
  mode = "subtype",
  base_tables = "path/to/input_dir",
  subtype_filename = "subtype_sig.csv",
  base_out = "path/to/output_dir"
)

```

Environment variable `DGIDB_URL` can override the default DGIdb GraphQL endpoint if needed.

Feel free to contact [zhanglingfeng@whu.edu.cn](zhanglingfeng@whu.edu.cn), if you have any question.

License: MIT License
