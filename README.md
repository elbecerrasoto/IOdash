<p align="left">
  <a href="https://semver.org/"><img src="https://img.shields.io/badge/version-0.0.0-blue" alt="Semantic Versioning"></a>
  <a href="https://snakemake.github.io/"><img src="https://img.shields.io/badge/license-MIT-red.svg?style=flat" alt="MIT License"></a>
</p>
<hr />

# Input-Output Leontief Models for Mexico

## v0.0.0

## Contents

- [Description](#description)
- [Installation](#installation)
- [Contributing](#contributing)

## Description

This repository contains a Shiny app designed to perform Input-Output Leontief models at the state level.

The target region is Mexico, and the data used is publicly available from the National Institute of Statistics and Geography (INEGI) ([link](https://en.www.inegi.org.mx/investigacion/coumip/)).

## Installation

_IOdash_ uses the R datascience ecosystem, with libraries like the _tidyverse_ and _shiny_.
Also it uses standard _Linux_ utilities like _make_.

As _IOdash_ is being developed you will need to check the used libraries on top of the included _R scripts_.
If everything is setup, try for a succesful run of:

``` sh
make check
```

## Contributing

Use the R `styler` package for code formatting.  
A `Makefile` is provided to simplify this process:

```sh
make style

```
