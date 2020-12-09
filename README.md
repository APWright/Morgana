# MORGANA : Flight Rules Studio

2020 Caltech/JPL/Artcenter Data Visualization Summer Program
Authors:

- Austin Wright
- Barbara Alonso
- Rachel Rose Waterhouse

Our [final presentation](https://www.youtube.com/watch?v=qzsKZQX6q0Y&t=2s) gives a good overview of the context of the problems this tool aims to solve as well as giving an introduction to the tool itself.

## Install

To use the tool clone the repository

```shell
 git clone https://github.com/APWright/Morgana.git
```

And open `index.html` in browser.

To compile from scratch install [elm](https://guide.elm-lang.org/install/elm.html)

And compile using

```shell
elm make src/Main.elm --output=lib/main.js
```

For reference on using elm see the [elm guide](https://guide.elm-lang.org/) as well as the [package repository](https://package.elm-lang.org/) for documentation of the packages used in this project.

## Documentation

For further reference information about the syntax and semantics of the flight rules logical spec language see:

- [docs/semantics.md](docs/semantics.md)
- [docs/syntax.md](docs/syntax.md)

And for documentation of future development and roadmap see [docs/roadmap.md](docs/roadmap.md).

## Examples

Example rule translations can be found in [rules/](rules/), and loaded into the tool using the _Load Tool_ functionality.
Example simulations can be found in [simulations/](simulations/), and loaded into the tool using the _Load Simulation_ functionality.
