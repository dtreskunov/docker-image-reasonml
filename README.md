# ReasonML Docker Image for Development

After getting frustrated by [instructions](https://reasonml.github.io/docs/en/global-installation.html) on setting up [ReasonML](https://reasonml.github.io) on Windows, I decided to build a Docker image that would provide a sane working environment for coding in ReasonML.

---

## Image Contents

The image contains:
* [OCaml](https://ocaml.org) - strongly-typed functional language
* [ReasonML](https://reasonml.github.io/) - syntax sugar for OCaml making it JavaScript-like
* [OPAM](https://opam.ocaml.org) - package manager for OCaml
* [Emacs](https://www.gnu.org/software/emacs/) - ~~text editor~~ operating system
* ...and various tools:
    * [merlin](https://github.com/ocaml/merlin) - context sensitive completion for OCaml in Vim and Emacs
    * [ocp-indent](https://github.com/OCamlPro/ocp-indent) - indentation tool for OCaml, to be used from editors like Emacs and Vim
    * [tuareg](https://github.com/ocaml/tuareg) - Emacs OCaml mode
* ...as well as Emacs plugins:
    * [magit]() - Git ~~GUI~~ porcelain
    * [helm](https://github.com/emacs-helm/helm) - incremental completion and selection narrowing framework
    * [projectile](https://github.com/bbatsov/projectile) - Project Interaction Library for Emacs
    * [auto-complete](https://github.com/auto-complete/auto-complete) - 'nuff said
    * [company](https://github.com/company-mode/company-mode) - Modular in-buffer completion framework for Emacs
    * [utop](https://github.com/diml/utop) - universal toplevel (REPL aka shell) for OCaml/ReasonML
    * ...here's a nice [demo](https://tuhdo.github.io/helm-projectile.html) of what Helm and Projectile can do

## Usage

### Emacs in the terminal
```
docker run --rm -it dtreskunov/reasonml emacs
```

### Work on your project's source code
This will mount the source you have (on the Docker host) as the `/project` directory in the container. On Windows, `<path-to-project>` should be of the form `/c/users/...`.

```
docker run --rm -it -v /<path-to-project>:/src -w /project dtreskunov/reasonml emacs .
```

### Emacs GUI via X Server
1. Install an X Server. On Windows, you can use [Chotolatey](https://chocolatey.org/) and run `choco install -y vcxsrv`. Make sure that access controls are disabled. See [here](http://blog.ctaggart.com/2016/05/visual-studio-code-served-from-docker.html) for more details.
2. Find out the IP address of the Docker host, e.g. `192.168.1.100`.

```
docker run --rm -it --net=host dtreskunov/reasonml emacs -d 192.168.1.100:0
```

## Workflow

This image uses the "alternative" installation method mentioned in ReasonML docs (OPAM). It supports the following workflow for setting up a new project:

1. Mount an empty host directory inside the container (say, `/project`) (see below)
2. Duplicate the layout from `/template-project`:
    * `src/*` - put your source files (OCaml or ReasonML) here
    * `src/jbuild` - instructions for jbuilder (see [here](https://jbuilder.readthedocs.io/en/latest/jbuild.html))
    * `<project-name>.opam` - instructions for OPAM (see [here](https://opam.ocaml.org/doc/Packaging.html) and [here](https://opam.ocaml.org/doc/Manual.html#opam))
3. Run `opam pin add <project-name> /project --no-action` to tell OPAM that `<project-name>` is installed locally in this location
4. Run `opam depext --install <project-name>` to tell OPAM to install dependencies, compile, and install the project
5. Make changes to the source code
6. To recompile:
    * Run `opam upgrade <project-name>` - installs build artifacts into `~/.opam/4.05.0`
    * or run `jbuilder build @install` - puts artifacts into `_build/install/default`, which is used by the Merlin Emacs plugin

See [here](https://stackoverflow.com/a/28810997) for an explanation of this workflow.
