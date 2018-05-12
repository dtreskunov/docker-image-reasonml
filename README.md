# ReasonML Docker Image

After getting frustrated by [instructions](https://reasonml.github.io/docs/en/global-installation.html) on setting up [ReasonML](https://reasonml.github.io) on Windows, I decided to build a Docker image that would provide a sane working environment for coding in ReasonML.

This image uses the "alternative" installation method mentioned in ReasonML docs (OPAM).

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
    * [helm](https://github.com/emacs-helm/helm) - incremental completion and selection narrowing framework
    * [projectile](https://github.com/bbatsov/projectile) - Project Interaction Library for Emacs
    * [auto-complete](https://github.com/auto-complete/auto-complete) - 'nuff said
    * [company](https://github.com/company-mode/company-mode) - Modular in-buffer completion framework for Emacs
    * [utop](https://github.com/diml/utop) - universal toplevel (REPL aka shell) for OCaml/ReasonML

## Usage

### Emacs in the terminal
```
docker run --rm -it dtreskunov/reasonml emacs
```

### Work on your project's source code
This will mount the source you have (on the Docker host) as the `/src` directory in the container. Unfortunately, this doesn't seem to work when using [docker-machine](https://docs.docker.com/machine/) on Windows.

```
docker run --rm -it -v /<path-to-project>:/src -w /src dtreskunov/reasonml emacs .
```

### Emacs GUI via X Server
1. Install an X Server. On Windows, you can use [Chotolatey](https://chocolatey.org/) and run `choco install -y vcxsrv`. Make sure that access controls are disabled. See [here](http://blog.ctaggart.com/2016/05/visual-studio-code-served-from-docker.html) for more details.
2. Find out the IP address of the Docker host, e.g. `192.168.1.100`.

```
docker run --rm -it --net=host dtreskunov/reasonml emacs -d 192.168.1.100:0
```
