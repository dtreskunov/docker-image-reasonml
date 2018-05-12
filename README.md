# ReasonML Docker Image

After getting frustrated by [instructions](https://reasonml.github.io/docs/en/global-installation.html)
on setting up [ReasonML](https://reasonml.github.io) on Windows, I decided to build a
Docker image that would provide a sane working environment for coding in ReasonML.

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

```
docker run --rm -it -v C:/<path-to-project>:/src -w /src dtreskunov/reasonml:latest emacs .
```
