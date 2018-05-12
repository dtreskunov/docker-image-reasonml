#dtreskunov/reasonml:local
FROM ocaml/opam:alpine
RUN sudo apk update
RUN sudo apk add m4 emacs
WORKDIR /home/opam/opam-repository
RUN git pull
RUN opam update
RUN opam install --yes reason.3.0.4 merlin.3.0.5 ocp-indent user-setup tuareg

# Emacs goodies
WORKDIR /home/opam
COPY --chown=opam:nogroup .emacs.d .emacs.d
RUN opam user-setup install
RUN sudo apk add emacs
RUN emacs --script .emacs.d/packages.el
