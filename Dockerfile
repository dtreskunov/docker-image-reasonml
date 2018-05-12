#dtreskunov/reasonml:local
FROM ocaml/opam:alpine
RUN sudo apk update
RUN sudo apk add m4 diffutils emacs
WORKDIR /home/opam/opam-repository
RUN git pull --quiet
RUN opam update
RUN opam install --yes reason.3.0.4 merlin.3.0.5 ocp-indent user-setup tuareg

# Emacs goodies
WORKDIR /home/opam
COPY .emacs.d .emacs.d
RUN sudo chown -R opam:nogroup .emacs.d
RUN opam user-setup install
RUN emacs --script .emacs.d/packages.el
