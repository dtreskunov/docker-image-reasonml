#dtreskunov/reasonml:local
FROM ocaml/opam:alpine
RUN sudo apk update
RUN sudo apk add m4
WORKDIR /home/opam/opam-repository
RUN git pull --quiet
RUN opam update
RUN opam install --yes reason.3.0.4

# Emacs goodies
WORKDIR /home/opam
RUN sudo apk add diffutils font-noto the_silver_searcher emacs-gtk2
RUN opam install --yes merlin.3.0.5
RUN opam install --yes ocp-indent
RUN opam install --yes user-setup
RUN opam install --yes tuareg
COPY .emacs.d .emacs.d
RUN sudo chown -R opam:nogroup .emacs.d
RUN opam user-setup install
RUN emacs --script .emacs.d/init.el

# Template project
COPY template-project /template-project
RUN sudo chown -R opam:nogroup /template-project
RUN opam pin add template-project /template-project --yes --no-action
RUN opam depext --install --yes template-project
WORKDIR /template-project
