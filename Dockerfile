FROM node:latest
RUN apt-get update

# let the `node` user use `sudo`
RUN apt-get install --yes sudo
RUN groupadd wheel && adduser node wheel
ADD sudoers.d /etc/sudoers.d

# install build tools
WORKDIR /home/node
USER node
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
RUN npm install -g esy

# add template project
ADD .profile .profile
RUN sudo chown node:node .profile
RUN git clone https://github.com/esy-ocaml/esy-reason-project templates/esy-reason-project

# build template projects
RUN sudo apt-get install --yes rsync
RUN bash --login -c 'cd templates/esy-reason-project && esy install'

# Emacs
RUN sudo apt-get install --yes fonts-noto silversearcher-ag emacs24
COPY --chown=node:node .emacs.d .emacs.d
RUN emacs --script .emacs.d/init.el
RUN touch templates/esy-reason-project/.projectile

ENTRYPOINT [ "bash", "--login", "-c" ]
CMD ["bash"]
