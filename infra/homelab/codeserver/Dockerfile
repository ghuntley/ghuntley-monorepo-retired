FROM codercom/code-server:latest

EXPOSE 8443

VOLUME [ "${PWD}:/home/coder/code" ]

USER root

RUN apt-get update

# Install direnv
RUN apt-get -y install direnv

# Install Nix
RUN addgroup --system nixbld \
  && usermod -a -G nixbld coder \
  && mkdir -m 0755 /nix && chown coder /nix \
  && mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

# Install Docker
RUN curl -fsSL https://get.docker.com -o get-docker.sh \
  && sudo sh get-docker.sh \
  && usermod -aG docker coder

RUN apt-get update \
  && apt-get install -y qemu-kvm libvirt-daemon-system libvirt-clients bridge-utils \
  && usermod -aG kvm coder \
  && usermod -aG kvm root \
  && usermod -aG libvirt coder \
  && usermod -aG libvirt root

# Install Nix
CMD /bin/bash -l
USER coder
ENV USER coder
WORKDIR /home/coder

RUN touch .bash_profile && \
  curl https://nixos.org/nix/install | sh

RUN mkdir -p /home/coder/.config/nixpkgs && echo '{ allowUnfree = true; }' >> /home/coder/.config/nixpkgs/config.nix

RUN echo '. /home/gitpod/.nix-profile/etc/profile.d/nix.sh' >> /home/coder/.bashrc
RUN echo 'eval "$(direnv hook bash)"' >> /home/coder/.bashrc
RUN echo 'eval `ssh-agent`' >> /home/coder/.bashrc

# Install visual studio code niceities
RUN sudo apt-get install silversearcher-ag

# Reset the font cache
#USER coder
#RUN fc-cache -f -v
