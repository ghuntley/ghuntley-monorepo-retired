FROM gitpod/workspace-full:latest

# Install Nix
USER root
RUN addgroup --system nixbld \
  && usermod -a -G nixbld gitpod \
  && mkdir -m 0755 /nix && chown gitpod /nix \
  && mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

CMD /bin/bash -l
USER gitpod
ENV USER gitpod
WORKDIR /home/gitpod

RUN touch .bash_profile && \
  curl https://nixos.org/nix/install | sh

RUN echo '. /home/gitpod/.nix-profile/etc/profile.d/nix.sh' >> /home/gitpod/.bashrc

# Give back control
USER root
