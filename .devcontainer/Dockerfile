FROM mcr.microsoft.com/devcontainers/universal

# RUN df -h

# RUN rm -rf /usr/share/dotnet /usr/local/lib/android /usr/local/php /usr/local/py-utils \
#         /usr/local/python /usr/local/rvm /usr/local/sdkman /usr/local/share/nvm \
#         /opt/ghc /opt/hostedtoolcache/CodeQL /opt/conda /opt/dotnet \
#         /home/codespace/.local/lib/python3.12

# RUN df -h

USER root

RUN cd /root && \
        curl -L -o autoconf.tar.gz https://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz && \
        tar fxz autoconf.tar.gz && \
        cd autoconf-2.71 && \
        ./configure --prefix=/usr && \
        make && \
        make install && \
        cd .. && \
        rm -rf autoconf-2.71 autoconf.tar.gz

ARG USERNAME=codespace
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN curl -L -o /usr/bin/opam https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-i686-linux && \
        chmod +x /usr/bin/opam && \
        apt-get update && \
        apt-get remove -y swig3.0 unixodbc-dev libpq-dev \
        default-libmysqlclient-dev libgdiplus jq python3-pip sqlite3 \
        libsqlite3-dev tk-dev uuid-dev clang lldb llvm gdb vim-doc xtail \
        libsecret-1-dev && \
        apt-get clean && \
        rm -rf /var/lib/apt/lists/*

USER ${USERNAME}

# We run out of disk space, so we create the switch after the VM is ready.
# RUN opam init -a --disable-sandboxing --yes --bare && \
#         opam update -a && \
#         opam switch create 4.14.0 --yes  && \
#         eval $(opam env --switch 4.14.0) && \
#         opam install --yes  ocamlformat merlin ocaml-lsp-server bonsai

USER root
