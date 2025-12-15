# Dockerfile for the JSON HTTP server for the Kallisti project's interaction between IHMC's and CMU's code

FROM ubuntu:22.04
WORKDIR /usr/src/app

# get necessary tools for download stuff
RUN apt update && apt install -y
RUN apt install -y wget bzip2

# download and install SBCL
RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-2.5.3-x86-64-linux-binary.tar.bz2
RUN tar -xf sbcl-2.5.3-x86-64-linux-binary.tar.bz2
RUN rm sbcl-2.5.3-x86-64-linux-binary.tar.bz2
RUN cd sbcl-2.5.3-x86-64-linux && sh install.sh
RUN rm -r sbcl-2.5.3-x86-64-linux

# download and install QuickLisp
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --quit --load quicklisp.lisp --eval '(quicklisp-quickstart:install :path "/usr/src/app/quicklisp")'
RUN rm quicklisp.lisp

# copy the relevant code
COPY act-up-v1_3_3.lisp
COOY IBL.lisp
COPY http-server.lisp ./http-server.lisp
COPY docker-entrypoint.sh ./docker-entrypoint.sh
RUN chmod a+x docker-entrypoint.sh

# get the dependencies compiled
RUN sbcl --load quicklisp/setup.lisp --load act-up-v1_3_3 --load IBL --load http-server.lisp --eval '(sb-ext:exit)'

ENTRYPOINT [ "./docker-entrypoint.sh" ]
