FROM        r-base:latest
MAINTAINER  Nick Waters nickp60@gmail.com
RUN apt-get update
RUN apt-get install -y git libgit2-dev build-essential libssl-dev libcurl4-openssl-dev libxml2-dev curl
RUN cachebuster=c9s53b3hf git clone https://github.com/nickp60/meltsy.git
RUN Rscript meltsy/install_deps.R
EXPOSE 80