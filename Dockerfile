FROM gcc:8.2.0
RUN apt-get update -yq && \
    apt-get install \
        --yes \
        --no-install-recommends \
        cmake && \
    apt-get autoclean && \
    apt-get autoremove && \
    apt-get clean && \
    rm -rf /tmp/* /var/tmp/* && \
    rm -rf /var/lib/apt/lists