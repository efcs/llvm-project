
#===-------------------------------------------------------------------------------------------===//
# buildslave
#===-------------------------------------------------------------------------------------------===//
FROM ericwf/llvm-builder-base:latest AS llvm-github-actions-worker

COPY --from=ericwf/compiler:gcc-5 /opt/gcc-5 /opt/gcc-5
COPY --from=ericwf/compiler:gcc-tot /opt/gcc-tot /opt/gcc-tot
COPY --from=ericwf/compiler:llvm-4 /opt/llvm-4 /opt/llvm-4.0



RUN apt-get update && \
    apt-get install -y --no-install-recommends \
  && rm -rf /var/lib/apt/lists/*

ADD scripts/install_clang_packages.sh /tmp/
RUN /tmp/install_clang_packages.sh && rm /tmp/install_clang_packages.sh

RUN  useradd --system --shell $(which bash) --home-dir /home/llvm-github-worker llvm-github-worker --create-home --base-dir /home/llvm-github-worker

ADD scripts/github-actions/install-actions-runner.sh /tmp/
RUN /tmp/install-actions-runner.sh --install /actions-runner/ --version 2.165.2 && rm /tmp/install-actions-runner.sh

RUN chown -R llvm-github-worker:llvm-github-worker /actions-runner/
ADD scripts/github-actions/* /scripts

USER llvm-github-worker
WORKDIR /actions-runner/
