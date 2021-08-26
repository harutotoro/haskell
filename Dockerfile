FROM chenmoucheng/clash:1.2.5
COPY --chown=gitpod:gitpod . /home/gitpod/
WORKDIR /home/gitpod
ENV SHELL /bin/bash
