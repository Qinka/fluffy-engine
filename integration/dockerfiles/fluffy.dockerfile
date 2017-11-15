# fluffy-engine docker file
FROM debian:latest
MAINTAINER Johann Lee <me@qinka.pro>
RUN apt update && apt install -y libgmp10 libpq5
COPY bin/fluffy /bin
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000
