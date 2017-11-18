# fluffy-engine docker file
FROM debian:latest
MAINTAINER Johann Lee <me@qinka.pro>
RUN apt update && apt install -y libgmp10 libpq5
ADD root /
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000
