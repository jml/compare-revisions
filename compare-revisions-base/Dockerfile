FROM ubuntu:18.04
RUN apt-get -y update && apt-get -y install git
RUN mkdir -p /var/www/static/
COPY bootstrap/css/bootstrap.min.css /var/www/static/style.css
