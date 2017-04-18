FROM       alpine:3.5
RUN        apk add --update git
COPY       . /
EXPOSE     80
ENTRYPOINT [ "/compare-revisions" ]
