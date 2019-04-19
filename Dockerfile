FROM ubuntu:18.04

RUN apt-get update && apt-get install -y language-pack-es
RUN echo "LANG=es_CO.UTF-8" > /etc/default/locale

RUN apt-get update && apt-get install -y emacs25 curl

RUN mkdir /tmp/org/

COPY elpa /root/.emacs.d/elpa
COPY htmlize /root/.emacs.d/htmlize
COPY lain /root/.emacs.d/lain
COPY emacs /root/.emacs

COPY entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["emacs"]