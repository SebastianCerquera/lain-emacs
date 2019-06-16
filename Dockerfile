FROM ubuntu:18.04

RUN apt-get update && apt-get install -y language-pack-es
RUN echo "LANG=es_CO.UTF-8" > /etc/default/locale
RUN ln -sf /usr/share/zoneinfo/America/Bogota /etc/localtime

RUN apt-get update && apt-get install -y emacs25 curl git

RUN mkdir /tmp/org/

COPY elpa /root/.emacs.d/elpa
COPY htmlize /root/.emacs.d/htmlize
COPY evil /root/.emacs.d/evil
COPY lain /root/.emacs.d/lain
COPY emacs /root/.emacs

COPY entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

RUN git config --global user.email "sebastian2zenggmail.com"
RUN git config --global user.name "LAIN"

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["emacs"]