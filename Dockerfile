FROM silex/emacs:master-debian

RUN apt-get update && apt-get install -y curl git locales tzdata

# Configure locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Configure timezone
ENV TZ=America/Bogota
ENV TZDIR=/usr/share/zoneinfo
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN mkdir /tmp/org/


COPY lain /root/.emacs.d/lain
COPY emacs /root/.emacs

# Install Emacs packages during Docker build
RUN emacs --batch \
    --eval "(setq package-archives '((\"gnu\" . \"http://elpa.gnu.org/packages/\") (\"melpa\" . \"https://melpa.org/packages/\")))" \
    --eval "(package-initialize)" \
    --eval "(package-refresh-contents)" \
    --eval "(package-install 'elnode)" \
    --eval "(package-install 'evil)" \
    --eval "(package-install 'dash)" \
    --eval "(package-install 'json)" \
    --eval "(package-install 'htmlize)" \
    --eval "(package-install 'undo-tree)"

COPY entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

RUN git config --global user.email "lain@mail.com"
RUN git config --global user.name "LAIN"

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["emacs"]
