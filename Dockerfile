FROM silex/emacs:master-debian

RUN apt-get update && apt-get install -y curl git

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
    --eval "(package-install 'htmlize)"

COPY entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh

RUN git config --global user.email "lain@mail.com"
RUN git config --global user.name "LAIN"

ENTRYPOINT ["/opt/entrypoint.sh"]
CMD ["emacs"]
