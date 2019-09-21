#!/bin/bash

sudo sbcl --script ./src/main.lisp && \
sudo wkhtmltopdf ./src/my-cv.html ./src/resources/cv.david-rueda.pdf && \
sudo cp -r ./src/resources /mnt/linode/my/var/www/localhost/htdocs/
