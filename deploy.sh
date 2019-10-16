#!/bin/bash

wkhtmltopdf ./src/my-cv.html ./src/resources/cv.david-rueda.pdf && \
    cp -r ./src/resources /home/davd/linode/var/www/localhost/htdocs/
