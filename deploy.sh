#!/bin/bash

sudo sbcl --script ./src/main.lisp
sudo cp -r ./src/resources /mnt/linode/my/var/www/localhost/htdocs/
