#!/bin/bash


IMAGE_DIR="/home/sjunkin/Backgrounds"
PIC=$(ls $IMAGE_DIR/* | shuf -n1)
gsettings set org.gnome.desktop.background picture-uri "file://$PIC"
