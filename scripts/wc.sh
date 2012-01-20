#! /bin/bash

DIRECTORY=$1

ocamlwc -p $DIRECTORY*.ml | grep total |\
    sed 's/^[ \t]*//' | sed 's/total/are code./' |\
    sed 's/     / total lines, of which /' | sed 's/)/ of lines are documentation)/'
