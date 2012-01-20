#! /bin/bash

omake doc-dot
wc *.ml | sed 's|[0-9]+ +||'
