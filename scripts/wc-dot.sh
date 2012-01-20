#! /bin/bash

phrase=$(wc -l *.ml | sed 's|.ml||g' | sed 's|\([0-9]\+\) \([A-Za-z]\+\)|\2 - \1|');

capitals=$(for i in $phrase; do convert=`echo -n "${i:0:1}" | tr "[:lower:]" "[:upper:]"`; echo -n "${convert}${i:1} "; done | sed 's/^[ \t]*//;s/[ \t]*$//')

echo -n $capitals | sed 's|\([0-9]\+\) |\1\n|g' > wc.txt
