#! /bin/bash
#
#  Git-rss
#  --------
#  use:
#     git-rss.sh
#     in a directory containing a git repository.

#  Default settings
TITLE="Git-RSS Recent Commit Feed"
LINK="http://www.google.com"
DESCRIPTION="An RSS feed of recent commits to a git repository."
LANGUAGE="en-us"
DATE=$(date)
WEBMASTER=""
OUTPUT="rss.xml"
DIR="."
NUMBER="10"

#  Look for default config file
if [ -f ~/.git-rss ]; then
    source ~/.git-rss
fi
#  Look for directory config file
if [ -f .git-rss ]; then
    source .git-rss
fi

#  If there are no command-line options, exit.
if [ $# -lt 1 ]; then
	echo "Usage: `basename $0` [options], for help give option -h" 1>&2
	exit 1
fi

#  Parse command-line arguments
while getopts hvt:l:d:o:r:n: Options; do
    case $Options in
        h) echo "`basename $0` [options]

This script should be run from inside a directory containing
a working git repository or else run with the -r option.

Options:
-v         show version
-h         show help
-t expr    specifies the title of the rss feed.
-l expr    specifies the link of the rss feed.
-d expr    specifies the description of the rss feed.
-o file    specifies output filename.
           default: rss.xml
-r dir     specifies directory other than current directory
           to run in.
           default: current directory
-n expr    specifies the number of items in the feed.
           default: 10"
            exit 0 ;;
        v) echo "git-rss Version 0.9"
            exit 0 ;;
        t) TITLE=$OPTARG;;
        l) LINK=$OPTARG;;
        d) DESCRIPTION=$OPTARG;;
        o) OUTPUT=$OPTARG;;
        r) DIR=$OPTARG;;
        n) NUMBER=$OPTARG;;
    esac
done

cd $DIR

#  Test if there's a git repository in current folder
git show > /dev/null 2>&1
if [ "$?" != "0" ]; then
    if [ $DIR = "." ]; then
        DIR="current directory"
    fi
    echo "$0: No git repository in working directory ($DIR)." 1>&2
    exit 1
fi

FORMAT="<item>%n<title>%s</title>%n<link>$LINK</link>%n<description>%n%b&lt;br/&gt;%nAuthor: %an%n</description>%n<pubDate>%ai</pubDate>%n<guid>$LINK</guid></item>"

LOG=$(git rev-list -n $NUMBER --pretty=format:"$FORMAT" HEAD |\
      sed 's/commit [a-z0-9]\{40\}//;')

echo "<?xml version=\"1.0\" encoding=\"utf8\"?>
<rss version=\"2.0\">
  <channel>
    <title>$TITLE</title>
    <link>$LINK</link>
    <description>$DESCRIPTION</description>
    <language>$LANGUAGE</language>
    <pubDate>Thu Dec 11 23:30:21 PST 2008</pubDate>
    <lastBuildDate>$DATE</lastBuildDate>
    <ttl>$NUMBER</ttl>

$LOG
    </channel>
</rss>" > $OUTPUT
