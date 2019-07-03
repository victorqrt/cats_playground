#!/bin/sh

MYSELF=`which "$0" 2>/dev/null`
[ $? -gt 0 -a -f "$0" ] && MYSELF="./$0"

java=javaw
if test -n "$JAVA_HOME"; then
java="$JAVA_HOME/bin/javaw"
fi

exec "$java" $java_args -jar $MYSELF "$@"

exit 1
