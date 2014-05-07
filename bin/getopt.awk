#!/bin/env awk

BEGIN { SHORT_OPTS=""; LONG_OPTS=""; }
{
    for (i=1; i<=NF; i++) {
        split($i, b, ":");
        opts=b[1]
        # print "DEBUG: length(b) == " length(b);
        if (length(b) == 2)
            extra_tok=":"
        else
            extra_tok=""
        # print "DEBUG: opts=" opts;
        split(opts, a, "|");
        if (length(a) == 2) {
            # print "Both forms:\t" a[1] " then " a[2] extra_tok;
            SHORT = a[1];
            LONG  = a[2];
        } else {
            # print "Single forms:\t" a[1] extra_tok;
            if (length(a[1]) == "1") {
                SHORT = a[1]; LONG = "";
            } else {
                LONG = a[1]; SHORT = ""; }
        }
        if (length(SHORT) > 0) {
            if (length(SHORT_OPTS) > 0)
                SHORT_OPTS = SHORT_OPTS SHORT extra_tok;
            else
                SHORT_OPTS = "-o " SHORT extra_tok;
        }
        if (length(LONG) > 0) {
            if (length(LONG_OPTS) > 0)
                LONG_OPTS  = LONG_OPTS "," LONG extra_tok;
            else
                LONG_OPTS  = "-l " LONG extra_tok;
        }
    }
} END {
    print SHORT_OPTS " " LONG_OPTS
}
