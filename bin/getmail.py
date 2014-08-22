#!/bin/env python

# Used to let Emacs call and retrieve my mail but until it becomes
# multi-threaded, I'm going to need to run this outside to help remove and
# pauses I've observed. I also believe I can control situations where we
# need to kill and restart the mail retrieval when I've had a disruption in
# network connectivity.

import os
import sys
import subprocess
import signal
import time
from subprocess import call, PIPE

def daemonize():
    """
    do the UNIX double-fork magic, see Stevens' "Advanced
    Programming in the UNIX Environment" for details (ISBN 0201563177)
    http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16
    """
    try:
        pid = os.fork()
        if pid > 0:
            # exit first parent
            sys.exit(0)
    except OSError, e:
        sys.stderr.write("fork #1 failed: %d (%s)\n" % (e.errno, e.strerror))
        sys.exit(1)

    # decouple from parent environment
    os.chdir("/")
    os.setsid()
    os.umask(0)

    # do second fork
    try:
        pid = os.fork()
        if pid > 0:
            # exit from second parent
            sys.exit(0)
    except OSError, e:
        sys.stderr.write("fork #2 failed: %d (%s)\n" % (e.errno, e.strerror))
        sys.exit(1)

def getmail_oneshot(debug=False):
    """ One time shot of pulling email """
    cmd = 'time offlineimap -1 -o -l %s' % (os.path.expanduser('~/offlineimap.out'))
    if debug:
        cmd += ' -d imap'
    return call(cmd.split(), stdout=PIPE, stderr=PIPE)

if __name__ == '__main__':

    MAIL_INTERVAL = 60 # seconds
    from optparse import OptionParser
    parser = OptionParser(usage="%prog [options]")
    parser.add_option('-i', '--interval', dest="interval", default=MAIL_INTERVAL,
                      help="Interval to sleep inbetween mail retrievals. Defaults to %ds" % MAIL_INTERVAL)
    parser.add_option('-o', '--oneshot', dest="oneshot", default=False, action="store_true",
                      help="Run once and do not daemonize")
    parser.add_option('-v', '--verbose', dest="verbose", default=False, action="store_true",
                      help="Enable debug level logging for the mail retrieval")
    (options, args) = parser.parse_args()

    if options.oneshot:
        getmail_oneshot()
        sys.exit(0)

    print 'Going to daemonize myself and fetch your email every %ds. Chow.' % (options.interval)
    daemonize()

    while True:
        getmail_oneshot()
        time.sleep(options.interval)
