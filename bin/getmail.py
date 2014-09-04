#!/bin/env python

# Used to let Emacs call and retrieve my mail but until it becomes
# multi-threaded, I'm going to need to run this outside to help remove and
# pauses I've observed. I also believe I can control situations where we
# need to kill and restart the mail retrieval when I've had a disruption in
# network connectivity.

import os
import sys
import subprocess
import time
import threading
import traceback
import shlex

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

class Command(object):
    """
    Enables to run subprocess commands in a different thread with TIMEOUT option.

    Based on jcollado's solution:
    http://stackoverflow.com/questions/1191374/subprocess-with-timeout/4825933#4825933
    """
    command = None
    process = None
    status = None
    output, error = '', ''

    def __init__(self, command):
        if isinstance(command, basestring):
            command = shlex.split(command)
        self.command = command

    def run(self, timeout=None, hardkill=False, **kwargs):
        """ Run a command then return: (status, output, error). """
        def target(**kwargs):
            try:
                self.process = subprocess.Popen(self.command, **kwargs)
                self.output, self.error = self.process.communicate()
                self.status = self.process.returncode
            except:
                self.error = traceback.format_exc()
                self.status = -1
        # default stdout and stderr
        if 'stdout' not in kwargs:
            kwargs['stdout'] = subprocess.PIPE
        if 'stderr' not in kwargs:
            kwargs['stderr'] = subprocess.PIPE
        # thread
        thread = threading.Thread(target=target, kwargs=kwargs)
        thread.start()
        thread.join(timeout)
        if thread.is_alive():
            if hardkill:
                self.process.kill()
            else:
                self.process.terminate()
            thread.join()


if __name__ == '__main__':

    DEFAULT_INTERVAL = 60
    DEFAULT_TIMEOUT  = 300

    from optparse import OptionParser
    parser = OptionParser(usage="%prog [options]")
    parser.add_option('-i', '--interval', dest="interval", default=DEFAULT_INTERVAL, type="int",
                      help="Interval to sleep inbetween mail retrievals. Defaults to %ds" % DEFAULT_INTERVAL)
    parser.add_option('-o', '--oneshot', dest="oneshot", default=False, action="store_true",
                      help="Run once and do not daemonize")
    parser.add_option('-t', '--timeout', dest="timeout", default=DEFAULT_TIMEOUT, type="int",
                      help="Time allowed for mail command before terminating. Defaults to %ds" % DEFAULT_TIMEOUT)
    parser.add_option('-v', '--verbose', dest="verbose", default=False, action="store_true",
                      help="Enable debug level logging for the mail retrieval")
    (options, args) = parser.parse_args()

    mail_command = 'offlineimap -1 -o -l %s' % os.path.expanduser('~/offlineimap.out')
    if options.verbose:
        mail_command += ' -d imap'

    command = Command(mail_command)

    if options.oneshot:
        command.run(timeout=options.timeout, hardkill=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        sys.exit(0)

    print 'Going to daemonize myself and fetch your email every %ds. Chow.' % (options.interval)
    daemonize()

    while True:
        command.run(timeout=options.timeout, hardkill=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        time.sleep(options.interval)
