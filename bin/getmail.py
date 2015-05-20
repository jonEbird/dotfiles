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
import getpass
import re
try:
    import gnupg
except ImportError, e:
    print >> sys.stderr, "Need the gnupg module. Try: sudo pip install python-gnupg"
    sys.exit(1)
try:
    import gnomekeyring as gk
except ImportError:
    print >> sys.stderr, """Unable to import gnome keyring module
On Debian like systems you probably need: python-gnomekeyring
Or on Feodara systems, you want: gnome-python2-gnomekeyring"""
    sys.exit(1)

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

def get_netrc(retries=3):
    """ Reads my GPG encrypted ~/.netrc.gpg file and returns list of tuples
    of (machine, login, password) """
    netrc_data = []
    gpg = gnupg.GPG()
    while not netrc_data and retries:
        passphrase = getpass.getpass('GPG Passphrase: ')
        d = gpg.decrypt_file(open(os.path.expanduser("~/.netrc.gpg"), "rb"),
                             passphrase=passphrase)
        if d.ok:
            netrc_data = re.findall('^machine (.*?) login "?(.*?)"? password ?(.*)$',
                                    d.data, re.M)
            break
        else:
            print 'Bad passphrase. Try again (retries (%d) > 0)' % retries
            retries = retries - 1
    return netrc_data


class KeyringManager(object):

    def __init__(self, app="msmtp", protocol="smtp"):
        self.app = app
        self.protocol = protocol
        self.valid_hosts = []
        try:
            self.keyring = gk.get_default_keyring_sync()
        except gk.NoKeyringDaemonError:
            print >> sys.stderr, "Error: Can not open gnome keyring."
            print >> sys.stderr, "Are you running a GNOME session?"
            sys.exit(2)

    def __valid_hosts(self, app, protocol):
        """ Return a valid list of acceptable host names to match against
        based on 'app' and/or 'protocol' """
        valid_hosts = []
        if app == "msmtp":
            with open(os.path.expanduser("~/.msmtprc"), 'r') as f:
                data = f.read()
            smtp_hosts = re.findall("^host\s+(.*)$", data, re.M)
            valid_hosts += smtp_hosts
        # Add other elif blocks as needed
        return valid_hosts

    def put_pass(self, username, password, server):
        keyring_type = gk.ITEM_NETWORK_PASSWORD
        display_name = '%s password for %s at %s' % (self.app.upper(), username, server)
        usr_attrs = {'user': username, 'server': server, 'protocol': self.protocol}
        id = gk.item_create_sync(self.keyring, keyring_type,
                                 display_name, usr_attrs, password, False)
        return id is not None

    def get_pass(self, username, server):
        """ Return the password from the keyring. """
        for retries in range(3):
            try:
                results = gk.find_network_password_sync(user=username, server=server,
                                                        protocol=self.protocol)
                return results[0]["password"]
            except gk.NoMatchError:
                print >> sys.stderr, "No password set for user '%s' in server '%s'" % \
                    (username, server)
                return ''
            except gk.IOError as e:
                print >> sys.stderr, "Problem retrieving password: %s" % str(e)
        return ''

    def populate_keyring(self, app="msmtp", protocol="smtp"):
        """Populate keyring with all of my email netrc passwords """
        self.valid_hosts = self.__valid_hosts(app, protocol)

        for machine, login, password in get_netrc():
            if machine in self.valid_hosts:
                self.put_pass(login, password, machine)


def get_password(username, server):
    """ Retrieve password from KeyringManager """
    km = KeyringManager()
    return km.get_pass(username, server)

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
    parser.add_option('--nokeyring', dest="nokeyring", default=False, action="store_true",
                      help="Bypass populating the Gnome keyring with passwords")
    (options, args) = parser.parse_args()

    if not options.nokeyring:
        km = KeyringManager()
        km.populate_keyring()

    mail_command = 'offlineimap -1 -o -l %s' % os.path.expanduser('~/offlineimap.out')
    if options.verbose:
        mail_command += ' -d imap'

    command = Command(mail_command)

    if options.oneshot:
        command.run(timeout=options.timeout, hardkill=True,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        sys.exit(0)

    print 'Going to daemonize myself and fetch your email every %ds. Chow.' % (options.interval)
    daemonize()

    while True:
        command.run(timeout=options.timeout, hardkill=True,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        time.sleep(options.interval)
