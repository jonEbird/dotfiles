#!/bin/env python

import os
import sys
import subprocess
import time

def has_wmctrl_installed(cmd='wmctrl'):
    """ Check whether you have wmctrl installed or not """
    for path in os.getenv('PATH').split(':'):
        if os.path.exists(os.path.join(path, cmd)):
            return True
    return False

def wmctrl(*args):
    """ Run the wmctl with the arguments passed and return the stdout """
    #print 'DEBUG: Running: %s' % (' '.join(['wmctrl'] + list(args)))
    p = subprocess.Popen(['wmctrl'] + list(args), stdout=subprocess.PIPE)
    #p.wait()
    stdout, stderr = p.communicate()
    if stderr:
        print 'STDERR: "%s"' % (stderr)
    return stdout.strip()

def pid2name(pid):
    """ Return the "comm" name of the process id """
    if int(pid) < 1:
        return ''
    cmd = 'ps --no-header -o comm -p %s' % (str(pid))
    p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE)
    p.wait()
    stdout, stderr = p.communicate()
    return stdout.strip()

class Wmctrl(object):
    """ Helpful object for collecting information on WM via the wmctrl
    utility. """
    wm_xoffset = 0
    wm_yoffset = 0
    win_info = []
    __calibrated = False

    def __init__(self):
        self.refresh_wininfo()

    def get_wininfo(self):
        """ Retrive win_info """
        new_winfo = []
        for line in wmctrl('-lGp').split('\n'):
            try:
                w_id, desktop, pid, x_offset, y_offset, width, height, host, title = line.split(None, 8)
                if int(pid) < 1:
                    continue
                if self.__calibrated:
                    new_winfo.append((w_id, desktop, pid,
                                      int(x_offset) - self.wm_xoffset, int(y_offset) - self.wm_yoffset,
                                      width, height, host, title))
                else:
                    new_winfo.append((w_id, desktop, pid, x_offset, y_offset, width, height, host, title))
            except ValueError, e:
                print 'Problem parsing line "%s"' % line
        return new_winfo

    def refresh_wininfo(self):
        """ Refresh win_info with the current state of X11 windows on the
        machine """
        self.win_info = self.get_wininfo()

    def get_geometry(self, win):
        self.refresh_wininfo()
        for w_id, desktop, pid, x_offset, y_offset, width, height, host, title in self.win_info:
            if w_id == win:
                return int(x_offset), int(y_offset), int(width), int(height)
        return 0, 0, 0, 0

    def identify(self, name):
        self.refresh_wininfo()
        for w_id, desktop, pid, x_offset, y_offset, width, height, host, title in self.win_info:
            if str(name).lower() in '%s %s' % (pid2name(pid), title.lower()):
                return w_id
        return ''

    def calibrate(self):
        """ Calculate the current geometry of the particular window (win),
        apply the value and then re-check the new geometry to observe the
        offset. Finally move the window back to original location and return x,
        y offsets """
        if self.__calibrated:
            return
        self.refresh_wininfo()
        win = self.win_info[0][0]
        x1, y1, w, h = self.get_geometry(win)
        if not w:
            print 'DEBUG: not working'
            return 0, 0
        args = '-i -r %s -e 0,%s,%s,%d,%d' % (win, x1, y1, w, h)
        wmctrl(*args.split())
        time.sleep(.25) # Give time for command to complete
        x2, y2, w, h = self.get_geometry(win)
        self.wm_xoffset = abs(x2 - x1)
        self.wm_yoffset = abs(y2 - y1)
        # Now move the window back
        args = '-i -r %s -e 0,%s,%s,%d,%d' % (win, x1 - self.wm_xoffset, y1 - self.wm_yoffset, w, h)
        wmctrl(*args.split())
        self.__calibrated = True
        self.refresh_wininfo()

    def get_commands(self):
        """ Produce commands which can be used to reset windows to their
        current location and geometry """
        self.calibrate()
        cmds = []
        for w_id, desktop, pid, x_offset, y_offset, width, height, host, title in self.win_info:
            cmd = 'wmctrl -i -r %s -e 0,%s,%s,%s,%s # %s' % (w_id, x_offset, y_offset, width, height, pid2name(pid))
            cmds.append(cmd)
        return cmds

if __name__ == '__main__':

    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-s", "--script", dest="script", action="store_true", default=False,
                      help="Generate wmctrl commands to place windows back to where they are now")
    parser.add_option("-O", "--output", dest="output", default="-",
                      help="File to send output to. Defaults to stdout")
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                      help="Show additional information")
    (options, args) = parser.parse_args()

    W = Wmctrl()
    if options.output == "-":
        out_fh = sys.stdout
    else:
        try:
            out_fh = open(options.output, 'w')
        except (OSError, IOError), e:
            print >> sys.stderr, 'Problem opening file "%s" for output: %s' % (options.output, str(e))

    if options.verbose:
        W.calibrate()
        print '# Calibrated offset to be (%d, %d)' % (W.wm_xoffset, W.wm_yoffset)

    if options.script:
        out_fh.write('#!/bin/bash\n\n# Commands generated from %s\n\n%s\n' % (sys.argv[0], '\n'.join(W.get_commands())))
        out_fh.write('pkill -x -SIGUSR1 conky\n')

    out_fh.close()
