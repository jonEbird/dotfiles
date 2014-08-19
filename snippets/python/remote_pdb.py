#!/bin/env python

# Taken from http://www.dzone.com/snippets/remote-debugging-python-using
#   Only added the SO_REUSEADDR socket option since I hit the breakpoint often

import pdb, socket, sys

class Rdb(pdb.Pdb):
    def __init__(self, port=4444):
        self.old_stdout = sys.stdout
        self.old_stdin = sys.stdin
        self.skt = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.skt.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.skt.bind((socket.gethostname(), port))
        self.skt.listen(1)
        (clientsocket, address) = self.skt.accept()
        handle = clientsocket.makefile('rw')
        pdb.Pdb.__init__(self, completekey='tab', stdin=handle, stdout=handle)
        sys.stdout = sys.stdin = handle

    def do_continue(self, arg):
        sys.stdout = self.old_stdout
        sys.stdin = self.old_stdin
        self.skt.close()
        self.set_continue()
        return 1

    do_c = do_cont = do_continue

# Example usage - connect with 'telnet <hostname> 4444'
# https://github.com/nblock/pdb-cheatsheet
#  Things to try:
#   1. 'where' - shows a call stack
#   2. 'list' - show current code surrounding point
if __name__=='__main__':
    def buggy_method():
        x = 3
        remote_debug = Rdb()
        remote_debug.set_trace()
        print x

    buggy_method()
