#!/bin/bash

for S in $SERVERS; do
    ssh-copy-id root@$S
done
