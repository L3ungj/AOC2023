import subprocess
import os
import sys

if len(sys.argv) < 2:
    print("Usage: py run.py <name>")
    exit(0)

fi = sys.argv[1]
subprocess.call(['ghc',
                 '-O2',
                 '-o', 't', os.path.join('src', f'{fi}.hs')])

subprocess.call(['./t'], stdout=open("fo.txt", "w"), stderr=open("fe.txt", "w"))