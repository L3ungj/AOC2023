import subprocess
import os
import sys

if len(sys.argv) < 2:
    print("Usage: py create.py <name>")
    exit(0)

fi = sys.argv[1]
subprocess.call(['ghc', '-o', 't', os.path.join('src', f'{fi}.hs')])

subprocess.call(['./t'])