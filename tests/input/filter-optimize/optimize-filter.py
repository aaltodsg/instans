#!/usr/bin/env python3
import sys


def genfile(n):
    fname = 'optimize-filter{}.trig'.format(n)
    print('Generating file {}'.format(fname))
    with open(fname, 'w') as stream:
        stream.write('@prefix : <http://instans.org/> .\n')
        for i in range(n):
            stream.write(':left :value {} .\n'.format(i))


if __name__ == "__main__":
    genfile(int(sys.argv[1]))
