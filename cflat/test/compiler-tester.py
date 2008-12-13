#!/usr/bin/env python

import commands
import os
import sys
from popen2 import Popen4

def compile_and_run(compiler, code):
    """Returns (compile status, output).
    compile status is 'OK' or 'BAD'.
    If compile fails, output has compiler output. If compile succeeds, it has
    the output of the program run.
    
    """
    proc = Popen4(compiler)
    proc.tochild.write(code)
    proc.tochild.close()
    if proc.wait() != 0:
        return ('BAD', proc.fromchild.read())
    proc = Popen4('./temp.exe | xargs')
    return ('OK', proc.fromchild.read().strip())

def print_indented(message):
    for line in message.splitlines():
        print "     ", line

def run_test(compiler, code, correct_result):
    """Returns True if the test passes, otherwise False."""
    status, output = compile_and_run(compiler, code)
    if correct_result == 'BAD':
        if status == 'BAD':
            print "PASS"
            return True
        elif status == 'OK':
            print "FAIL: Bad code compiled. Code:"
            print_indented(code)
            return False
    elif correct_result == 'OK':
        if status == 'BAD':
            print "FAIL: Good code didn't compile. Code:"
            print_indented(code)
            print "Compiler output:"
            print_indented(output)
            return False
        elif status == 'OK':
            print "PASS"
            return True
    else:
        if status == 'BAD':
            print "FAIL: Good code didn't compile. Code:"
            print_indented(code)
            print "Compiler output:"
            print_indented(output)
            return False
        elif correct_result == output:
            print "PASS"
            return True
        else:
            print "FAIL: Incorrect output from execution. Code:"
            print_indented(code)
            print "Executed code output:"
            print_indented(output)
            print "Correct output:"
            print_indented(correct_result)
            return False

if len(sys.argv) < 2:
    print "You must specify a test file."
    exit(1)
test_file_name = sys.argv[1]
test_file = open(test_file_name)
print "Loading test file '%s'." % test_file_name
compiler = '#'
while compiler.startswith('#'):
    compiler = test_file.readline().strip()
print "Using compile command '%s'." % compiler
test_count = 0
pass_count = 0
code = ""
for line in test_file:
    if line == "...\n":
        code = ""
    elif line.startswith("... "):
        test_count += 1
        correct_result = line.strip("\n")[4:]
        if (run_test(compiler, code, correct_result)):
            pass_count += 1
    else:
        code += line
test_file.close()
print "%d / %d tests passed." % (pass_count, test_count)
