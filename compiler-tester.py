#!/usr/bin/env python

import commands
import os
import sys

def compile_and_run(compiler, code):
    """Returns (compile status, output).
    compile status is 'GOOD' or 'BAD'.
    If compile fails, output has compiler output. If compile succeeds, it has
    the output of the program run.
    
    """
    stdin, stdouterr = os.popen4(compiler)
    stdin.write(code)
    stdin.close()
    compiler_ouput = stdouterr.readlines()
    stdouterr.close()
    return ('BAD', ''.join(compiler_ouput))


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
        elif status == 'GOOD':
            print "FAIL: Bad code compiled. Code:"
            print_indented(code)
            return False
    elif correct_result == 'GOOD':
        if status == 'BAD':
            print "FAIL: Good code didn't compile. Code:"
            print_indented(code)
            print "Compiler output:"
            print_indented(output)
            return False
        elif status == 'GOOD':
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
            print "Browser simulator output:"
            print_indented(output)
            return False


test_file_name = sys.argv[1]
test_file = open(test_file_name)
compiler = test_file.readline().strip()
print test_file_name, compiler
test_count = 0
pass_count = 0
code = ""
for line in test_file:
    if line == ">>>\n":
        code = ""
    elif line[:4] == ">>> ":
        test_count += 1
        correct_result = line[4:-1]
        if (run_test(compiler, code, correct_result)):
            pass_count += 1
    elif line[:1] != "#":
        code += line
test_file.close()

print "%d / %d tests passed." % (pass_count, test_count)
