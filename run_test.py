#!/usr/bin/python3

"""
A little tool for testing stdout of the compiler

- It collects *.java files in the test directory, then run the compiler
in verbose mode in the command line and compare the output with the expected one.
- By convention, the expected file shares the same name as the test file.
for example: hello.expected for hello.java
"""

import os
import subprocess
import sys
import argparse

TEST_DIR = "tests"
EXPECTED_EXTENTION = ".expected"
COMPILER = "./Main.native"

# { test_case: message }
ERR_REPORT = {}
FAILED_REPORT = {}
PASSED_REPORT = {}


def run_test (test_file):
    # run the test
    p = subprocess.Popen([COMPILER, '-v', test_file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out = p.stdout.read()

    # load expected file
    expected_file = test_file[:-5] + EXPECTED_EXTENTION
    if os.path.exists(expected_file):
        with open(expected_file, "r") as f:
            expected = f.read()
            if  expected != out:
                FAILED_REPORT[test_file] = "\n# Expected:\n{0}\n# Got:\n{1}".format(expected, out)
            else:
                PASSED_REPORT[test_file] = out
    else:
        ERR_REPORT[test_file] = "file {0} not found!\n# Got:\n{1}".format(expected_file, out)


def print_report(report, msg):
    if len(report) > 0:
        for test_case, err in report.items():
            print("{0} in {1}: {2}".format(msg, test_case, err))


def print_summary():
    print("===========================================")
    print("{0} passed; {1} failed; {2} error".format(len(PASSED_REPORT), len(FAILED_REPORT), len(ERR_REPORT)))
    print("===========================================")


def main(test_dir=TEST_DIR, test_file=None):

    # build
    print ("building...")
    p = subprocess.Popen(['ocamlbuild', COMPILER],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if (p.returncode != 0):
        print ("Failed to build!")
        print (out)
        print (err)
        exit()

    java_files = []
    if test_file is None:
        # also search the sub directories
        for root, dirs, files in os.walk(test_dir):
            for f in files:
                filepath = os.path.join(root, f)
                if filepath.endswith(".java"):
                    java_files.append(filepath)
    else:
        java_files = [os.path.join(test_dir, f) for f in os.listdir(test_dir) if f.endswith(".java")
                        and os.path.isfile(os.path.join(TEST_DIR, f)) and f == test_file]

    print ("testing...")
    map(run_test, java_files)
    print_report(ERR_REPORT, "**Error")
    print_report(FAILED_REPORT, "**Failed")
    print_summary()


def parse_args(options):
    parser = argparse.ArgumentParser(description="miniJavac testing tool")
    parser.add_argument(
        "-d", "--testdir", default=TEST_DIR,
        help="The test directory to run")
    parser.add_argument(
        "-f", "--testfile",
        help="test file to run")
    return parser.parse_args()


if __name__ == "__main__":
    options = parse_args(sys.argv[1:])
    main(options.testdir, options.testfile)
