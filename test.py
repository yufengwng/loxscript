#!/usr/bin/env python3
#
# Adapted from https://github.com/munificent/craftinginterpreters/blob/master/util/test.py

from os import listdir
from os.path import abspath, dirname, isdir, join, realpath, relpath, splitext
from subprocess import Popen, PIPE

import re
import sys


REPO_DIR = dirname(realpath(__file__))
TEST_DIR = 'spec'

OUTPUT_EXPECT = re.compile(r'#=> (.*)')
PARSE_ERROR_EXPECT = re.compile(r'#!! (.+)')
RUNTIME_ERROR_EXPECT = re.compile(r'#@! (.+)')
SYNTAX_ERROR_RE = re.compile(r'\[line \d+\] Error.+')
STACK_TRACE_RE = re.compile(r'\[line (\d+)\] in')

EX_DATAERR = 65
EX_SOFTWARE = 70


passed = 0
failed = 0
expectations = 0

interpreter = None
filter_paths = None
verbose = False


class Interpreter:
    def __init__(self):
        self.args = [join(REPO_DIR, 'target/debug/loxscript')]

    def invoke(self, path):
        args = self.args[:]
        args.append(path)
        return Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)


class Test:
    def __init__(self, path):
        self.path = path
        self.output = []
        self.parse_errors = set()
        self.runtime_error_line = 0
        self.runtime_error_message = None
        self.exit_code = 0
        self.failures = []

    def read_source(self):
        with open(self.path, 'r') as source:
            for line in source:
                yield line

    def parse(self):
        global expectations

        line_num = 1
        for line in self.read_source():
            match = OUTPUT_EXPECT.search(line)
            if match:
                self.output.append((match.group(1).strip(), line_num))
                expectations += 1

            match = PARSE_ERROR_EXPECT.search(line)
            if match:
                self.parse_errors.add(match.group(1).strip())
                self.exit_code = EX_DATAERR
                expectations += 1

            match = RUNTIME_ERROR_EXPECT.search(line)
            if match:
                self.runtime_error_line = line_num
                self.runtime_error_message = match.group(1).strip()
                self.exit_code = EX_SOFTWARE
                expectations += 1

            line_num += 1

    def run(self):
        proc = interpreter.invoke(self.path)
        out, err = proc.communicate()
        self.validate(proc.returncode, out, err)

    def validate(self, exit_code, out, err):
        if self.parse_errors and self.runtime_error_message:
            self.fail('Test error: Cannot expect both parse and runtime errors.')
            return

        try:
            out = out.decode('utf-8').replace('\r\n', '\n')
            err = err.decode('utf-8').replace('\r\n', '\n')
        except:
            self.fail('Error decoding output.')

        # Remove the trailing last empty lines.
        error_lines = err.split('\n')
        if error_lines[-1] == '':
            del error_lines[-1]
        out_lines = out.split('\n')
        if out_lines[-1] == '':
            del out_lines[-1]

        # Validate that an expected runtime error occurred.
        if self.runtime_error_message:
            self.validate_runtime_error(error_lines)
        else:
            self.validate_parse_errors(error_lines)

        self.validate_exit_code(exit_code, error_lines)
        self.validate_output(out_lines)

    def validate_runtime_error(self, error_lines):
        if len(error_lines) < 1:
            self.fail('Expected runtime error "{}" but got none.', self.runtime_error_message)
            return

        line = error_lines[0]
        if line != self.runtime_error_message:
            self.fail('Expected runtime error "{}" but got:', self.runtime_error_message)
            self.fail(line)

        match = None
        for ln in error_lines:
            match = STACK_TRACE_RE.search(ln)
            if match:
                break

        if not match:
            self.fail('Expected stack trace but got none.')
        else:
            stack_line = int(match.group(1))
            if stack_line != self.runtime_error_line:
                self.fail('Expected runtime error on line {} but was on line {}',
                        self.runtime_error_line, stack_line)

    def validate_parse_errors(self, error_lines):
        # Validate that every lexing/parsing error was expected.
        found_errors = set()
        num_unexpected = 0
        for line in error_lines:
            line = line.strip()
            match = SYNTAX_ERROR_RE.search(line)
            if match:
                if line in self.parse_errors:
                    found_errors.add(line)
                else:
                    if num_unexpected < 10:
                        self.fail('Unexpected error:')
                        self.fail(line)
                    num_unexpected += 1
            elif line != '':
                if num_unexpected < 10:
                    self.fail('Unexpected output on stderr:')
                    self.fail(line)
                num_unexpected += 1

        if num_unexpected > 10:
            self.fail('(truncated {} more...)', num_unexpected - 10)

        # Validate that every expected error occurred.
        for error in self.parse_errors - found_errors:
            self.fail('Missing expected error: {}', error)

    def validate_exit_code(self, exit_code, error_lines):
        if exit_code == self.exit_code: return

        if self.failures and self.failures[-1] != '---':
            self.failures.append('---')

        if len(error_lines) > 10:
            error_lines = error_lines[0:10]
            error_lines.append('(truncated...)')
        self.fail('Expected return code {} but got {}. Stderr:', self.exit_code, exit_code)
        self.failures += error_lines

    def validate_output(self, out_lines):
        appended = False
        def append_delimiter():
            nonlocal appended
            if not appended and self.failures and self.failures[-1] != '---':
                self.failures.append('---')
            appended = True

        idx = 0
        for line in out_lines:
            if idx >= len(self.output):
                append_delimiter()
                self.fail('Got output "{}" when none was expected.', line)
            elif self.output[idx][0] != line:
                append_delimiter()
                self.fail('Expected output "{}" on line {} but got "{}".',
                        self.output[idx][0], self.output[idx][1], line)
            idx += 1

        while idx < len(self.output):
            append_delimiter()
            self.fail('Missing expected output "{}" on line {}.',
                    self.output[idx][0], self.output[idx][1])
            idx += 1

    def fail(self, message, *args):
        if args:
            message = message.format(*args)
        self.failures.append(message)


def run_script(path):
    global passed
    global failed

    if (splitext(path)[1] != '.lox'):
        return

    # Check if we are just running a subset of the tests.
    if filter_paths:
        this_test = relpath(path, join(REPO_DIR, TEST_DIR))
        matched_filter = False
        for filter_path in filter_paths:
            if this_test.startswith(filter_path):
                matched_filter = True
                break
        if not matched_filter:
            return

    # Normalize path to relative path.
    path = relpath(path).replace('\\', '/')

    # Update the status line.
    status("Passed: {} Failed: {} {}".format(green(passed), red(failed), gray(f"({path})")))

    # Read the test and parse out the expectations.
    test = Test(path)
    test.parse()
    test.run()

    # Display the results.
    if len(test.failures) == 0:
        passed += 1
        if verbose:
            status(green('PASS') + ': ' + path)
            print('')
    else:
        failed += 1
        status(red('FAIL') + ': ' + path)
        print('')
        for failure in test.failures:
            print('      ' + pink(failure))
        print('')


def run_suite():
    global interpreter
    global expectations
    global passed
    global failed

    interpreter = Interpreter()
    expectations = 0
    passed = 0
    failed = 0

    walk(join(REPO_DIR, TEST_DIR), run_script)
    status()

    successful = (failed == 0)
    if successful:
        print("All {} tests passed ({} expectations).".format(green(passed), expectations))
    else:
        print("{} tests passed. {} tests failed.".format(green(passed), red(failed)))

    return successful


def walk(folder, callback):
    folder = abspath(folder)
    for entry in listdir(folder):
        nfile = join(folder, entry)
        if isdir(nfile):
            walk(nfile, callback)
        else:
            callback(nfile)


def status(line = None):
    # Erase the status line.
    print('\033[2K', end='')
    # Move the cursor to the beginning.
    print('\r', end='')
    if line:
        print(line, end='')
        sys.stdout.flush()


def color_text(text, color):
    # No ANSI escapes on Windows.
    if sys.platform == 'win32':
        return str(text)
    return color + str(text) + '\033[0m'


def red(text):      return color_text(text, '\033[31m')
def green(text):    return color_text(text, '\033[32m')
def yellow(text):   return color_text(text, '\033[33m')
def pink(text):     return color_text(text, '\033[91m')
def gray(text):     return color_text(text, '\033[1;30m')


def print_usage():
    print('Usage: test.py [-v] [filters]')


def main(args):
    global filter_paths
    global verbose

    if len(args) > 2:
        print_usage()
        sys.exit(1)
    elif len(args) == 2:
        if args[0] != '-v':
            print_usage()
            sys.exit(1)
        verbose = True
        filter_paths = args[1].split(',')
    elif len(args) == 1:
        if args[0] == '-v':
            verbose = True
        else:
            filter_paths = args[0].split(',')

    if not run_suite():
        sys.exit(1)


if __name__ == '__main__':
    main(sys.argv[1:])
