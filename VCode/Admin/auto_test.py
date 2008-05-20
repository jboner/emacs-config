##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

"""Automated regression testing function.

This file contains functions for defining and running automated
regression tests.
"""

import re, sys, time
import profile
import util
from debug import trace
from Object import Object
import exceptions
from utilsqh import cleanTraceback
test_reg = {}

foreground_tests = 0x1
background_tests = 0x2
foreground_and_background = foreground_tests | background_tests

suite_reg = {'all': ['.*']}
suite_reg['highlights'] = ['automatic_abbreviations',
'mediator_console', 'python']
suite_reg['fabfour'] = ['SymDict', 'automatic_abbreviations',
'mediator_console', 'python']

suite_reg['upto'] = ['CmdInterp', 'EdSim', 'EdSim_alloc_cleanup', 'SymDict', 
'Object', 'Symdict', 'am_dictionaries', 'automatic_abbreviations']

suite_reg['mostupto'] = ['CmdInterp', 'EdSim', 'EdSim_alloc_cleanup', 
'SymDict', 'automatic_abbreviations']

suite_reg['threeupto'] = ['CmdInterp', 'EdSim', 
'SymDict', 'automatic_abbreviations']

suite_reg['twobefore'] = ['CmdInterp', 'EdSim', 
'automatic_abbreviations']

suite_reg['cibefore'] = ['CmdInterp', 'automatic_abbreviations']

suite_reg['edbefore'] = ['EdSim', 'automatic_abbreviations']
suite_reg['insdel'] = ['basic_correction', 'change_direction',
'compile_symbols', 'insert_delete']
suite_reg['insdel2'] = ['basic_correction', 'insert_delete']

test_header_fmt = '\n\n******************************************************************************\n* Test       : %s\n* Description : %s\n******************************************************************************\n\n'

class DuplicateTest(exceptions.RuntimeError):
    def __init__(self, msg):
        RuntimeError.__init__(self, msg)

class TestSuite(Object):
    """class representing a suite of automated tests

    **INSTANCE ATTRIBUTES**

    *STR* foreground -- names of foreground in the order in which they were 
    added to the suite

    *STR* background -- names of background in the order in which they were 
    added to the suite

    *{STR: FCT}* tests -- map from test names to the function to call
    to perform the test

    *{STR: STR}* descriptions -- map from test names to their descriptions

    *{STR: BOOL}* is_foreground -- map from test names to flag indicating 
    whether the test is a foreground or a background test
    *{List} (QH) files_to_skip_in_report: to strip lines in the error report
    (now the result of unittest testing also comes back in foo.txt)
    
    """
    def __init__(self, **args):
        self.deep_construct(TestSuite, 
                            {'foreground': [],
                             'background': [],
                             'tests': {},
                             'descriptions': {},
                             'is_foreground': {},
                             'files_to_skip_in_report': ['unittest.py', 'TestCaseWithHelpers.py']
                            },
                            args)

    def _run_named(self, tests, profile_prefix = None):
        """private method which runs tests by name 

        If profile_prefix is specified, use the profile module to run each
        test, sending the outputs to a file
        profile_prefix + '.' + testname + '.dat'
        """
        for test in tests:
            desc = self.descriptions[test]
            fct = self.tests[test]
            sys.stdout.write(test_header(test, desc))
            if profile_prefix is None:
                result = apply(fct)
                if result:
                    sys.stdout.write(self.clean_result(result))
                    sys.stdout.flush()
            else:
                import __main__
                __main__.test_to_profile = fct
                outfile = profile_prefix + '.' + test + '.dat'
                profile.run('test_to_profile()', outfile)
            sys.stdout.flush()

    def clean_result(self, testResult, filesToSkip=None):
        """return the enhanced result of test"""
        if testResult.wasSuccessful():
            return "tests passed"
        L = []
        L.append(self.clean_part_of_results(testResult.errors, 'errors',
                                            filesToSkip=self.files_to_skip_in_report))
        L.append(self.clean_part_of_results(testResult.failures, 'failures',
                                            filesToSkip=self.files_to_skip_in_report))
        return '\n'.join(L)

    def clean_part_of_results(self, part, name, filesToSkip):
        """produce a cleaned string for 'errors' and 'failures' of a test result"""
        if not part:
            return ""
        L = []
        L.append('\n--------------- %s -----------------\n'% name)
        for case, tb in part:
            L.append('\n---------- %s --------\n'% case)
            cleanTb = cleanTraceback(tb, filesToSkip)
            # special for VCode::
            cleanTb = util.replace_all_within_VCode(cleanTb)
            L.append(cleanTb)
        return '\n'.join(L)


    def run_foreground(self, profile_prefix = None):
        """
        Runs the foreground tests
        
        If profile_prefix is specified, use the profile module to run each
        test, sending the outputs to a file
        profile_prefix + '.' + testname + '.dat'
        """
        self._run_named(self.foreground, profile_prefix = profile_prefix)

    def run_background(self, profile_prefix = None):
        """
        Runs the background tests
        
        If profile_prefix is specified, use the profile module to run each
        test, sending the outputs to a file
        profile_prefix + '.' + testname + '.dat'
        """
        self._run_named(self.background, profile_prefix = profile_prefix)

    def add_test(self, name, fct, desc="", foreground = 0):
        """Add a test to the suite

        Adds a test with name *STR name* and function *FCT fct* to the
        tests registry. An optional description string *STR desc* can also
        be specified.

        If the test must be run with the editor in the foreground 
        (for example, if it sends keystrokes directly to the foreground
        window, or uses NatText like the current implementation of text
        mode), then the foreground argument must be true.  When running all
        tests, these tests will be run first. 
        """
        if (self.tests.has_key(name)):
            msg = "WARNING: Test '%s' defined twice" % name
            raise DuplicateTest(msg)
        else:
            self.tests[name] = fct
            self.descriptions[name] = desc
            self.is_foreground[name] = foreground
            if foreground:
                self.foreground.append(name)
            else:
                self.background.append(name)

    def foreground_count(self):
        """number of foreground tests

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of foreground tests
        """
        return len(self.foreground)

    def background_count(self):
        """number of ordinary (background) tests

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- number of ordinary (background) tests
        """
        return len(self.background)

    def count(self):
        """total number of tests

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- total number of tests
        """
        return self.foreground_count() + self.background_count()

    def has_test(self, name):
        """checks whether a test by the given name exists in the suite

        **INPUTS**

        *STR* name -- name of the test to check

        **OUTPUTS**

        *BOOL* -- true if the test exists
        """
        return self.tests.has_key(name)

    def names(self):
        """lists names of tests in the suite

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- all tests
        """
        return self.foreground + self.background
    
    def sort(self):
        """sort tests alphabetically

        Note: foreground tests and background tests are sorted
        independently, preserving the fact that foreground tests are
        always executed first.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.foreground.sort()
        self.background.sort()

    def select_all(self):
        """create a new TestSuite which is a duplicate of this one

        **INPUTS**

        *none*

        **OUTPUTS**

        *TestSuite* -- the new suite
        """
        suite = TestSuite()
        for name in self.foreground + self.background:
            suite.add_test(name, self.tests[name], 
                desc = self.descriptions[name], 
                foreground = self.is_foreground[name])
        return suite

    def select_single(self, name):
        """create a new TestSuite consisting of a single test from this
        suite

        **INPUTS**

        *STR* name -- name of the test to choose

        **OUTPUTS**

        *TestSuite* -- the new suite, or None if the test is unknown
        """
        if not self.has_test(name):
            return None
        suite = TestSuite()
        suite.add_test(name, self.tests[name], desc = self.descriptions[name], 
            foreground = self.is_foreground[name])
        return suite

    def select_list(self, names):
        """create a new TestSuite consisting of several named tests from this
        suite.  

        Note: select_list will not check whether any of the named tests
        exist, so it may return an empty suite.

        **INPUTS**

        *[STR]* names -- names of the tests to choose

        **OUTPUTS**

        *TestSuite* -- the new suite
        """
        suite = TestSuite()
        for name in names:
            if self.has_test(name):
                suite.add_test(name, self.tests[name], 
                    desc = self.descriptions[name], 
                    foreground = self.is_foreground[name])
        return suite

    def select_range(self, first = None, last = None):
        """create a new TestSuite consisting of a subsequence of tests from
        this suite

        **INPUTS**

        *STR* first -- name of the first test to choose, or None, to
        start with the first test in this suite

        *STR* last -- name of the last test to choose, or None, to
        end with the last test in this suite

        **OUTPUTS**

        *TestSuite* -- the new suite, or None if either test is unknown
        or the first test is after the last test
        """
        all_tests = self.foreground + self.background
        if not all_tests:
            return None
        if first is None:
            first = all_tests[0] 
        if last is None:
            last = all_tests[-1] 
        if not self.has_test(first):
            return None
        if not self.has_test(last):
            return None
        for i in range(len(all_tests)):
            if all_tests[i] == first:
                i_first = i
                break
        for i in range(len(all_tests)):
            if all_tests[i] == last:
                i_last = i
                break
        if i_first > i_last:
            return None
        suite = TestSuite()
        for name in all_tests[i_first:i_last + 1]:
            suite.add_test(name, self.tests[name], 
                desc = self.descriptions[name], 
                foreground = self.is_foreground[name])
        return suite

    def select_foreground(self):
        """create a new TestSuite consisting of all the foreground tests 
        from this suite.  

        Note: select_foreground will not check whether there are any 
        foreground tests in this suite, so it may return an empty suite.

        **INPUTS**

        *none*

        **OUTPUTS**

        *TestSuite* -- the new suite
        """
        suite = TestSuite()
        for name in self.foreground:
            suite.add_test(name, self.tests[name], 
                desc = self.descriptions[name], foreground = 1)
        return suite

    def select_background(self):
        """create a new TestSuite consisting of all the background tests 
        from this suite.  

        Note: select_background will not check whether there are any 
        background tests in this suite, so it may return an empty suite.

        **INPUTS**

        *none*

        **OUTPUTS**

        *TestSuite* -- the new suite
        """
        suite = TestSuite()
        for name in self.background:
            suite.add_test(name, self.tests[name], 
                desc = self.descriptions[name], foreground = 0)
        return suite

class SuiteFactory(Object):
    """object which returns a test suite by name
    
    SuiteFactory in fact maintains a TestSuite containing all the tests, 
    as well as any named suites which have been defined, so that it can
    return a new suite corresponding to any individual test, or a
    pre-existing named suite.

    **INSTANCE ATTRIBUTES**

    *TestSuite* tests -- suite consisting of all defined 
    regression tests

    *{STR: TestSuite}* suites -- map from names to suites

    *[STR]* reserved -- list of reserved names
    """
    def __init__(self, **args):
        self.deep_construct(SuiteFactory, 
                            {
                                'tests': TestSuite(),
                                'reserved': ['all', 'foreground', 'fg',
                                    'background', 'bg'],
                                'suites': {}
                            }, args)
    
    def add_test(self, name, fct, desc="", foreground = 0):
        """Add a test to the set of all tests

        Adds a test with name *STR name* and function *FCT fct* to the
        tests registry. An optional description string *STR desc* can also
        be specified.

        If the test must be run with the editor in the foreground 
        (for example, if it sends keystrokes directly to the foreground
        window, or uses NatText like the current implementation of text
        mode), then the foreground argument must be true.  When running all
        tests, these tests will be run first. 
        """
        self.tests.add_test(name, fct, desc = desc, foreground = foreground)

    def create_suite(self, name):
        """return a suite by name

        **INPUTS**

        *STR* name -- the name of the test, or of a suite previously
        defined (by select_list or select_range), or a reserved name
        (all, foreground/fg, background/bg)

        **OUTPUTS**

        *TestSuite* -- the corresponding suite, or None if the name is
        unknown
        """
        if self.has_test(name):
            return self.tests.select_single(name)
# when adding another special suite name, make sure to add the name to
# self.reserved in __init__, as well as adding an additional
# if clause here
        if name == 'all':
            return self.tests.select_all()
        if name == 'foreground' or name == 'fg':
            return self.tests.select_foreground()
        if name == 'background' or name == 'bg':
            return self.tests.select_background()
        if self.suites.has_key(name):
            return self.suites[name].select_all()
        return None

    def has_test(self, name):
        """checks whether a test by the given name exists 

        **INPUTS**

        *STR* name -- name of the test to check

        **OUTPUTS**

        *BOOL* -- true if the test exists
        """
        return self.tests.has_test(name)

    def names(self):
        """lists names of tests in the suite

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- all tests
        """
        return self.tests.names()
    
    def sort(self):
        """sort tests alphabetically

        Note: foreground tests and background tests are sorted
        independently, preserving the fact that foreground tests are
        always executed first.

        Note: if used, this function should be called before any suites are
        defined

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.tests.sort()

    def name_taken(self, name):
        """checks whether a given name is taken by a test, or an
        existing suite, or is a reserved suite name

        **OUTPUTS**

        *STR* -- error message if the suite name is taken, or None if it is not
        """
        if name in self.tests.names():
            msg = 'Name %s of suite conflicts with existing test name' % name
            return msg
        if name in self.reserved:
            msg = 'Name %s of suite conflicts with a reserved name' % name
            return msg
        if self.suites.has_key(name):
            msg = 'Name %s of suite conflicts with an existing suite' % name
            return msg
        return None

    def define_suite(self, name, tests):
        """define a new named TestSuite consisting of several named tests 

        **INPUTS**

        *STR* name -- name for the new test suite

        *[STR]* tests -- names of the tests to choose

        **OUTPUTS**

        *BOOL* -- true if the suite was successfully created
        """
        msg = self.name_taken(name)
        if msg:
            sys.stderr.write(msg)
            sys.stderr.write('\n')
            return 0
        suite = self.tests.select_list(tests)
        if suite is None or suite.count() == 0:
            msg = 'Suite %s is empty\n' % name
            sys.stderr.write(msg)
            return 0
        self.suites[name] = suite

    def define_suite_by_range(self, name, first = None, last = None):
        """define create a new TestSuite consisting of a subsequence of tests

        **INPUTS**

        *STR* first -- name of the first test to choose, or None, to
        start with the first test in this suite

        *STR* last -- name of the last test to choose, or None, to
        end with the last test in this suite

        **OUTPUTS**

        *BOOL* -- true if the suite was successfully created
        """
        msg = self.name_taken(name)
        if msg:
            sys.stderr.write(msg)
            sys.stderr.write('\n')
            return 0
        suite = self.tests.select_range(first = first, last = last)
        if suite is None or suite.count() == 0:
            msg = 'Suite %s is empty\n' % name
            sys.stderr.write(msg)
            return 0
        self.suites[name] = suite


def test_header(name, desc):
    """Returns the header of a test
    
    ... with test name *STR test_name* and description *STR desc*.
    """
    return '\n\n' + ('*' * 79) + '\n* Name        : ' + name + '\n* Description : ' + desc + '\n' + ('*' * 79) + '\n\n'

