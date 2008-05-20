################################################################################
# IIPyhonUtils
# 
# A library of Python utility classes developped by the
# Interactive Information Group of the Institute for Information Technology
# at the National Research Council of Canada (NRC).
#
#
# (c) National Research Council of Canada, 2000
################################################################################

from exceptions import Exception
import unittest

import TestCaseWithHelpers
import debug, util

class DummyObject1:
    def __init__(self, do1_attr1, do1_attr2):
        self.do1_attr1 = do1_attr1
        self.do1_attr2 = do1_attr2

class DummyObject2:
    def __init__(self, do2_attr1, do2_attr2):
        self.do2_attr1 = do2_attr1
        self.do2_attr2 = do2_attr2

        
class MyTestCaseWithHelpers(TestCaseWithHelpers.TestCaseWithHelpers):
    def __init__(self, name):
        TestCaseWithHelpers.TestCaseWithHelpers.__init__(self, name)
        
    def test(self):
        # We need this, otherwise when we instantiate MyTestCaseWithHelpers,
        # PyUnit will complain that the class does not have a test() method.
        pass

class TestCaseWithHelpersTest(unittest.TestCase):
    def __init__(self, name):
        unittest.TestCase.__init__(self, name)
        
    def setUp(self):
       self.test_case = MyTestCaseWithHelpers('test')
    
    def test_assert_equal_integers(self):
        self.assert_that__assert_equal__fails(1, 2, 
                                                        "Comparing different integers did not raise an exception")
        self.assert_that__assert_equal__succeeds(1, 1, 
                                                        "Comparing equal integers raised an exception")
            
    def test_assert_equal_floats(self):            
        self.assert_that__assert_equal__fails(1.0, 2.0, 
                                                        "Comparing different floats did not raise an exception")
        self.assert_that__assert_equal__succeeds(1.0, 1.0, 
                                                        "Comparing equal floats raised an exception")

    def test_assert_equal_strings(self):
        err = None
        self.test_case.assert_equal_string('hello', 'hello')

        try:
            self.test_case.assert_equal_string('hi', 'ha')
            self.fail("Exception not raised when comparing strings that differ at second character only.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(1, err)
        
        try:
            self.test_case.assert_equal_string('hi', 'yi')
            self.fail("Exception not raised when comparing strings that differ at first character only.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(0, err)
            
        try:
            self.test_case.assert_equal_string('hello', 'hello world')
            self.fail("Exception not raised when first string is a substring of the second one.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(5, err)

        try:
            self.test_case.assert_equal_string('hello world', 'hello')
            self.fail("Exception not raised when first string is a superstring of the second one.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(5, err)

        try:
            self.test_case.assert_equal_string('', 'hello')
            self.fail("Exception not raised when first string only is empty.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(-1, err)

        try:
            self.test_case.assert_equal_string('hello', '')
            self.fail("Exception not raised when second string only is empty.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(-1, err)

        self.test_case.assert_equal_string('', '')
        
        try:
            self.test_case.assert_equal_string(None, 'hello')
            self.fail("Exception not raised when first string only is None.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(-1, err)

        try:
            self.test_case.assert_equal_string('hello', None)
            self.fail("Exception not raised when second string only is None.")
        except TestCaseWithHelpers.AssertStringEqualsFailed, err:
            self.assert_str_comparison_err_was(-1, err)

        self.test_case.assert_equal_string(None, None)
            
    def test_assert_equal_lists(self):        
        self.assert_that__assert_equal__fails(
             [0, 1, 2], [0, 1], 
              "Comparing lists of different lenghts did not raise an exception")
        self.assert_that__assert_equal__fails(
             [0, 1, 2], [0, 1, 3], 
              "Comparing lists with different entries did not raise an exception")
        self.assert_that__assert_equal__succeeds(
             [0, 1, 2], [0, 1, 2],
             "Comparing identical lists raised an exception")
        
    def test_assert_equal_tuples(self):        
        self.assert_that__assert_equal__fails(
             (0, 1, 2), (0, 1), 
              "Comparing tuples of different lenghts did not raise an exception")
        self.assert_that__assert_equal__fails(
             (0, 1, 2), (0, 1, 3), 
              "Comparing tuples with different entries did not raise an exception")
        self.assert_that__assert_equal__succeeds(
             (0, 1, 2), (0, 1, 2),
             "Comparing identical tuples raised an exception")
        
    def test_assert_equal_dictionaries(self):        
        self.assert_that__assert_equal__fails(
             {'a': 1,  'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'c': 3, 'd': 4}, 
              "Comparing dictionaries of different lenghts did not raise an exception")
        self.assert_that__assert_equal__fails(
              {'a': 1,  'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'z': 3},  
              "Comparing lists with different keys did not raise an exception")
        self.assert_that__assert_equal__fails(
              {'a': 1,  'b': 2, 'c': 3}, {'a': 1, 'b': 2, 'c': 10},  
              "Comparing lists with different values did not raise an exception")

        self.assert_that__assert_equal__succeeds(
             [0, 1, 2], [0, 1, 2],
             "Comparing identical lists raised an exception")

    def test_assert_equal_objects(self):
        self.assert_that__assert_equal__fails(
                DummyObject1(1, 2), DummyObject1(1, 10),
                "Comparing objects of same type but different attributes should have raised an exception but didn't.")
        self.assert_that__assert_equal__succeeds(
                DummyObject1(1, 2), DummyObject1(1, 2),
                "Comparing equivalent objects raised an exception.")
        self.assert_that__assert_equal__fails(
                DummyObject1(1, 2), DummyObject2(1, 2),
                "Comparing objects of different types should have raised an exception but didn't.")
        
    def test_assert_equal_nested_data_structures(self):        
        self.assert_that__assert_equal__fails(
                [{'a': 1}, {'b': 2, 'c': DummyObject1(1, 2)}],
                [{'a': 1}, {'b': 2, 'c': DummyObject1(1, 999)}],
                "Comparing different nested data structures should have raised an exception but didn't.")
        self.assert_that__assert_equal__succeeds(
                [{'a': 1}, {'b': 2, 'c': DummyObject1(1, 2)}],
                [{'a': 1}, {'b': 2, 'c': DummyObject1(1, 2)}],
                "Comparing equivalent nested data structures raised an exception.")

    def test_assert_equal_with_epsilon(self):
        self.assert_that__assert_equal__fails(1, 2, "Numbers should not have been within epsilon of each other", 0.5)
        self.assert_that__assert_equal__succeeds(1, 1.5, "Numbers should have been within epsilon of each other", 0.5)
                
    def test_infinite_loop_in_object_structure(self):
        do1a = DummyObject1(1, 2)
        do1b = DummyObject1(1, 2)
        do1a.do1_attr1 = do1b
        do1b.do1_attr1 = do1a
        try: 
           self.test_case.assert_equal(do1a, do1b)
           self.fail("Comparing objects with infinite in their composition structure did not raise exception.")
        except RuntimeError, err:
            self.test_case.assert_equal(
                              err.args, 
                              ("maximum recursion depth exceeded.\n" + \
                               "Error happened while doing an assert_equal().\n" + \
                               "Maybe one of the arguments of assert_equal() has an infinite loop in its composition structure?", 
                               ),
                              "Comparing objects with infinite in their composition raised WRONG exception."
                               
)
        
##############################################################################
# Assertions and helpers
##############################################################################

    def assert_str_comparison_err_was(self, exp_first_diff_pos, str_compar_err):
       self.assert_(exp_first_diff_pos == str_compar_err.first_diff, 
                    "The position of the first difference was wrong.\nGot: %s\nExpected: %s" % 
                    (exp_first_diff_pos, str_compar_err.first_diff))
       # make sure can convert the exception into a string.                  
       message = "%s" % str_compar_err
       
    def assert_that__assert_equal__fails(self, expected, got, message, epsilon=0):
        exception_was_raised = False
        try:
            self.test_case.assert_equal(expected, got, message, epsilon)
            message = "%s\nRecursive comparison between those two items should have failed but it didn't." % message
            message = "%s\nGot argument was:\n%s\nExpected argument was:\n%s" % (message, repr(got), repr(expected))
        except AssertionError, err:
            exception_was_raised = True 
            
        if not exception_was_raised:
            self.fail(message)
            

    def assert_that__assert_equal__succeeds(self, expected, got, message, epsilon=0):
        message = "%s\nRecursive comparison between those two items should have succeeded but it didn't." % message
        message = "%s\nGot argument was:\n%s\nExpected argument was:\n%s" % (message, got, expected)
        self.test_case.assert_equal(expected, got, message, epsilon)
            
            

