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
import exceptions
import os
import re
import string
import types
import unittest
from pprint import pformat

import debug


class AssertStringEqualsFailed(AssertionError):
   def __init__(self, exp_string, got_string, first_diff, orig_message):
      self.exp_string = exp_string
      self.got_string = got_string
      self.first_diff = first_diff
      self.orig_message = orig_message
      
   def __str__(self):
       if (self.first_diff < 0):
           format = "%s\nOne string was empty but not the other.\n" + \
                    "   Expected: \n%s\n" + \
                    "   Got: \n%s\n"
           message = format % (self.orig_message, self.exp_string, self.got_string)
       else:
           exp_with_tilde = self.exp_string[:self.first_diff] + '~~~' + self.exp_string[self.first_diff:]
           got_with_tilde = self.got_string[:self.first_diff] + '~~~' + self.got_string[self.first_diff:]         
           format = "%s\n----\nStrings differed. See triple tilde (~~~) position below.\n" + \
                     "Expected:\n" + \
                     "   '%s'\n" + \
                     "Got:\n" + \
                     "   '%s'\n"
           message = format % (self.orig_message, exp_with_tilde, got_with_tilde)
       return message

class TestCaseWithHelpers(unittest.TestCase):
    """A subclass of pyUnit TestCase, that has helper method to
    facilitate unit testing.
    """

    def __init__(self, name):
        unittest.TestCase.__init__(self, name)
        
    def remind_me_to_implement_this_test(self):
        self.fail("DON'T FORGET TO IMPLEMENT THIS TEST!!!")
        
    def assert_equal(self, expected, got, mess="", epsilon=0):

        debug.trace('assert_equal', 'expected=%s, got=%s' % (expected, got))
        try:
           self.assert_equal_dispatch_according_to_type(expected, got, mess, epsilon)
        except RuntimeError, err:
            err = self.check_for_infinite_recursion_error(err)
            raise err
        
    def assert_equal_dispatch_according_to_type(self, expected, got, mess, epsilon):
        self.assert_(self.isnumber(epsilon), "assert_equal called with a value of epsilon that was not a number. Type of epsilon was: %s, value was %s" % (self.what_class(epsilon), epsilon))
        if (self.what_class(expected) != self.what_class(got) 
            # Note: Floats can be considered same type as ints
            and not (self.isnumber(expected) and self.isnumber(got))):
            mess = mess + "\n----\nThe two objects were not of same type"
            mess = mess + "\nExpected:\n   %s\nGot:\n   %s" % (expected, got)
            self.fail(mess)

        if (self.issequence(expected)):
            debug.trace('TestCaseWithHelpers.assert_equal_dispatch_according_to_type', "** comparing sequences")
            self.assert_equal_sequence(expected, got, mess, epsilon) 
        elif (self.isdictionary(expected)):
            debug.trace('TestCaseWithHelpers.assert_equal_dispatch_according_to_type', "** comparing dictionaries")
            self.assert_equal_dictionary(expected, got, mess, epsilon) 
        elif (self.isnumber(expected)):
            mess = mess + "\n----\nThe two numbers differed significantly."
            mess = mess + "\nExpected:\n   %s\nGot:\n   %s" % (expected, got)
            if epsilon == 0:
               self.assert_(expected == got, mess)
            else:
               self.assert_((got <= expected + epsilon) and (got >= expected - epsilon), 
                        mess + "\nValue was not within epsilon=%s range of expected value." % epsilon)
        elif (self.isstring(expected)):
            debug.trace('TestCaseWithHelpers.assert_equal_dispatch_according_to_type', "** comparing string types")
            self.assert_equal_string(expected, got, mess)
        elif (self.isbasetype(expected)):
            debug.trace('TestCaseWithHelpers.assert_equal_dispatch_according_to_type', "** comparing simple base types")
            self.assert_(expected == got, mess) 
        else:
            debug.trace('TestCaseWithHelpers.assert_equal_dispatch_according_to_type', "** comparing objects")
            self.assert_equal_objects(expected, got, mess, epsilon)
        

    def assert_equal_string(self, exp_string, got_string, message=''):
       first_diff = self.find_first_diff_char(got_string, exp_string)
         
       if first_diff != None:
          raise AssertStringEqualsFailed(exp_string, got_string, first_diff, message)
    
    def assert_equal_sequence(self, expected, got, mess="", epsilon=0):
        debug.trace('TestCaseWithHelpers.assert_equal_sequence', 
                    "** expected=%s, got=%s" % (expected, got))
        mess = mess + "\n----\nThe two sequences differred\nExpected sequence:\n   %s\nGot sequence:\n   %s" % \
               (pformat(expected), pformat(got))
        
        self.assert_equal(len(expected), len(got), mess + "\nThe two sequences did not have the same length.")
        
        for ii in range(len(expected)):
           mess_ii = mess + "\nThe two sequences differed at element ii=%s." % (ii)
           self.assert_equal(expected[ii], got[ii], mess_ii, epsilon)

    def assert_equal_dictionary(self, expected, got, mess="", epsilon=0):
        expected_keys = expected.keys()
        expected_keys.sort()
        got_keys = got.keys()
        got_keys.sort()
        
        mess = mess + "\n----\nThe two dictionaries differed"
        mess = mess + "\nExpected:\n   %s\nGot:\n   %s" % (pformat(expected), pformat(got))
        
        # NOTE: Keys must be exactly equal, even if epsilon is greater than 0.
        self.assert_equal(expected_keys, got_keys, 
                              mess + "\nDictionaries did not have the same list of keys.")
        for a_key in expected_keys:
            self.assert_equal(expected[a_key], got[a_key], 
                    mess + "\nValues of dictionaries differed at key '%s'" % a_key, 
                    epsilon)

    
    def assert_equal_objects(self, expected, got, mess, epsilon=0):
        self.assert_equal(self.what_class(expected), self.what_class(got),
                           mess + "\n----\nThe two objects were not of the same class or type.")

        self.assert_equal(expected.__dict__, got.__dict__, 
                           mess + "\n----\nAttributes of the two objects differed." 
                                + "\nExpected:\n   %s\nGot:\n   %s" % (expected.__dict__, got.__dict__), 
                           epsilon)  

    def check_for_infinite_recursion_error(self, err):
        if (isinstance(err, RuntimeError) and
            err.args == ('maximum recursion depth exceeded', )):
            err.args = \
               ("maximum recursion depth exceeded.\n" + \
                "Error happened while doing an assert_equal().\n" + \
                "Maybe one of the arguments of assert_equal() has an infinite loop in its composition structure?", 
                )
        return err
                   
    def assert_not_equal(self, not_expected, got, mess):
        if got == not_expected:
           mess = mess + "\nValues were equal when they should not have been. Got: %s" % got
           self.fail(mess)

    def assert_string_contains(self, pattern, the_string, mess=''):
        self.assert_(string.find(the_string, pattern) != -1, 
                     mess + "\nSubstring: '%s' was not found in string: '%s'" % (pattern, the_string))

    def assert_sequences_have_same_length(self, expected, got, mess):
        display_both_lists_mess = \
           "\nExpected list:\n   %s\nGot list:\n   %s" % (pformat(expected), pformat(got))
        
        if len(expected) != len(got):  
           self.fail(mess + "\nExpeted sequence of length %s, but got sequence of length %s" 
                     % (len(expected), len(got)) + \
                     display_both_lists_mess)
           
    def assert_dicts_have_same_keys(self, expected_dict, got_dict, mess):
       expected_keys = expected_dict.keys()
       expected_keys.sort()
       got_keys = got_dict.keys()
       got_keys.sort()
       self.assert_equal(expected_keys, got_keys, 
                                              "%s\nThe two dictionaries did not have the same keys" % mess)
   
    def find_first_diff_char(self, string1, string2): 
        debug.trace('TestCaseWithHelpers.find_first_diff_char', "string1='%s', string2='%s'" % (string1, string2))
        
        if string1 == None:
           string1 = ''
        if string2 == None:
           string2 = ''
        
        if (string1 == string2):
           return None
        if (len(string1) < len(string2)):
           upto = len(string1)-1;
        else:
           upto = len(string2)-1;
        if (upto < 0):
           # one of the strings was empty but not the other
           return -1

        debug.trace('TestCaseWithHelpers.find_first_diff_char', "upto='%s'" % upto);   
        for ii in range(upto+1):
            char1 = string1[ii]
            char2 = string2[ii]
            debug.trace('TestCaseWithHelpers.find_first_diff_char', 'ii=%s, char1=%s, char2=%s' % (ii, char1, char2))
            if char1 != char2:
               return ii;
        if (upto < len(string1)-1 or upto < len(string2)-1):
           return upto + 1;
        else:
           return undef;
   


    def assert_file_exists(self, fpath, mess):
        if not os.path.exists(fpath):
           self.fail(mess + "\nFile '%s' did not exist." % fpath)
        elif not os.path.isfile(fpath):
           self.fail(mess + "\nPath '%s' is not a file." % fpath)
           
    def assert_file_content_is(self, fpath, expected_content, message):
        self.assert_file_exists(fpath, message)       
        file = open(fpath, 'r')
        got_content = file.read()
        file.close()
        self.assert_equal(expected_content, got_content, message)
    
    def assert_file_contains_N_lines(self, fpath, exp_N_lines, mess):
        self.assert_file_exists(fpath, mess)
        file = open(fpath, 'r')
        content = file.read()
        file.close()
        got_N_lines = len(content.split("\n"))
        self.assert_equal(exp_N_lines, got_N_lines, mess + "\nNumber of lines in the file was not as expected.")
        
    def make_empty_dir_for_file(self, fpath):
        directory = self.makedirs_for_file(fpath)
        files = os.listdir(directory)
        for file in files:
            if os.path.isfile(file):
                os.remove(file)
            elif os.path.isdir(file):
                os.removedirs(file)
            else:
                pass
    
    def makedirs_for_file(self, fpath):
        head, tail = os.path.split(fpath)
        try:
            os.makedirs(head)
        except exceptions.Exception:
            # Presumably, directory already existed
            pass
        return head

######################################################################
# Some introspection methods needed by TestCaseWithHelpers.
# We put them here instead of in a separate class, in order to make
# TestCaseWithHelpers self-contained.
######################################################################

    def what_class(self, instance):
        """Returns a string describing the class of an instance.

        It works with any Python class or Python standard data types (int, float,
        string, etc.), but not with extension classes."""

        is_class = 'unknown'
        try:
             tmp = instance.__class__
             is_class = tmp
        except exceptions.AttributeError:
            #
            # The instance is not a python class. Maybe one of the
            # standard python data types?
            #
            is_class = type(instance)

        return is_class

    def isnumber(self, instance):
        return isinstance(instance, types.IntType) or isinstance(instance, types.FloatType)
    
    def issequence(self, instance):
        return isinstance(instance, types.ListType) or \
               isinstance(instance, types.TupleType)
    
    def isdictionary(self, instance):
        return isinstance(instance, types.DictionaryType)
    
    def isstring(self, instance):
        return isinstance(instance, types.StringType)
    
    def isbasetype(self, instance):
        return re.search("^\<type ", repr(self.what_class(instance)))
    
######################################################################
# Old deprecated names for some methods. Still supported for backward 
# compatibility.
######################################################################

    def assert_equals(self, expected, got, mess, epsilon=0):
        print "\nWARNING: Call to deprecated method TestCaseWithHelpers.assert_equals()\n"
        self.assert_equal(expected, got, mess, epsilon)

    def assert_string_equals(self, exp_string, got_string, message=''):
        print "\nWARNING: Call to deprecated method TestCaseWithHelpers.assert_string_equals\n"
        self.assert_equal_string(exp_string, got_string, message)
        
    def assert_dicts_have_same_content(self, expected_dict, got_dict, mess):
        print "\nWARNING: Call to deprecated method TestCaseWithHelpers.assert_dicts_have_same_content\n"
        self.assert_equals(expected_dict, got_dict, mess)

    def assert_sequences_have_same_content(self, expected, got, mess):
                print "\nWARNING: Call to deprecated method TestCaseWithHelpers.assert_sequences_have_same_content\n"
                self.assert_equal(expected, got, mess)
        
