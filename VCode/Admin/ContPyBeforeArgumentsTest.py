import debug
import cont_gen
import VoiceCodeRootTest

class ContPyBeforeArgumentsTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """Test the BeforeArguments Context in python

    This context should apply if The cursor is just before the arguments of a function definition or a function call.
    So before the  the parens.

    def here(arg):
        pass

    or 

    x = here()

    But not here or f(here)

    This context can be used for integrating add arguments and with arguments
    QH march 2007

    """   
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
    def setUp(self):
        self._init_simulator_regression()
        self.context = cont_gen.ContPyBeforeArguments()

##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      
    def test_This_is_how_you_create_a_ContPyBeforeArguments(self):
        context = cont_gen.ContPyBeforeArguments()
        self.assert_(isinstance(context, cont_gen.ContPyBeforeArguments))
        
    def test_ContPyBeforeArguments_applies_if_cursor_is_just_before_open_paren(self):
        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = here()")
        self._say("select here")
        self._assert_active_buffer_content_is('outside = here<CURSOR>()')
        self.assert_(self.context.applies(self._app()), 
                     "Context should have applied because cursor is in front of open paren")

    def test_ContPyBeforeArguments_fails_if_cursor_is_not_just_before_open_paren(self):
        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = f()")
        self._say("select outside")
        self._assert_active_buffer_content_is('outside<CURSOR> = f()')
        self.failIf(self.context.applies(self._app()), 
                     "Context should not apply because cursor is not just before open paren")

    def test_ContPyBeforeArguments_applies_only_for_python(self):

        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select f")
        self.assert_(self.context.applies(self._app()), 
                     "Context should have applied because cursor was inside function arguments, in the correct language.")
        self._open_empty_test_file('temp.c')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select f")
        self.failIf(self.context.applies(self._app()), 
                     "Context should NOT have applied because the file was in the wrong language.")


##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################
    def test_Results_of_with_arguments(self):
        """testing the results in a python buffer"""

        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("""g = None
h = func(3, 4)
i = None'
def test(i=5):
j = i + 6
k = test(i=7)""")
        self._assert_active_buffer_content_is("""g = None
h = func(3, 4)
i = None'
def test(i=5):
    j = i + 6
    k = test(i=7)<CURSOR>""")
        self._say("select func")
        self._assert_active_buffer_content_is("""g = None
h = func<CURSOR>(3, 4)
i = None'
def test(i=5):
    j = i + 6
    k = test(i=7)""")
        self.assert_(self.context.applies(self._app()), 'func should apply, because before a function call')
        self._say("select four")
        self.failIf(self.context.applies(self._app()), '4 should not apply')
        self._say("select five")
        self.failIf(self.context.applies(self._app()), '5 should not apply')
        self._say("select test")
        self.assert_(self.context.applies(self._app()), 'at test should apply')
        self._say("select None")
        self._assert_active_buffer_content_is("""g = None
h = func(3, 4)
i = None<CURSOR>'
def test(i=5):
    j = i + 6
    k = test(i=7)""")
        self.failIf(self.context.applies(self._app()), 'at None should NOT apply')
        
        
    
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################
