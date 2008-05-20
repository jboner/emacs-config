import debug
import cont_gen
import VoiceCodeRootTest

class ContPyInsideArgumentsTest(VoiceCodeRootTest.VoiceCodeRootTest):
    """Test the InsideArguments Context in python

    This context should apply if The cursor is inside the arguments section of a function definition or a function call.  So between the parens.

    def f(here):
        pass

    or 

    x = f(here)

    This context can be used for formatting the "=" without spacing when inside. 
    (QH, dec 2006)

    """   
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
    def setUp(self):
        self._init_simulator_regression()
        self.context = cont_gen.ContPyInsideArguments()

##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      
    def test_This_is_how_you_create_a_ContPyInsideArguments(self):
        context = cont_gen.ContPyInsideArguments()
        self.assert_(isinstance(context, cont_gen.ContPyInsideArguments))
        
    def test_ContPyInsideArguments_applies_if_cursor_is_inside_function_arguments(self):
        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select inside")
        self._assert_active_buffer_content_is('outside = f(inside<CURSOR>)')
        self.assert_(self.context.applies(self._app()), 
                     "Context should have applied because cursor inside function arguments")

    def test_ContPyInsideArguments_fails_if_cursor_is_outside_function_arguments(self):
        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select outside")
        self._assert_active_buffer_content_is('outside<CURSOR> = f(inside)')
        self.failIf(self.context.applies(self._app()), 
                     "Context should not apply because cursor is outside function arguments")

    def test_ContPyInsideArguments_applies_only_for_python(self):

        self._open_empty_test_file('temp.py')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select inside")
        self.assert_(self.context.applies(self._app()), 
                     "Context should have applied because cursor was inside function arguments, in the correct language.")
        self._open_empty_test_file('temp.c')
        self._insert_in_active_buffer("outside = f(inside)")
        self._say("select inside")
        self.failIf(self.context.applies(self._app()), 
                     "Context should NOT have applied because the file was in the wrong language.")


##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################
    def test_Results_of_equalsign(self):
        """testing the results in a python buffer"""

        # nevermind:        
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
        self._say("select three")
        self._assert_active_buffer_content_is("""g = None
h = func(3<CURSOR>, 4)
i = None'
def test(i=5):
    j = i + 6
    k = test(i=7)""")
        self._say("select four")
        self.assert_(self.context.applies(self._app()), '4 should apply, because inside a function call')
        self._say("select five")
        self.assert_(self.context.applies(self._app()), '5 should apply, because inside a function call')
        self._say("select six")
        self.failIf(self.context.applies(self._app()), '6 should NOT apply, because inside a function call')
        self._say("select seven")
        self.assert_(self.context.applies(self._app()), '7 should apply, because inside a function call')
        self._say("select test")
        self.failIf(self.context.applies(self._app()), 'at test should NOT apply, because inside a function call')
        self._say("select none")
        self.failIf(self.context.applies(self._app()), 'at None should NOT apply, because inside a function call')
        
        
    
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################
