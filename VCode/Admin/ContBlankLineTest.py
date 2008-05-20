import debug
import cont_gen
import Context
import VoiceCodeRootTest

class ContBlankLineTest(VoiceCodeRootTest.VoiceCodeRootTest):
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
   def setUp(self):
       self._init_simulator_regression()
       self.context = cont_gen.ContBlankLine()

##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################

      
   def test_This_is_how_you_create_a_ContBlankLine(self):
       context = cont_gen.ContBlankLine()
       self.assert_(isinstance(context, cont_gen.ContBlankLine))
       self.assert_(isinstance(context, Context.Context))
       # next one should be false:
       self.failIf(isinstance(context, cont_gen.ContPyInsideArguments))
       
   def test_ContBlankLine_only_applies_if_cursor_is_on_a_blank_line(self):
       self._open_empty_test_file('temp.py')
       self.assert_(self.context.applies(self._app()), 
                    "Context should have applied because cursor it was on a blank line")
       self._insert_in_active_buffer("now line is not blank anymore")
       self.failIf(self.context.applies(self._app()), 
                    "Context should NOT have applied because cursor was NOT on a blank line")
       
   def test_You_can_also_make_a_ContBlankLine_which_only_applies_for_a_particular_language(self):
       self.context = cont_gen.ContBlankLine("python")
       
       self._open_empty_test_file('temp.py')
       self.assert_(self.context.applies(self._app()), 
                    "Context should have applied because cursor was on a blank line of the correct language.")
       
       self._open_empty_test_file('temp.c')
       self.failIf(self.context.applies(self._app()), 
                    "Context should NOT have applied because eventhough it was on a blank line, the file was in the wrong language.")


##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################


       
 
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################
