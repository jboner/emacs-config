import debug
from CmdInterp import CmdInterp
from CmdInterp import LSAlias
from CmdInterp import AliasMeaning
import VoiceCodeRootTest
import vc_globals
import re
import os
import regression
from SpokenUtterance import MockSpokenUtterance

class CmdInterpTest(VoiceCodeRootTest.VoiceCodeRootTest):
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
   def setUp(self):
       self._init_simulator_regression()
       self.interp = self._command_interpreter()

##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################

   def __test_reminder(self):
       pass
       self.fail("Remember to reactivate all tests in CmdInterpTest")
      
   def test_this_is_how_you_create_a_CmdInterp(self):
       interp = CmdInterp()
       
   def test_this_is_how_you_add_a_known_symbol(self):
       self.interp.add_symbol('thiIsASymbol')
       

##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################


   def test_fuzzy_symbol_matches_should_not_consme_LSAs_or_CSCs(self):
       self.interp.add_symbol('function')
       
       # Note: "function open" is a valid fuzzy match for function,
       #       but it should not be allowed in this case
       self.assert_utterance_translates_to(
               ['function', 'open', 'paren'],
               'function(<CURSOR>',
               mess="Failed translation. Might it be that a fuzzy symbol match consumed part of a LSA.")
                      
   def test_symbol_containing_an_LSA(self):
       self.interp.add_symbol('MyVec_INT')
       
       self.assert_utterance_translates_to(['my', 'vec', 'int'],
               'MyVec_INT<CURSOR>', into_file='temp.c', 
               mess="Failed to correctly translate known symbol which "+
                    "contained the 'int' C keyword, even when spoken " +
                    "form was an exact match to the native symbol.")

       self.assert_utterance_translates_to(['my', 'vector', 'int'],
               'MyVec_INT<CURSOR>', into_file='temp.c', 
               mess="Failed to correctly translate known symbol which "+
                    "contained the 'int' C keyword, in case where spoken " +
                    "form was a fuzzy match to the native symbol.")


       self._open_empty_test_file('test.c')       
       utterance = MockSpokenUtterance(['my', 'vector', 'int'])
       self.interp.interpret_utterance(utterance, 
                                       self._app())
       self._assert_active_buffer_content_is('MyVec_INT<CURSOR>', 
               "Failed to correctly translate 'my vector int' to known symbol MyVec_INT. Possibly because of 'int' is a C keyword?.")
       

 
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################

   def assert_utterance_translates_to(self, words, 
                                      expected_translation, 
                                      into_file="test.py", mess=""):
       self._open_empty_test_file(into_file)
       utterance = MockSpokenUtterance(words)
       self.interp.interpret_utterance(utterance, 
                                       self._app())
       self._assert_active_buffer_content_is(expected_translation, 
               "Failed to correctly translate utterance %s." % words)
       
       
       
