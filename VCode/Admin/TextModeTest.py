import debug
import VoiceCodeRootTest
import time
import re

class TextModeTest(VoiceCodeRootTest.VoiceCodeRootTest):
   """Test dictation of normal text.
   """
   
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
   def setUp(self):
      self._init_simulator_regression(exclusive=0)
      
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      

   def test_toggle_text_mode(self):
       self._open_file(VoiceCodeRootTest.foreground_py)   
       self._say(['new', 'statement', 'above'])

       self._say(['text', 'mode', 'on'], never_bypass_sr_recog=1)
       self._say(['this', 'should', 'be', 'typed', 'as', 'normal', 'text'], 
                 never_bypass_sr_recog=1)
       time.sleep(1)
       self._app().process_pending_updates()
       time.sleep(1)
       self._app().process_pending_updates()
       self._assert_current_line_content_is(\
           "This should be typed as normal text<CURSOR>",
           "Text dictated with text mode on was wrong(case igored).",
           compare_as_regexp=True, regexp_flags=re.IGNORECASE)
                                            
       self._app().insert("\n")

       self._say(['with', 'arguments'], never_bypass_sr_recog=1) 
       time.sleep(1)
       self._app().process_pending_updates()
       time.sleep(1)
       self._app().process_pending_updates()
       self._assert_current_line_content_is(\
           "with arguments<CURSOR>",
           "CSC dictated while text mode on, should have been printed as normal text.(case ignored)",
           compare_as_regexp=True, regexp_flags=re.IGNORECASE)
       
       self._say(['text', 'mode', 'off'], never_bypass_sr_recog=1)
       self._say(['new', 'statement', 'this', 'should', 'be', 'typed', 
                  'as', 'a', 'variable', 'name'], 
                  never_bypass_sr_recog=1)
       time.sleep(1)
       self._app().process_pending_updates()
       time.sleep(1)
       self._app().process_pending_updates()
       self._assert_current_line_content_is("this_should_be_typed_as_a_variable_name<CURSOR>",
                                            "New variable name was not inserted properly after switching text mode off.")
       
       self._app().insert("\n")                                                                 
       self._say(['with', 'arguments'], never_bypass_sr_recog=1) 
       time.sleep(1)
       self._app().process_pending_updates()
       time.sleep(1)
       self._app().process_pending_updates()
       self._assert_current_line_content_is("(<CURSOR>)",
                                            "CSC dictated while text mode on, not interpreted correctly after switching text mode off.")
       
