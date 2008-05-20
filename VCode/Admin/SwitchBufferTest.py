import debug
import VoiceCodeRootTest
import time

class SwitchBufferTest(VoiceCodeRootTest.VoiceCodeRootTest):
   """Test dictation of normal text.
   """
   
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
      
   def setUp(self):
      self._init_simulator_regression()
      self.sleep_for_a_while()
      
   def sleep_for_a_while(self):
       time.sleep(1)
      
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################
      
   def test_emacs_do_switch_buffer(self):
      self._open_file('dummy.py')
      self._say(['class', 'dummy'])
      self._open_file('test.py')
      # here are the differences QH
      # sleeping 5 times
      self.sleep_for_a_while()
      self.sleep_for_a_while()
      self.sleep_for_a_while()
      self.sleep_for_a_while()
      self.sleep_for_a_while()
            
      self._assert_active_buffer_is_called('test.py', "Emacs didn't start out in the right buffer")
      self._say(['yo', 'switch', 'to', 'buffer'])
      self.sleep_for_a_while()
      
      self._assert_active_buffer_is_called('*Completions*', 
               "Emacs didn't switch to the *Completion* buffer after switch buffer command was uttered")
      
      exp_buff_content = """Click <mouse-2> on a completion to select it.
In this buffer, type RET to select the completion near point.

Possible completions are:
<CURSOR>*Messages*			   *scratch*
dummy.py			   test.py"""      
      self._assert_active_buffer_content_is(exp_buff_content, 
                                            "Content of the buffer list was wrong.")
      
      self._say(['select', 'dummy', ], never_bypass_sr_recog = 1)
      self._say(['new', 'line'])    
      self.sleep_for_a_while()
      
      self._assert_active_buffer_is_called('dummy.py', 
               "Emacs didn't switch to dummy buffer after it was selected in the Completions buffer.")
               
      self._say(['yo', 'switch', 'to', 'buffer'])
      self.sleep_for_a_while()
            
      self._assert_active_buffer_is_called('*Completions*', 
               "Emacs didn't switch back to the Completion buffer after switch buffer command was uttered")

      self._say(['select', 'dummy', ], never_bypass_sr_recog = 1)      
      content_before_invalid_dictation = self._get_buffer_content_with_cursor_position()
      self._say(['hello'], never_bypass_sr_recog = 1)     
      self.sleep_for_a_while()
      
      self._assert_active_buffer_is_called('*Completions*', 
               "Emacs should not have switched from the *Completion* buffer when user tried to dictate into it.")
      self.assert_equal(content_before_invalid_dictation, 
              self._get_buffer_content_with_cursor_position(),
              "Content of *Completions* buffer should not have changed when user tried to dictate into it.")

      # 
      # We do a correct switch buffer to avoid leaving Emacs split in two windows,
      # which could change the result of some future tests (move relative page
      # in particular, which depend on the number of lines displayed in
      # the current Emacs window).
      #   
      self._say(['select', 'dummy', ], never_bypass_sr_recog = 1)
      self._say(['new', 'line'])    
      self.sleep_for_a_while()

