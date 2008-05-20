import debug
import VoiceCodeRootTest

class SourceBuffTest(VoiceCodeRootTest.VoiceCodeRootTest):
     
    def __init__(self, name):
        VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
    
    def setUp(self):
       self._init_simulator_regression(exclusive=0)
       self._open_empty_test_file('blah.py')
       self.source_buff = self._app().curr_buffer()
                                                         
    def tearDown(self):
        pass
        
##########################################################
# Documentation tests
#
# These tests illustrate how to use the class.
##########################################################


##########################################################
# Unit tests
#
# These tests check the internal workings of the class.
##########################################################
       
    def test_get_text_of_line(self):

       # setting up 4 lines:
       self.source_buff.insert("line1\nline2\nline3\nline4")
       self.source_buff.goto(0)
       self.source_buff.goto_line(0)
       got_text = self.source_buff.get_text_of_line(3)
       self.assert_equal("line3", got_text, 
                         "Got wrong content of line 3.")
    
    
       self.source_buff.goto_line(2)
       got_text = self.source_buff.get_text_of_line()
       self.assert_equal("line2", got_text, 
                         "Got wrong content of current line when current line was line no 2.")
                         
       got_text = self.source_buff.get_text_of_line(1)
       self.assert_equal("line1", got_text, 
                         "Got wrong content of line 1")
                         
       got_text = self.source_buff.get_text_of_line(4)
       self.assert_equal("line4", got_text, 
                         "Got wrong content of last line")
                         
       got_text = self.source_buff.get_text_of_line(999)
       self.assert_equal("line4", got_text, 
                         "Asking for content of line beyond length of buffer should have returned content of last line.")
                         
       got_text = self.source_buff.get_text_of_line(-1)
       self.assert_equal("line1", got_text, 
                         "Asking for content of line before first line, should return content of first line.")



    def test_get_text_of_previous_lines(self):

       # setting up 4 lines:
       self.source_buff.insert("line1\nline2\nline3\nline4")
       self.source_buff.goto_line(3)

       current_pos = self.source_buff.cur_pos()
       num = self.source_buff.line_num_of(current_pos)
       self.assert_equal(3, num, "Got wrong line number")
       got_text = self.source_buff.get_text_of_line(num)
       self.assert_equal("line3", got_text, 
                         "Got wrong content of line num.")

       Lines = []
       for i in range(num, 0, -1):
           Lines.insert(0, self.source_buff.get_text_of_line(i))
       got_text = '\n'.join(Lines)
       self.assert_equal(got_text, "line1\nline2\nline3",
                         "Got wrong content of lines 1,2,3")

       check_current_pos = self.source_buff.cur_pos()
       self.assert_equal(current_pos, check_current_pos,
                         "Current position should not have changed")
       

      
###############################################################
# Assertions.
# 
# Use these methods to check the state of the class.
###############################################################
