import debug
import VoiceCodeRootTest
import vc_globals
import os
import regression

class NavigationWithinBufferTest(VoiceCodeRootTest.VoiceCodeRootTest):
   def __init__(self, name):
      VoiceCodeRootTest.VoiceCodeRootTest.__init__(self, name)
          
   def test_nevermind(self):
 
# Un-comment next line if you want to actually see how the current
# page changes in the active buffer.
#      self._set_automatic_buffer_printing(1)
   
      self._init_simulator_regression()
      self._open_file(self._get_test_data_file_path('large_buff_py'))
      
      orig_linenum = self._get_top_visible_line_num()
      self._say(['yo', 'page', 'down'])
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, 1, 
              "Page down command failed", )

      orig_linenum = new_linenum
      self._say(['do', 'that', 'again'])   
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, 1, 
              "Failed to repeat previous page down command", )

      orig_linenum = new_linenum
      self._say(['again', 'two\\two', 'times']) 
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, 2, 
              "Failed to repeat previous page down command twice", )

      orig_linenum = new_linenum      
      self._say(['yo', 'page', 'up'])
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, -1, 
              "Page up command failed.", )

      orig_linenum = new_linenum      
      self._say(['do', 'that', 'again'])   
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, -1, 
              "Repeating page up command failed.", )

      orig_linenum = new_linenum      
      self._say(['again', 'two\\two', 'times']) 
      new_linenum = self._get_top_visible_line_num()
      self._assert_moved_by_n_pages(orig_linenum, new_linenum, -2, 
              "Repeating page up command twice failed.", )
    
      self._goto_line(11)
      self._say(['go', 'to', 'end', 'of', 'line'])
      self._assert_cursor_looking_at("\n", 1, 
             "Cursor should have moved to end of line.")
      
      self._say(['go', 'to', 'beginning', 'of', 'line'])
      self._assert_cursor_looking_at("\n", -1, 
             "Cursor should have moved to end of line.")

      self._say(['yo', 'top', 'of', 'file'])
      self._assert_cur_pos_is(0, "Should have moved to top of file.")
      
      self._say(['yo', 'bottom', 'of', 'file'])
      self._assert_cur_pos_is(self._len()-1, "Should have moved to bottom of file.")
      
   def _sign(self, number):
      if number == 0:
         return 0
      else:
         return abs(number)/number
      
   def _get_top_visible_line_num(self):
      (visible_start_pos, visible_end_pos) = self._app().get_visible()
      top_line = self._app().line_num_of(visible_start_pos)
      return top_line
      
   def _assert_moved_by_n_pages(self, orig_line, new_line, n, message, ):
      diff = new_line - orig_line
      # 
      # Set this to a value that is smaller than
      # the number of lines displayed by most editors
      #
      approx_lines_per_page = 40
      
      min_lines_displacement = int(abs(n)*approx_lines_per_page/2)
      expected_direction = self._sign(n)
      got_direction = self._sign(diff)
      
      # NOTE: This test is not completely accurate, but it will work
      #       with most windows sizes. To get an accurate verification
      #       we would have to query the editor to know how many lines
      #       it displays per page, which would require writing a new
      #       message. 
      message = message + "\nDid not move by the appropriate number of lines."
      self.assert_(abs(diff) > min_lines_displacement, 
                   message + 
                   "\nExpected to have moved by at least %s lines, but moved by %d" % (min_lines_displacement, abs(diff)))
      self.assert_equal(
              expected_direction, got_direction, 
              message + 
              "\nExpected to move in direction %s, but moved in direction %s" % (expected_direction, got_direction)
              )
                   