##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

"""State information for the programming environment."""


import debug
import re, string, sys
import SourceBuff
import AppTracker
import sb_services, SourceBuffNonCached


from Object import Object
import SourceBuffState

class SourceBuffTB(SourceBuff.BackspaceMixIn, 
        SourceBuffNonCached.SourceBuffNonCached):
    """implementation of (most of) SourceBuff as a wrapper around an
    object which inherits multiple interfaces: TextBuffer, VisibleBuffer, 
    and NumberedLines.

    **INSTANCE ATTRIBUTES**

    *TextBuffer, VisibleBuffer, NumberedLines underlying* -- underlying
    TextBuffer (also supporting VisibleBuffer and NumberedLines) 

    *STR f_name* -- name of the corresponding file, if different from
    the buffer name
  
    [SB_ServiceLang] *lang_srv* -- Language service used to know the
    programming language of a source file.

    [SB_ServiceLineManip] *line_srv* -- Service for manipulating lines.

    [SB_ServiceIndent] *indent_srv* -- Code indentation service.

    CLASS ATTRIBUTES**
    
    """
    
    def __init__(self, underlying_buffer, change_specification = 0, 
        f_name = None, **attrs):
        """
        Create the SourceBuffTB object

        **INPUTS**

        *TextBuffer, VisibleBuffer, NumberedLines underlying_buffer* -- 
        underlying TextBuffer (also supporting VisibleBuffer and NumberedLines) 

        *BOOL change_specification* -- tells whether the underlying
        buffer also supports change specification
        """

        self.init_attrs({'lang_srv': sb_services.SB_ServiceLangServerSide(buff=self),
                         'indent_srv': sb_services.SB_ServiceIndent(buff=self, indent_level=3, indent_to_curr_level = 1),
                         'line_srv': sb_services.SB_ServiceLineManip(buff=self),
                        })
        self.deep_construct(SourceBuffTB,
                            {'underlying': underlying_buffer,
                             'f_name': f_name},
                            attrs
                            )
        self.add_owned_list(['indent_srv', 'line_srv',
            'lang_srv'])
        if change_specification:
            self.underlying.set_change_callback(self.on_underlying_change)

    def uses_server_side_indent(self):
       return 1

    def on_underlying_change(self, start, end, text, selection_start,
        selection_end, buffer, program_initiated):
        """method called by the underlying buffer to signal a change

        **INPUTS**

        *INT* start -- start of the changed region
        
        *INT* end -- end of the changed region
        
        *STR* text -- new text for the changed region

        *INT* selection_start -- start of selection after change

        *INT* selection_end -- end of selection after change

        *TextBufferChangeSpecify* buffer -- underlying buffer
        
        *BOOL* program_initiated -- true if the change was initiated by
        a program call to the buffer's methods

        **OUTPUTS**

        *none*
        """
# program-initiated changes will already call on_change, so we don't
# want to report them twice
        if program_initiated:
            return
        self.on_change(start, end, text, program_initiated)

    def name_file(self, new_file_name):
        """changes the filename

        **INPUTS**

        *STR new_file_name* -- the new name

        **OUTPUTS**

        *none*
        """
        self.f_name = new_file_name

    def file_name(self):
        """Returns the name of the file being displayed in a buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *STR* -- the name of the file
        """        
        if self.f_name != None:
            return self.f_name
        return self.buff_name

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *STR* -- the name of the language
        """
        return self.lang_srv.language_name()    

    def get_pos_selection(self):
        """retrieves current position of cursor and the range of 
        current selection
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *(INT, (INT, INT))* (pos, (start, end))
        
        pos is the offset into buffer of current cursor position
        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        return (self.underlying.cur_pos(), self.underlying.get_selection())

    def set_selection(self, range, cursor_at = 1):
        """sets range of current selection, and sets the position to 
        beginning or end of the selection.

        **INPUTS**

        *(INT, INT)* range -- offsets into buffer of the start and end
        of the selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).

        *INT* cursor_at -- indicates whether the cursor should be
        placed at the left (0) or right (1) end of the selection.  Note:
        cursor_at is ignored unless the application supports this
        choice, as indicated by bidirectional_selection.  
        Most Windows applications do not.

        **OUTPUTS**

        *none*
        """
        self.underlying.set_selection(range[0], range[1])

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer

        **INPUTS**

        *INT start* is the start of the region returned.
        Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the region to be returned (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *STR* -- contents of specified range of the buffer
        """
        return self.underlying.get_text(start, end)
      
    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer.  Note: this is a low level
        interface.  Usually, higher level interfaces like insert and
        delete are preferable.

        **INPUTS**

        *STR text* is the new text.
        
        *INT start* is the offset into the buffer of the text to the
        replaced.  Defaults to start of buffer.

        *INT end* is the offset into the buffer of the character following 
        the text to be replaced (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *none*
        """
        start, end = self.make_valid_range((start, end))
        self.underlying.set_text(text, start, end)
        self.on_change(start, end, text, 1)

    def get_visible(self):
        """ get start and end offsets of the currently visible region of
        the buffer.  End is the offset of the first character not
        visible (matching Python's slice convention)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        return self.underlying.get_visible()

    def make_position_visible(self):
        """scroll buffer (if necessary) so that the current position
        is visible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        self.underlying.make_position_visible()
    
    def line_num_of(self, position = None):
        """
        Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.  (defaults to the current position)
        
        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        
        return self.underlying.line_num_of(position)
      
    def len(self):
        """return length of buffer in characters.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* length 
        """
        return self.underlying.len()

    def beginning_of_line(self, pos):
        """Returns the position of the beginning of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the beginning of line.
        

        **OUTPUTS**
        
        *INT* beg_pos -- Position of the beginning of the line
        """
        return self.line_srv.beginning_of_line(pos)


    def end_of_line(self, pos):
        """Returns the position of the end of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the end of line.
        

        **OUTPUTS**
        
        *INT* end_pos -- Position of the end of the line
        """
        return self.line_srv.end_of_line(pos)


    def move_relative_page(self, direction=1, num=1):
        """Moves up or down a certain number of pages
        
        **INPUTS**
        
        *INT* direction=1 -- If positive, page down. If negative, page up.
        
        *INT* num=1 -- Number of pages to move.
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        range = self.underlying.get_visible()
        first, last = self.underlying.line_nums_of_range(range)
        height = last -first + 1
        if direction < 0:
            num = - num
        current = self.underlying.line_num_of()
        self.underlying.goto_line(current + num)
       
    def insert(self, text, range = None):
        """Replace text in range with 
        with text

        **INPUTS**

        *STR text* -- new text

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """

        if range == None:
            start, end = self.get_selection()
        else:
            start, end = self.make_valid_range(range)
        self.underlying.set_text(text, start, end)
        self.on_change(start, end, text, 1)
        return AppTracker.TextBlock(text, start)

    def insert_indent(self, code_bef, code_after, range = None):
        """Insert code into source buffer and indent it.

        Replace code in range 
        with the concatenation of
        code *STR code_bef* and *str code_after*. Cursor is put right
        after code *STR bef*.

        **INPUTS**

        *STR* code_bef -- code to be inserted before new cursor location
        
        *STR* code_bef -- code to be inserted after new cursor location

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """
        
        return self.indent_srv.insert_indent(code_bef, code_after, range)

    def indent(self, range = None):
        """Indent code in a source buffer region.

        **INPUTS**

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """

        self.indent_srv.indent(range)

    def incr_indent_level(self, levels=1, range=None):
        
        """Increase the indentation of a region of code by a certain
        number of levels.
        
        **INPUTS**
        
        *INT* levels=1 -- Number of levels to indent by.
        
        *(INT, INT)* range=None -- Region of code to be indented 
        

        **OUTPUTS**
        
        *none* -- 
        """

        self.indent_srv.incr_indent_level(levels, range)

    def decr_indent_level(self, levels=1, range=None):

        """Decrease the indentation of a region of code by a certain number
        of levels.
        
        **INPUTS**hello
        
        *STR* levels=1 -- Number of levels to unindent

        *(INT, INT)* range=None -- Start and end position of code to be indent.
        If *None*, use current selection

        **OUTPUTS**
        
        *none* -- 
        """

        self.indent_srv.decr_indent_level(levels, range)


    def delete(self, range = None):
        """Delete text in a source buffer range.

        **INPUTS**

        *(INT, INT)* range -- code range to be deleted.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """
        if range == None:
            start, end = self.get_selection()
        else:
            start, end = self.make_valid_range(range)
        self.underlying.set_text('', start, end)
        self.on_change(start, end, '', 1)
        
    def goto(self, pos):

        """Moves the cursor to position *INT pos* of source buffer
        (and make selection empty)
        """

        self.underlying.set_selection(pos, pos)

    def goto_line(self, linenum, where=-1):
        """Go to a particular line in a buffer.

        *INT linenum* is the line number.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.
        """
        self.underlying.goto_line(linenum, where)

    def refresh(self):
        """Force a refresh of the buffer"""
        self.underlying.refresh()

    def newline_conventions(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        return ['\n']


    def pref_newline_convention(self):
        """Returns the form of newline that the editor prefers for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        return '\n'

