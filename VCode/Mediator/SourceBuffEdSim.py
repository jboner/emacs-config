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


import re, string, sys

from Object import Object
import SourceBuff
import AppTracker
import debug, find_difference, sb_services, SourceBuffNonCached, util
from debug import trace


class SourceBuffEdSim(SourceBuff.BackspaceMixIn, 
    SourceBuffNonCached.SourceBuffNonCached):
    """concrete class representing a disconnected source buffer for the
    editor simulator EdSim.

    **INSTANCE ATTRIBUTES**
    
    *INT pos=0* -- Cursor position (in number of chars) in the buffer
    *(INT, INT)* selection_range -- the current selection as
      character offsets into contents.
    *STR content=None* -- Content of the source buffer
    *BOOL global_selection* -- makes the 'visible' region the whole
    buffer (useful for regression testing)
  
    *BOOL* instance_reporting -- flag which turns on diagnostic reporting 
    to check on proper allocation/de-allocation

    [SB_ServiceLang] *lang_srv* -- Language service used to know the
    programming language of a source file.

    [SB_ServiceLineManip] *line_srv* -- Service for manipulating lines.

    [SB_ServiceIndent] *indent_srv* -- Code indentation service.

    CLASS ATTRIBUTES**

    *none*

    ..[SB_ServiceLang] file:///./sb_services.SB_ServiceLang.html
    ..[SB_ServiceLineManip] file:///./sb_services.SB_ServiceLineManip.html"""
    
    def __init__(self, indent_level=3, indent_to_curr_level=1, init_pos=0,
                 init_selection = None, initial_contents="",
                 global_selection = 1, instance_reporting = 0,
                 **attrs):
        
        self.init_attrs({'lang_srv': sb_services.SB_ServiceLangServerSide(buff=self),
                         'line_srv': sb_services.SB_ServiceLineManip(buff=self),
                         'indent_srv': sb_services.SB_ServiceIndent(buff=self, indent_level=3, indent_to_curr_level = 1)
                         })

        self.deep_construct(SourceBuffEdSim,
                            {'pos': init_pos, 
                             'selection': init_selection, 
                             'content': initial_contents, 
                             'global_selection': global_selection,
                             'indent_level': indent_level,
                             'indent_to_curr_level': indent_to_curr_level,
                             'instance_reporting': instance_reporting }, 
                            attrs
                            )

        self.add_owned_list(['indent_srv', 'line_srv', 'lang_srv'])

        if self.instance_reporting:
            print 'SourceBuff.__init__:', util.within_VCode(self.name())
        self.pos = self.make_within_range(self.pos)
        if not self.selection:
            self.selection = (self.pos, self.pos)
        s, e = self.get_selection()
        if (s < e):
            self.selection = (self.pos, self.pos)
            

    def __del__(self):
        "destructor"
        if self.instance_reporting:
            print 'SourceBuff.__del__:', util.within_VCode(self.name())

    def rename_buffer_cbk(self, new_buff_name):
        
        """AppState invokes this method when 
        AppState.rename_buffer_cbk is called to notify VoiceCode that 
        an existing text buffer has been renamed
        
        **INPUTS**

        STR *new_buff_name* -- new name of the buffer.
        
        **OUTPUTS**
        
        *none*
        
        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

        if self.instance_reporting:
            print 'SourceBuff.rename_buffer_cbk:', self.name(), new_buff_name
        self.SourceBuffNonCached.rename_buffer_cbk(new_buff_name)
      
    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# function, after performing their own duties
        if self.instance_reporting:
            print 'SourceBuff.remove_other_references:', util.within_VCode(self.name())
        SourceBuffNonCached.SourceBuffNonCached.remove_other_references(self)

    def file_name(self):
        return self.name()

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *STR* -- the name of the language
        """

        return self.lang_srv.language_name()

    def get_pos_selection(self):
        if self.selection == None:
            self.selection = (self.cur_pos(), self.cur_pos())
        return (self.pos, self.selection)

    def set_selection(self, range, cursor_at = 1):
        start, end = self.make_valid_range(range)
        self.selection = (start, end)
        self.pos = end

    def get_text(self, start = None, end = None):
        if start == None:
            start = 0
        if end == None:
            end = self.len()
        start, end = self.make_valid_range((start, end))
        return self.content[ start: end]
    
    def set_text(self, text, start = None, end = None):
        if start == None:
            start = 0
        if end == None:
            end = self.len()
        start, end = self.make_valid_range((start, end))
        before = self.content[0:start]
        after = self.content[end:]
        self.content = before + text + after
        self.goto(start + len(text))
        self.on_change(start, end, text, 1)

    def get_visible(self):
        if self.global_selection:
            return (0, self.len())
        top, bottom = self.lines_around_cursor()
        lines = string.split(self.contents(), '\n')
        s = 0
        for line in lines[0: top -1]:
            s = s + len(line) + 1
        e = s
        for line in lines[top: bottom -1]:
            e = e + len(line) + 1
        e = e + len(lines[bottom])
        return s, e

    def make_position_visible(self):
        pass

    def line_num_of(self, position = None):
        """Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.
        

        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        return self.line_srv.line_num_of(position)


    def len(self):
        return len(self.content)

    def beginning_of_line(self, pos = None):
        """Returns the position of the beginning of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the beginning of line.
        

        **OUTPUTS**
        
        *INT* beg_pos -- Position of the beginning of the line
        """
        return self.line_srv.beginning_of_line(pos)


    def end_of_line(self, pos = None):
        """Returns the position of the end of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the end of line.
        

        **OUTPUTS**
        
        *INT* end_pos -- Position of the end of the line
        """
        return self.line_srv.end_of_line(pos)

    def print_buff_if_necessary(self):
        """Always print content of buffer after changes, even if we are not
        using EdSim for regression testing."""
        self.print_buff()

    def refresh(self):
        self.print_buff()
        
    def move_relative_page(self, direction=1, num=1):
        """Moves up or down a certain number of pages
        
        **INPUTS**
        
        *INT* direction=1 -- If positive, page down. If negative, page up.
        
        *INT* num=1 -- Number of pages to move.
        

        **OUTPUTS**
        
        *none* -- 
        """

        num_lines = 2*self.print_nlines + 1
        self.move_relative_line(direction=direction, num=num_lines)


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
            range = self.get_selection()
        range = self.make_valid_range(range)
        start, end = range
        before = self.get_text(0,start)
        after = self.get_text(end)
        self.content = before + text + after
        self.goto(start + len(text))
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
        
        
    def uses_server_side_indent(self):
       return 1

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
        
        **INPUTS**
        
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
            range = self.get_selection()
        range = self.make_valid_range(range)
        start, end = range
        before = self.content[0:start]
        after = self.content[end:]
        self.content = before + after
        self.goto(start)
        self.on_change(start, end, "", 1)

        
    def goto(self, pos):

        """Moves the cursor to position *INT pos* of source buffer
        (and make selection empty) 
        """
        
        pos = self.make_within_range(pos)
        self.pos = pos
        self.selection = (pos, pos)



    def goto_line(self, linenum, where=-1):
        """Go to a particular line in a buffer.

        *INT linenum* is the line number.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.
        """

        self.line_srv.goto_line(linenum, where)

    def newline_conventions(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        return ['\n', '\r\n']


    def pref_newline_convention(self):
        """Returns the form of newline that the editor prefers for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        return '\n'
