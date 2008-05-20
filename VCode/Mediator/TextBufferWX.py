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
# (C)2000, David C. Fox
#
##############################################################################

"""interfaces for editor buffers with change notification"""


import debug
from Object import Object
from TextBuffer import *
from wxPython.wx import *

# workaround for minor bug in wxPython 2.3.2.1 (should be fixed in later
# versions, and the workaround will still work then)
def fix_x_y(value):
    if len(value) == 3:
        return value[1], value[2]
    return value

class TextBufferWX(TextBufferChangeSpecify, VisibleBuffer, StoreableTextBuffer,
        NumberedLines):
    """TextBufferChangeSpecify wrapper for wxTextCtrl
    
    **INSTANCE ATTRIBUTES**

    *wxTextCtrl* underlying -- underlying text control - a wxPython
    text control object

    *BOOL* program_initiated -- flag used internally to indicate to the
    whether the text changed event was due to a program-initiated change
    or to a user-initiated change.

    *BOOL* carriage_return_bug -- flag specifying whether the current
    version of wxPython requires a workaround for the carriage return
    bug.  (see comments below)

    *STR* crnl -- rep of CR-NL pair in underlying buffer
    *STR* nl -- rep of new line externally and in contents
    *INT* delta_width -- difference in lengths of crnl and nl
    *STR* contents_internal -- copy of contents of the buffer, with CR-LF
    *STR* contents_external -- copy of contents of the buffer, with only NL



    **CLASS ATTRIBUTES**
    
    *none* --
    """
    def __init__(self, underlying_control, carriage_return_bug = 1, **args):
        """wraps underlying wxPython wxTextCtrl

        **INPUTS**

        *wxTextCtrl* underlying_control -- underlying text control - a wxPython
        text control object
    
        **OUTPUTS**

        *none*
        """
        self.deep_construct(TextBufferWX,
                            {'underlying':underlying_control,
                            'program_initiated':0,
                            'contents_external': underlying_control.GetValue(),
                            'contents_internal': '',
                            'carriage_return_bug':carriage_return_bug,
                            'nl': '\n',
                            'crnl': '\015\n',
                            'delta_width': 0},
                            args)

        if not self.carriage_return_bug:
            self.crnl = self.nl
        self.delta_width = len(self.crnl) - len(self.nl)
        self.contents_internal = string.replace(self.contents_external,
            self.nl, self.crnl)
        parent = self.underlying.GetParent()
        ID = self.underlying.GetId()
#        EVT_TEXT(self.underlying, ID, self._on_evt_text)
        EVT_TEXT(parent, ID, self._on_evt_text)
      
    def _on_evt_text(self, event):
        """handler for wxEVT_COMMAND_TEXT_UPDATED.
        
        """
# program initiated calls originate from set_text, which handles
# updating self.contents_external and internal, and calling
# _on_change_specification
#        print '_on_evt_text', self.program_initiated
        if not self.program_initiated:
            contents = self.underlying.GetValue()
            start, end, text = \
                find_difference.find_string_difference(self.contents_external, 
                contents)
            self.contents_external = contents
            self.contents_internal = \
                string.replace(contents, self.nl, self.crnl)
            selection_start, selection_end = self.get_selection()
            self._on_change_specification(start, end, text,
                selection_start, selection_end, self.program_initiated)

    def modified(self):
        """has the buffer been modified since the last time it was
        saved?

        **INPUTS**

        *none*

        **OUTPUTS**

        *BOOL* -- true if the buffer has been modified since the last
        save (or load)
        """
        return self.underlying.IsModified()

    def save_file(self, f_path):
        """save the buffer to a file

        **INPUTS**

        *STR f_path* -- full path of the file

        **OUTPUTS**

        *BOOL* -- true if the file was saved successfully
        """
        return self.underlying.SaveFile(f_path)

    def load_file(self, f_path):
        """load the buffer from a file (erasing the current contents)

        **INPUTS**

        *STR f_path* -- full path of the file

        **OUTPUTS**

        *BOOL* -- true if the file was loaded successfully
        """
        success = self.underlying.LoadFile(f_path)
        return success

    def range_defaults(self, start = None, end = None):
        """translates from TextBuffer defaults for specifying start and
        end of a range to the appropriate values for wxTextCtrl (except
        that we use external offsets here)
        
        **INPUTS**
        
        *INT* start -- external offset of start of range, or None to
        default to the beginning of the buffer

        *INT* end -- external offset of character following end of 
        range, or None to default to the end of the buffer

        **OUTPUTS**

        *(INT, INT)* -- external offsets
        
        """

# note: this uses internal positions
        if (start == None):
          s = 0
        else:
          s = start
        if (end == None):
          e = len(self.contents_external)
        else:
          e = end
        return s, e

    def line_range_internal(self, start, end):
        """find line numbers of a range of internal positions within
        contents_internal

        **INPUTS**

        *INT* start -- character offset into contents_internal of start of range
        *INT* end  -- character offset into contents_internal of end of range

        **OUTPUTS**

        *(INT, INT)* -- corresponding range of line numbers
        """
        before = string.split(self.contents_internal[0:start], self.crnl)
#        print before
        first = len(before) - 1
        range = self.contents_internal[start:end]
        n = len(string.split(range, self.crnl)) -1
        second = first + n
#        print first, second
        return first, second

    
    def line_range_external(self, start, end):
        """find line numbers of a range of external positions within
        contents_external

        **INPUTS**

        *INT* start -- character offset into contents_external of start of range
        *INT* end  -- character offset into contents_external of end of range

        **OUTPUTS**

        *(INT, INT)* -- corresponding range of line numbers
        """
        first = len(string.split(self.contents_external[0:start], self.nl)) -1
        range = self.contents_external[start:end]
        n = len(string.split(range, self.nl)) -1
        second = first + n
        return first, second

    def external_to_internal(self, start, end):
        """converts a range of external positions (NL only) to
        internal positions (in the underlying
        buffer which uses CR-LF)

        **INPUTS**

        *INT* start -- start of range (external)
        *INT* end -- end of range (external)

        **OUTPUTS**

        *(INT, INT)* -- corresponding character range 
        internally, using CR-LF
        """
        lines = self.line_range_external(start, end)
        s = start +lines[0]*self.delta_width
        e = end +lines[1]*self.delta_width
        return s, e

    def internal_to_external(self, start, end):
        """converts a range of internal positions (in the underlying
        buffer which uses CR-LF) to external positions (NL only)

        **INPUTS**

        *INT* start -- start of range (internal)
        *INT* end -- end of range (internal)

        **OUTPUTS**

        *(INT, INT)* -- corresponding character range 
        externally, assuming only newlines
        """
        lines = self.line_range_internal(start, end)
        s = start -lines[0]*self.delta_width
        e = end -lines[1]*self.delta_width
        return s, e


    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer

        **INPUTS**

        *STR* text -- the new text.
        
        *INT* start -- the offset into the buffer of the text to the
        replaced.  Defaults to start of buffer.

        *INT* end -- the offset into the buffer of the character following 
        the text to be replaced (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *none* --
        """
# store initial value of flag
        program_initiated = self.program_initiated
        s, e = self.range_defaults(start, end)
        before = self.contents_external[0:s]
        after = self.contents_external[e:]
        st, en = self.external_to_internal(s, e)
        self.contents_external = before + text + after
#        print 'TBNwx.set_text ', s, e, text
        before_internal = self.contents_internal[0:st]
        after_internal = self.contents_internal[en:]
        t = string.replace(text, self.nl, self.crnl)
        self.contents_internal = before_internal + t + after_internal
        self.program_initiated = 1
#        self.set_selection(s, e)
        self.underlying.SetSelection(st, en)
        self.underlying.WriteText(text)
        self.make_position_visible()
        selection_start, selection_end = self.get_selection()
        self._on_change_specification(s, e, text, selection_start,
            selection_end, self.program_initiated)
# restore flag to initial value
        self.program_initiated = program_initiated
#        if self.carriage_return_bug:
#            s, e = self._internal_range(s, e)
#        self.program_initiated = 1
# this tries to use clipboard, for some unknown reason, and fails
#        self.underlying.Replace(s, e, text)

    def get_length(self):
        return len(self.contents_external)

    def get_text(self, start = None, end = None):
        """retrieves a portion of the buffer

        **INPUTS**

        *INT* start -- the start of the region returned.
        Defaults to start of buffer.

        *INT* end -- the offset into the buffer of the character following 
        the region to be returned (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *STR* -- contents of specified range of the buffer
        """
        s, e = self.range_defaults(start, end)
        return self.contents_external[s:e]

    def get_selection(self):
        """retrieves range of current selection

        **INPUTS**

        *none* --
        
        **OUTPUTS**

        *INT* (start, end) -- start is the offset into the buffer of 
        the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        s, e = self.underlying.GetSelection()
#        print s, e
        if self.carriage_return_bug:
            s, e = self.internal_to_external(s, e)
#        print 'external ',s,e
        return s, e

    def set_selection(self, start = None, end = None):
        """changes range of current selection

        **INPUTS**

        *INT* start -- the start of the region to be selected.
        Defaults to start of buffer.

        *INT* end -- the offset into the buffer of the character following 
        the region to be selected (this matches Python's slice convention).
        Defaults to end of buffer.

        **OUTPUTS**

        *none* --
        """
        # wxTextCtrl doesn't actually trigger a change notification (EVT_TEXT)
        # on selection changes, but just in case we switch to a
        # different underlying buffer which does,
        # we should set program_initiated before setting the
        # selection, and clear it afterwards
        program_initiated = self.program_initiated
        s, e = self.range_defaults(start, end)
#        print 'TBNwx.set_selection', s, e
        if self.carriage_return_bug:
            s, e = self.external_to_internal(s, e)
        self.program_initiated = 1
        self.underlying.SetSelection(s, e)
        self.program_initiated = program_initiated

    def cur_pos(self):
        """returns current position  (= end of the current selection)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the offset into the buffer of the current cursor
        position.
        """
        return self.get_selection()[1]

    def len(self):
        """returns length of buffer

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* -- the length of the buffer
        """
        return len(self.contents_internal)

    def line_height(self):
        """get number of lines per screen
        
        **INPUTS**
        
        *none*
        
        **OUTPUT**
        
        *INT* -- number of lines which fit on the screen at a time
        """
        width, height = self.underlying.GetClientSizeTuple()
        char_height = self.underlying.GetCharHeight()
        return height/char_height

    def get_visible(self):
        """ get start and end offsets of the currently visible region of
        the buffer.  End is the offset of the first character not
        visible (matching Python's slice convention)

        **INPUTS**

        *none* --

        **OUTPUTS**

        *INT* (start, end) -- visible range
        """
# check this
        screen  = self.line_height()
        starting_line = self.underlying.GetScrollPos(wxVERTICAL)
        start = self.underlying.XYToPosition(0, starting_line)
        ending_line = starting_line + screen - 1
        lines = self.underlying.GetNumberOfLines()
# if ending line of window goes beyond end of buffer, retrace steps
#        print 'visible'
        while self.underlying.XYToPosition(0, ending_line) == -1:
            ending_line = ending_line -1
#        print 'line: ', ending_line, lines - 1
        if ending_line + 1 <= lines - 1:
            end = self.underlying.XYToPosition(-1, ending_line+1)
        else:
            end = self.underlying.GetLastPosition()
#        print end
        ending_x, ending_y = fix_x_y(self.underlying.PositionToXY(end))
#        print starting_line, ending_line, ending_x
        if self.carriage_return_bug:
            start, end = self.internal_to_external(start, end)
#        print start, end
        return start, end

    def make_position_visible(self):
        s, e = self.get_visible()
        p = self.cur_pos()
        p_internal = self.external_to_internal(p, p)[0]
        if p > s and p < e:
            return
        if p > e:
            x, y = fix_x_y(self.underlying.PositionToXY(p_internal))
            y = y - self.line_height() + 1
            p_internal = self.underlying.XYToPosition(x, y)
        self.underlying.ShowPosition(p_internal)
        
    def line_num_of( self, pos = None):
        """find line number of position pos

        **INPUTS**

        *INT pos* -- the offset into the buffer of the desired position. 
         Defaults to the current position.

        **OUTPUTS**

        *INT* -- corresponding line number (starting with 0)
        """

        if pos == None:
            pos = self.cur_pos()
        lines = self.line_range_external(0, pos)
        return lines[1]
    
    def line_nums_of_range(self, range = None):
        """find line numbers of a range of positions

        **INPUTS**

        *(INT, INT) range* -- range of character offsets into the buffer. 
         Defaults to the current selection.

        **OUTPUTS**

        *(INT, INT)* -- corresponding pair of line numbers (starting with 0)
        """

        if range == None:
            range = self.get_selection()
        s, e = range
        lines = self.line_range_external(s, e)
        return lines
    

    def lines(self):
        """return number of lines in the buffer
        
        **INPUTS**
        
        *none*
        
        **OUTPUT**
        
        *int* -- number of lines in the buffer (incomplete lines are
        counted, so this is always > 0
        """
        return self.underlying.GetNumberOfLines()

    def position_of_line(self, line = None, where = -1):
        """returns the position of the start or end of the specified line 

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, returns position of end of buffer.

        *INT where* indicates whether the position of the end
         (*where > 0*) or at the beginning (*where < 0*) of the line
         should be returned.

        **OUTPUTS**

        *INT* -- position of start/end of that line.
        """

        end = None
        if line == None:
            end = self.cur_pos()
# we don't need lines past the desired line
        
        lines = string.split(self.get_text(0, end), self.nl)
        last = len(lines) - 1
        if line == None:
            line = last
        elif line > last:
            line = last
        before = string.join(lines[0:line], self.nl)
        position = len(before)
        if (line > 0):
            position = position + len(self.nl)
        if where > 0:
            position = position + self.line_length(line)
        return position

    def line_length(self, line = None):
        """returns the length of the specified line

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, returns None.

        **OUTPUTS**

        *INT* -- length of start of that line.
        """
        if line == None:
            line = self.line_num_of()
        elif line > self.underlying.GetNumberOfLines():
            return None
        return self.underlying.GetLineLength(line)

    def goto_line(self, line = None, where = -1):
        """Go to a particular line in a buffer.

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is greater than the number of lines, goes to the end of
        the buffer.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.

        **OUTPUTS**

        *none*
        """
        pos = self.position_of_line(line, where)
        self.set_selection(pos, pos)
    
    def range_of_line(self, line = None):
        """returns the character range corresponding to the specified line 
        (not including the newline)

        **INPUTS**

        *INT line* -- line number (starting with 0).  Defaults to current line.
        If line is out of range, last line is used.

        **OUTPUTS**

        *(INT, INT)* -- offsets into the buffer of the start and end of
        the line.
        """
        end = None
        if line == None:
            end = self.cur_pos()
# we don't need lines past the desired line
        
        lines = string.split(self.get_text(0, end), self.nl)
        last = len(lines) - 1
        if line == None:
            line = last
        elif line > last:
            line = last
        before = string.join(lines[0:line], self.nl)
        start = len(before)
        if line > 0:
            start = start + len(self.nl)
        end = start + self.line_length(line)
        return start, end
      
    def range_of_lines(self, first_line, last_line):
        """returns the character range corresponding to the specified range
        of lines (not including the final newline)

        **INPUTS**

        *INT first_line, second_line* -- line numbers (starting with 0)

        **OUTPUTS**

        *(INT, INT)* -- offsets into the buffer of the start and end of
        the range of lines.
        """
    
        lines = string.split(self.get_text(), self.nl)
        last = len(lines) - 1
        before = string.join(lines[0:first_line], self.nl)
        if last_line > last:
            last_line = last
        start = len(before)
        if first_line > 0:
            start = start + len(self.nl)
        between = string.join(lines[first_line:last_line])
        end = start + len(between) + self.line_length(last_line)
        if last_line > first_line:
            end = end + len(self.nl)
        return start, end
      
    def ranges_of_lines(self, first_line, last_line):
        """returns a list of the character ranges corresponding to the 
        specified range of lines (not including the final newline of
        each line)

        **INPUTS**

        *INT first_line, second_line* -- line numbers (starting with 0)

        **OUTPUTS**

        *[(INT, INT), ...]* -- offsets into the buffer of the start and end of
        the each line in the range of lines.
        """
        lines = string.split(self.get_text(), self.nl)
        last = len(lines) - 1
        before = string.join(lines[0:first_line], self.nl)
        ranges = []
        start = len(before)
        if first_line > 0:
            start = start + len(self.nl)
        if last_line > last:
            last_line = last 
        for line in range(first_line, last_line+1):
            end = start + self.line_length(line)
            ranges.append((start, end))
            start = end + len(self.nl)
        return ranges



