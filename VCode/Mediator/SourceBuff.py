#############################################################################
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

"""Interface to a buffer being edited in the programming environment"""


import debug
from debug import trace
import re, string, sys

from exceptions import IndexError

from Object import Object, OwnerObject

from SourceBuffCookie import SourceBuffCookie

import AppTracker

import sb_mixins

class SourceBuff(OwnerObject):
    """Interface to a buffer being edited in the programming environment
   
    This abstract class defines interface for manipulating buffer containing
    source code in some programming language.
    
    **INSTANCE ATTRIBUTES**
    
    *STR buff_name=None* -- Unique name for the buffer.
    
    *AppState app* -- application object containing the buffer

    (STR, INT, INT, INT) *last_search* -- Remember the details of the
    last search or selection that was done with method *search_for* or
    with *Select Pseudocode*. This is so that if the user repeats the
    same search or selection, we don't end up returning the same
    occurence over and over.

    The first 3 entries of the tuple correspond to the value of
    *regexp*, *direction*,
    and *where*. The last entry correspond to the position where
    cursor was put after last search.

    INT *print_nlines* -- When printing content of buffer to STDOUT
    (*print_buff* methods), print this number of lines before and
    after current line.
    

    CLASS ATTRIBUTES**
    
    """
    
    def __init__(self, app, buff_name=None, **attrs):
        self.init_attrs({'last_search': None})
        self.deep_construct(SourceBuff,
                            {'app': app,
                             'buff_name': buff_name,
                             'print_nlines': 3},
                            attrs
                            )                            
        self.name_parent('app')  

    def name(self):
        """returns the name of the buffer
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        STR -- name of the buffer."""
        return self.buff_name
        
    def on_change(self, start, end, text, program_initiated):
        """method which should be called after the contents of a buffer
        is changed.  If the SourceBuff represents a buffer in an 
        external editor which does not support change notification, then 
        on_change may only be called for mediator-initiated changes 
        (including responses from the external editor to 
        mediator-initiated changes).

        **INPUTS**

        *INT* start -- start of the modified range

        *INT* end -- end of the modified range.

        If both start and end are None, this is a buffer contents
        update (which may or may not reflect an actual change)

        *STR* text -- the new text replacing this range

        *BOOL* program_initiated -- true if the change was initiated by
        the mediator

        **OUTPUTS**

        *none*
        """
        if program_initiated:
            debug.trace('SourceBuff.on_change', 
                '(%d, %d) "%s" in %s\n' % (start, end, text, self.name()))
        self.app.on_change(self.name(), start, end, text, program_initiated)

    def rename_buffer_cbk(self, new_buff_name):
        """AppState invokes this method when 
        AppState.rename_buffer_cbk is called to notify VoiceCode that 
        an existing text buffer has been renamed
        
        **INPUTS**

        STR *new_buff_name* -- new name of the buffer.
        
        **OUTPUTS**
        
        *none*
        
        ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

        debug.trace('SourceBuff.rename_buffer_cbk', 'new_buff_name=%s' % new_buff_name)
        self.buff_name = new_buff_name

    def file_name(self):
        """Returns the name of the file being displayed in a buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        STR *name* -- 
        """        
        debug.virtual('SourceBuff.file_name')

    def to_sync(self, item, what, exclude):
        """Determines if an item is to be synchronised
        
        **INPUTS**
        
        STR *item* -- Type of item to be synchronised. Valid entries
        are: 'buff_name', 'content', 'cur_pos', 'selection'

        [STR] *what=[]* -- List of items to be included in or excluded
        from the list of things to synchronize (depending on value of
        *exclude*. Valid entries are: 'buff_name', 'content',
        'cur_pos', 'selection'.  *exclude_what=1*, this should be
        interpreted as a list of items that don't need to be
        synchronised. If *exclude_what=0*, then it should be
        interpreted as a list of items that need to be syncrhonized.

        BOOL *exclude* -- If *true*, then *what* is a list of items to
        be excluded from syncrhonization. Otherwise, *what* is a list
        of itmes to be included in the synchronization.

        **OUTPUTS**
        
        *none* -- 
        """
        
        in_list = (item in what)
        if exclude:
            answer = not in_list
        else:
            answer = in_list
        return in_list


    def application(self):
        """returns the AppState object of the application
        containing this buffer.

        **INPUTS**        

        *none*

        **OUTPUTS**

        *AppState* -- application object containing the buffer
        """
        return self.app

    def drop_breadcrumb(self, pos=None):

        """Drops a breadcrumb -- see AppState.drop_breadcrumb

        NOTE: the breadcrumb stack is maintained at the AppState level,
        where both the position and the buffer are stored. 
        There are no separate buffer-by-buffer stacks.  Therefore, it
        would make no sense to define SourceBuff.pop_breadcrumb.
        SourceBuff.drop_breadcrumb is included only as a convenient
        shorthand for

          buff.application().drop_breadcrumb(buff_name = buff.file_name(),
          pos = ...)
        
        *INT pos* is the position where to drop the crumb. *STR
         buff_name* is the name of the source buffer.
        
        If *pos* not specified, drop breadcrumb at cursor position.
        """
        self.application.drop_breadcrumb(buff_name = self.file_name(), pos = pos)


    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        debug.virtual('SourceBuff.language_name')


    def is_language(self, lang):
        """Check if a source buffer is in a particular language.

        Outputs *true* if and only if *self* is displaying a file
        written in programming language *STR lang*.
        """
        return (self.language_name() == lang)


    def region_distance(self, region1_start, region1_end, region2_start, region2_end):
        """Computes the distance between two regions of text
        
        **INPUTS**
        
        *INT* region1_start -- start position of first region
        
        *INT* region1_end -- end position of first region
        
        *INT* region2_start -- start position of 2nd region
        
        *INT* region2_end -- end position of 2nd region
        

        **OUTPUTS**
        
        *INT* distance -- distance between the two regions of text
        """

        distance = min(abs(region1_start - region2_start), abs(region1_start - region2_end), abs(region1_end - region2_start), abs(region1_end - region2_end))
        return distance

    def cur_pos(self):
        """retrieves current position of cursor .  Note: the current
        position should coincide with either the start or end of the
        selection.  

        **INPUTS**

        *none*
        
        **OUTPUTS**

        *INT* pos -- offset into buffer of current cursor position
        """
        ps = self.get_pos_selection()
        debug.trace('SourceBuff.cur_pos', 'pos, selection = %s' % repr(ps))
        pos = ps[0]
        debug.trace('SourceBuff.cur_pos', 'pos = %s' % repr(pos))
        return pos

    def get_selection(self):
        """retrieves range of current selection
        
        Note: the current position should coincide with either the 
        start or end of the selection. 
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *INT* (start, end)
        
        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        ps = self.get_pos_selection()
        debug.trace('SourceBuff.get_selection', 'pos, selection = %s' % repr(ps))
        selection = ps[1]
        debug.trace('SourceBuff.get_selection', 'selection = %s' % repr(selection))
        return selection

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
        debug.virtual('SourceBuff.get_pos_selection')


    def bidirectional_selection(self):
        """does editor support selections with cursor at left?

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *BOOL* -- true if editor allows setting the selection at the
        left end of the selection"""
        return self.app.bidirectional_selection()

    def goto_end_of_selection(self, end = 1):
        """moves cursor to one end of the selection, clearing the
        selection.

        **INPUTS**

        *INT* end -- left (0) or right (1) end of selection

        **OUTPUT**

        *none*
        """
        target = self.get_selection()[end]
        self.goto(target)

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
        debug.virtual('SourceBuff.set_selection')

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
        debug.virtual('SourceBuff.get_text')
        
    def get_text_of_line(self, line_num=None):
        old_pos = self.cur_pos()
        if line_num != None:
           self.goto_line(line_num)
        start = self.beginning_of_line()
        end = self.end_of_line()
        self.goto(old_pos)
        return self.get_text(start, end)

    def set_text(self, text, start = None, end = None):
        """changes a portion of the buffer

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
        debug.virtual('SourceBuff.set_text')
        

    def contents(self):
        """retrieves entire contents of the buffer
    
        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* contents 
        """
        return self.get_text()

    def distance_to_selection(self, start, *opt_end):
        """Computes the distance of a region to the current selection.
        
        **INPUTS**
        
        *INT* start -- start position of region
        
        *[INT]* *opt_end -- end position of region (optional)
        

        **OUTPUTS**
        
        *INT* -- the distance
        """
        if len(opt_end) > 0:
            end = opt_end[0]
        else:
            end = start
        # make sure start < end and start2 < end2
        if start > end:
            tmp = end
            end = start
            start = tmp            
        start2, end2 = self.get_selection()
        # make sure start2 < end2
        if start2 > end2:
            tmp = end2
            start2 = end2
            end2 = tmp        
        if start2 == None or end2 == None:
            start2 = self.cur_pos()
            end2 = start2
        return self.region_distance(start, end, start2, end2)
        
    def get_visible(self):
        """ get start and end offsets of the currently visible region of
        the buffer.  End is the offset of the first character not
        visible (matching Python's slice convention)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        debug.virtual('SourceBuff.get_visible')

    def make_position_visible(self):
        """scroll buffer (if necessary) so that the current position
        is visible

        **INPUTS**

        **OUTPUTS**

        *none*
        """
        debug.virtual('SourceBuff.make_position_visible')
    
    def line_num_of(self, position = None):
        """
        Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.  (defaults to the current position)
        
        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        
        debug.virtual('SourceBuff.line_num_of')


    def number_lines(self, astring, startnum=1):
        """Assign numbers to lines in a string.

        Used mainly for the purpose of doing a printout of the buffer
        content around the cursor (usually during regression testing).

        *STR astring* is the string in question.

        *INT startnum* is the number of the first line in *astring*
        
        Returns a list of pairs *[(INT, STR)]* where first entry is
        the line number and the second entry is the line."""

        #
        # Note: need to split using regexp self.buff.newline_regexp()
        #       but for now this will do.
        #

        regexp = re.compile(self.newline_regexp())
        lines = regexp.split(astring)
        result = []

        if (astring != ''):
            lineno = startnum                
            for aline in lines:
                result[len(result):] = [(lineno, aline)]
                lineno = lineno + 1
            
        return result



    def len(self):
        """return length of buffer in characters.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* length 
        """
        debug.virtual('SourceBuff.len')

    def make_valid_range(self, range):
        """Makes sure a region is increasing and within the buffer's range.
        
        **INPUTS** 
        
        *(INT, INT)* range -- offsets of initial range
        
        **OUTPUTS**
        
        *(INT, INT)* -- increasing range within bounds
        """

#        print '-- SourceBuff.make_valid_range: range=%s' % repr(range)
        
        start, end = range        
        if start == None: start = 0
# DCF: ugh, another one of these errors:
#          the end is after index self.len() -1 so at index self.len()
#        if end == None: end = self.len() - 1
        if end == None: end = self.len()
        if end < start:
            tmp = end
            end = start
            start = tmp
        start = self.make_within_range(start)
        end = self.make_within_range(end)
        return start, end
  
    def make_within_range(self, position):
        """Makes sure a position is within the buffer's range.
        
        **INPUTS**
        
        *INT* position -- The position. If outside of bounds, bring it back
        to the first or last position of the buffer.
        

        **OUTPUTS**
        
        *INT* position -- The possibly corrected position
        """

        length = self.len()
        if position < 0:
            position = 0
# cursor can be after last character in the buffer
        elif position > length and length > 0:
            position = length
        return position


    def beginning_of_line(self, pos = None):
        """Returns the position of the beginning of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the beginning of
        line, or none for the current position.
        

        **OUTPUTS**
        
        *INT* beg_pos -- Position of the beginning of the line
        """
        debug.virtual('SourceBuff.beginning_of_line')


    def end_of_line(self, pos = None):
        """Returns the position of the end of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the end of
        line, or none for the current position.
        

        **OUTPUTS**
        
        *INT* end_pos -- Position of the end of the line
        """
        debug.virtual('SourceBuff.end_of_line')
        
        
    def move_relative(self, rel_movement):
        """Move cursor to plus or minus a certain number of characters
        
        **INPUTS** 
        
        *INT rel_movement* -- number of characters to move, relative to 
        current position.  If < 0 then move to the left. Otherwise, move to the
        right.

        **OUTPUTS**

        *none*
        """
        pos = self.cur_pos()+rel_movement
        self.goto(pos)

    def move_relative_line(self, direction=1, num=1):
        """Moves up or down a certain number of lines
        
        **INPUTS**
        
        *INT* direction=1 -- If positive, line down. If negative, line up.
        
        *INT* num=1 -- Number of pages to move.
        

        **OUTPUTS**
        
        *none* -- 
        """

        #
        # Note: if moving downwards, put cursor after \n. Otherwise, put it
        # before.
        #
        self.search_for(regexp='(^|$|\n)', direction=direction,
                        where=direction, num=num)


    def move_relative_page(self, direction=1, num=1):
        """Moves up or down a certain number of pages
        
        **INPUTS**
        
        *INT* direction=1 -- If positive, page down. If negative, page up.
        
        *INT* num=1 -- Number of pages to move.
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('SourceBuff.move_relative_page')


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

        debug.virtual('SourceBuff.insert')
        

###################################################################
# Methods for code syntax navigation
###################################################################

    def syntax_nav_supported(self):
        """
        Indicates whether this SourceBuff instance supports syntax
        navigation (either in the mediator or through the external
        editor

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return 0

    def find_matching(self, direction = 1):
        """
        Finds a matching brace/bracket/parenthesis.

        NOTE: this method does not find matching quotes, or any other
        character where the opening and closing characters are
        identical

        ** INPUTS **

        *INT direction* -- direction of the search.  Direction = 1
        means to search forward for the character matching the one at
        the current cursor position.  Direction = minus 1 means to
        search backward for the character matching the one before the
        current cursor position.

        ** OUTPUTS **

        *INT* -- the position of the matching character, or if the
        character adjacent to the cursor was not a bracket, brace, or
        parenthesis, or if no matching character could be found
        """
        debug.virtual('SourceBuff.find_matching')

    def beginning_of_statement(self):
        """Finds the location of the beginning of the current
        statement

        NOTE: initially, this method maybe implemented using the
        external editor, said the exact definition of the current
        statement and where it starts may vary.

        ** INPUTS **

        *none*

        ** OUTPUTS **

        *INT* -- the position of the beginning of the statement found
        at the cursor position
        """
        debug.virtual('SourceBuff.beginning_of_statement')
        
    

###################################################################
# Methods for automatic indentation.
# Eventually make those abstract methods, and create methods in
# SourceBuffWithServices that forward them to the indentation service
###################################################################


    def uses_server_side_indent(self):
       """Returns TRUE iif automatic indentation is done on the server side
       using and instance of SB_ServiceIndent"""
       
       return 0
       
    def insert_indent(self, code_bef, code_after, range = None):
        """Insert code into source buffer and indent it.

        Replace code in range 
        with the concatenation of
        code *STR code_bef* and *str code_after*. Cursor is put right
        after code *STR bef*.

        **INPUTS**

        *STR* code_bef -- code to be inserted before new cursor location
        
        *STR* code_after -- code to be inserted after new cursor location

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *(before, after)* -- a tuple of AppTracker.TextBlock objects,
        representing the text inserted before and after the cursor,
        including any surrounding whitespace which was inserted,
        and the final range occupied by the text inserted
        (which is normally not the same as the range replaced).  If
        either code_bef or code_after could not be matched to the
        changes reported by the editor (or if one
        of the two was empty), then the corresponding element of the
        return tuple will be None.
        """

         
        #
        # Tabs in the code to insert are only useful when using
        # server-side indentation. This version of insert_indent()
        # is meant for client-side indentation, so remove them
        #
        code_bef = re.sub('\t', '', code_bef)        
        code_after = re.sub('\t', '', code_after)      


        trace('SourceBuff.insert_indent',
              '... code_bef=%s, code_after=%s, range=%s' % (code_bef, code_after, range))

        if range == None:
            trace('SourceBuff.insert_indent', "  no range")
            range = self.get_selection()
            trace('SourceBuff.insert_indent',
                'got selection ... range=%s' % repr(range))
        range = self.make_valid_range(range)          

        indent_from = range[0]
        self.insert(code_bef, range)
    
        self.indent((indent_from, self.cur_pos()))        
        final_cur_pos = self.cur_pos()
        inserted_text = None
        if code_bef:
            text = self.get_text(indent_from, final_cur_pos)
            inserted_text = AppTracker.TextBlock(text, indent_from)
        appended_text = None
        if code_after != '':
            self.insert(code_after, (self.cur_pos(), self.cur_pos()))
            end_pos = self.cur_pos()
            self.indent((final_cur_pos, self.cur_pos()))
            self.goto(final_cur_pos)
            text = self.get_text(final_cur_pos, end_pos)
            appended_text = AppTracker.TextBlock(text, final_cur_pos)
        return inserted_text, appended_text

       

    def indent(self, range = None):
        
        """Automatically indent the code in a source buffer region. Indentation
        of each line is determined automatically based on the line's context.

        **INPUTS**

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """

        debug.virtual('SourceBuff.indent')


    def incr_indent_level(self, levels=1, range=None):
        
        """Increase the indentation of a region of code by a certain
        number of levels.
        
        **INPUTS**
        
        *INT* levels=1 -- Number of levels to indent by.
        
        *(INT, INT)* range=None -- Region of code to be indented 
        

        **OUTPUTS**
        
        *none* -- 
        """

        debug.virtual('SourceBuff.incr_indent_level')

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

        debug.virtual('SourceBuff.decr_indent_level')

###################################################################

    def delete(self, range = None):
        """Delete text in a source buffer range.

        **INPUTS**

        *(INT, INT)* range -- code range to be deleted.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """
        debug.virtual('SourceBuff.delete')
        
    def delete_buffer_content(self):
        self.delete((0, self.len()))

    def delete_line(self):
        """Delete line at cursor.

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        debug.trace('SourceBuff.delete_line', "self.beginning_of_line()=%s, self.end_of_line()=%s" %
                    (self.beginning_of_line(), self.end_of_line()))
        self.delete((self.beginning_of_line(), self.end_of_line()+1))
        
    def select_line(self):
        self.set_selection((self.beginning_of_line(), self.end_of_line()+1))
        
    def backspace(self, n_times):
        """Delete a number of spaces before the cursor.

        If possible, the editor should simulate backspace keys, so as to
        ensure that the effect of this command is identical to manual
        backspacing.

        However, if this is not possible, it can simulate backspacing as
        closely as possible, or use BackspaceMixIn to do server-side
        backspacing, or perform the equivalent algorithm on the client
        side.
        
        **INPUTS**
        
        INT *n_times* -- number of characters to delete.
        
        **OUTPUTS**
        
        *none*
        """
        debug.virtual('SourceBuff.backspace')
                

    def copy_selection(self):
        """Copy the selected text"""
        debug.virtual('SourceBuff.copy_selection')     
        
    def cut_selection(self):
        """Cut the selected text"""
        debug.virtual('SourceBuff.cut_selection')
           
    def paste(self):
        """Paste content of clipboard into buffer"""
        debug.virtual('SourceBuff.paste')
        
    def goto(self, pos):
        """Moves the cursor to position *INT pos* of source buffer
        (and make selection empty)
        """
        
        debug.virtual('SourceBuff.goto')

    def goto_line(self, linenum, where=-1):
        """Go to a particular line in a buffer.

        *INT linenum* is the line number.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.
        """
        debug.virtual('SourceBuff.goto_line')
        
    def goto_end_of_line(self, pos=None):
        """Go to end of the line at a particular cursor position.

        *INT pos* -- cursor position of the line we want to go to. If
        *None*, then use cur_pos().
        """
        self.goto(self.end_of_line(pos))
# what's wrong with the implementation above, which takes advantage of
# any editor-specific optimizations for finding the end of the line (and
# doesn't do a regex search on the entire contents of the file)?
#
#        if pos != None:
#           self.goto(pos)
#        self.search_for(regexp="($|\n)", direction=1,
#                        where=-1, ignore_left_of_cursor=1, unlogged=1)

    def goto_beginning_of_line(self, pos=None):
        """Go to beginning of the line at a particular cursor position.

        *INT pos* -- cursor position of the line we want to go to. If
        *None*, then use cur_pos().
        """
        self.goto(self.beginning_of_line(pos))
# what's wrong with the implementation above, which takes advantage of
# any editor-specific optimizations for finding the end of the line (and
# doesn't do a regex search on the entire contents of the file)?
#
#        if pos != None:
#           self.goto(pos)
#        
#        self.search_for(regexp="(^|\n)", direction=-1,
#                        where=1, ignore_right_of_cursor=1, unlogged=1)

    def goto_range(self, range, where):
        """Goes to one extremity of a range
        
        (INT, INT) *range* -- range to go to
        
        INT *where* -- if > 0, go to the end of *range*, otherwise to 
        the start"""
        
        if where > 0:
            pos = range[1]
        else:
            pos = range[0]
        self.goto(pos)
        
                
    def print_buff_if_necessary(self):
        """Prints content of current buffer if necessary.

        This method serves two purposes:

        - When regression testing an external editor, it is used to display
        the content of the buffer of the external editor whenever it has
        changed

        - When using the EdSim line-based editor, it refreshes the printout
        of the current content after each change."""

        debug.trace('SourceBuff.print_buff_if_necessary', 'self.app.print_buff_when_changed=%s' % self.app.print_buff_when_changed)
        if self.app.print_buff_when_changed:
            self.print_buff()
        

    def refresh(self):
        """Force a refresh of the buffer"""
        debug.virtual('SourceBuff.refresh')

    def _state_cookie_class(self):
        """returns the class object for the type of cookie used by
        store_current_state.

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- class of state cookies corresponding to this
        SourceBuff

        """
        debug.virtual('SourceBuff._state_cookie_class')
        
    def store_current_state(self):
        """stores the current state of the buffer, including both the
        contents and the current selection, for subsequent restoration.
        Store_current_state returns a "cookie" which can be passed to
        restore_state or compare_with_current.  The type and attributes
        of the cookie will depend on the specific subclass of
        SourceBuff.  In the most straightforward implementation, it 
        may include a copy of the entire contents of the
        buffer and the selection.  In other cases, particularly when the
        editor or SourceBuff provides an internal undo stack, it may simply be a
        reference to a point in this stack.
        
        Important Notes:
        
        You should only pass the cookie to methods of
        the SAME SourceBuff object from which it came.  Generally,
        cookies can not be pickled and retrieved.

        The type of cookie will vary with the concrete subclass 
        of SourceBuff.  The corresponding class object is 
        returned by _state_cookie_class.  However, external callers
        should not depend on the type, attributes, or methods 
        of the cookie.

        This method does not synchronize with the editor prior to
        storing the state.  The caller is responsible for synchronizing 
        if desired.  (This avoids having duplicate synchronize calls 
        when storing the current state of more than one buffer).

        **INPUTS**

        *none*

        **OUTPUTS**

        *SourceBuffCookie* -- state cookie (see above).  Note that
        SourceBuffCookie is a dummy class.  The
        actual return type will vary with SourceBuff subclass.
        """
        debug.virtual('SourceBuff.store_current_state')

    def restore_state(self, cookie):
        """restores the buffer to its state at the time when
        the cookie was returned by store_current_state.  Both the
        contents and the selection will be restored.  However, other
        data, such as the search history, may not.  The restore
        operation can fail, which will be indicated by a return value of
        0, so the caller should always check the return value.
        
        **INPUTS**

        *SourceBuffCookie cookie* -- see above.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if restore was successful

        """
        debug.virtual('SourceBuff.restore_state')

    def compare_states(self, first_cookie, second_cookie, selection = 0):
        """compares the buffer states at the times when
        two cookies were returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of either cookie
        """
        debug.virtual('SourceBuff.compare_states')

    def compare_state_selections(self, first_cookie, second_cookie):
        """compares the selection and cursor positions at the times when
        two cookies were returned by store_current_state.
        If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or it cannot be determined due to expiration of 
        either cookie
        """
        debug.virtual('SourceBuff.compare_state_selections')

    def compare_with_current(self, cookie, selection = 0):
        """compares the current buffer state to its state at the time when
        the cookie was returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if state is the same, false if it is not, or
        it cannot be determined due to expiration of the cookie
        """
        debug.virtual('SourceBuff.compare_with_current')

    def compare_selection_with_current(self, cookie):
        """compares the current buffer position and selection to these
        values at the time when the cookie was returned by 
        store_current_state.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or if it cannot be determined due to 
        expiration of the cookie
        """
        debug.virtual('SourceBuff.compare_selection_with_current')


    def get_state_pos_selection(self, cookie):
        """retrieves the position and selection from a given state
        cookie.  

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *(INT, (INT, INT))* -- position and selection at the time the
        cookie was created by store_current_state, or None if the cookie
        is invalid (usually because the state corresponding to the cookie 
        has been lost).
        """
        debug.virtual('SourceBuff.get_state_pos_selection')

    def valid_cookie(self, cookie):
        """checks whether a state cookie is valid or expired.
        If the state corresponding to the cookie has
        been lost, valid_cookie will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if cookie is valid (i.e. restore_state should be
        able to work)
        """
        debug.virtual('SourceBuff.valid_cookie')


    def newline_conventions(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('SourceBuff.newline_conventions')



    def pref_newline_convention(self):
        """Returns the form of newline that the editor prefers for this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        debug.virtual('SourceBuff.pref_newline_convention')


    def newline_regexp(self):
        
        """Returns a regexp that matches all the forms of newline that
        the editor can recognise in this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        #
        # We use (?: form so that the order of groups on the original
        # regexp is not screwed up.
        #
        regexp = '(?:' + string.join(self.newline_conventions(), '|')
        regexp = regexp + ')'

        return regexp


    def char_search(self, char_exp, direction = 1, pos = None):
        """performs a quick search for the next/previous character
        matching char_exp, without moving the cursor, or logging the
        search

        **INPUTS**

        *STR* char_exp -- a regular expression assumed to match one
        character (e.g. "a", ".", "\s", "\S", "[A-Za-z]")

        *INT* direction -- if positive, search forward, otherwise
        search backward

        *INT* pos -- position within buffer to start, or None for
        current position

        **OUTPUTS**

        *INT* -- offset into the buffer of the next character in the
        appropriate direction matching the expression.  If a matching
        character was not found, char_search will raise an IndexError.
        """
        if pos is None:
            pos = self.cur_pos()
        if direction > 0:
            d = 1
            bound = self.len()
        else:
            d = -1
            pos = pos - 1
            bound = 0
        x = re.compile(char_exp)        
        while d*pos <= bound:
            if x.match(self.contents()[pos]):
                return pos
            pos = pos + d
        raise IndexError()

    def looking_at(self, regexp, pos = None):
       r = re.compile(regexp)
       if pos is None:
           pos = self.cur_pos()
       return r.match(self.get_text(), pos)

    def lookback_search(self, regexp, num=1, where=1, unlogged = 0):
        """A simplified search which steps back through the file one
        character at a time, and checks whether the regexp matches at
        that character position.  When a match is found, the cursor is
        moved to that position.  This may be faster than a general
        reverse regexp search when you are far from the beginning of the
        buffer.  Note that the match location may not be the same as
        search_for.  For example, a regexp of ' *x' would match the
        first 'x' before the cursor, effectively ignoring the ' *'.
        Also, expressions containing newlines may not match reliably if
        regexp_newline has both single and multi-character alternatives.

        **INPUTS**

        *STR regexp* -- simple regular expression, starting with a fixed
        string

        *INT* num -- number of occurences to search for

        *INT* where -- if positive, move cursor after the occurence,
        otherwise move it before
           
        *BOOL* unlogged -- if true, don't log the results of this
        search (used for searches done by mediator without user-initiation)

        **OUTPUTS**

        *(INT, INT)* -- returns the range corresponding to the match,
        or None if none was found
        """

        success = None

        #
        # Change the \n in the regexp so they match any of the forms
        # recognised by the editor in this buffer
        #
        regexp = re.sub('\n', self.newline_regexp(), regexp)

        #
        # Find position of all matches
        #

        text = self.get_text()
        best_match = None
        l = self.len()
        
        pos = self.cur_pos() - 1
        a_match = self.looking_at(regexp, pos)
        while pos >= 0:
            a_match = self.looking_at(regexp, pos)
            if a_match:
                num = num - 1
                if num == 0:
                    best_match = a_match.span()
# even if this is the same as the previous search, allow it unless we
# find a better one next time around
                    if not self.same_as_previous_search(regexp, -1,
                          where, a_match.span()):
                        break
                    num = 1
            pos = pos - 1

        if best_match:
            new_cur_pos = self.pos_extremity(best_match, where)
            self.goto(new_cur_pos)

        if not unlogged:
            self.log_search(regexp, direction, where, best_match)

        return best_match

    def search_for(self, regexp, direction=1, num=1, where=1,
                   include_current_line = 0, unlogged = 0):
        
        """Moves cursor to the next occurence of regular expression
           *STR regexp* in buffer.

           *INT* direction -- if positive, search forward, otherwise
            search backward

           *INT* num -- number of occurences to search for

           *INT* where -- if positive, move cursor after the occurence,
           otherwise move it before

           *BOOL* include_current_line -- if true, include the entire
           current line in the search (start at end of line if going
           backwards, start at beginning of line if going forwards)
           
           *BOOL* unlogged -- if true, don't log the results of this
           search (used for searches done by mediator without user-initiation)

           **OUTPUTS**
           
           *BOOL* -- true if a match was found
           """

        trace('SourceBuff.search_for', 
              "regexp='%s', direction=%s, num=%s, where=%s, include_current_line=%s, unlogged=%s, self.cur_pos()=%s" %
               (regexp, direction, num, where, include_current_line, unlogged, self.cur_pos()))               

        # save old cursor position in case we go to beginning or end
        # of line and search fails so we need to return 
        old_cur_pos = self.cur_pos()

        if include_current_line:
            if direction > 0:
                # searching forward, so start at beginning of line
                self.goto(self.beginning_of_line())
            else:
                # searching backward, so start at end of line
                self.goto(self.end_of_line())
                       
        best_match = self.search_for_match(regexp, direction =
            direction, num = num, where = where)

        if not unlogged:
            self.log_search(regexp, direction, where, best_match)
                   
        if best_match is None:
            self.goto(old_cur_pos)
            trace('SourceBuff.search_for','... not found.')
            return 0

        new_cur_pos = self.pos_extremity(best_match, where)
        self.goto(new_cur_pos)
        trace('SourceBuff.search_for','... found.')
        return 1    

    def search_for_match(self, regexp, direction=1, num=1, where=1):
        
        """Finds the next occurence of regular expression
           *STR regexp* in buffer, and returns the range corresponding
           to the match

           *INT* direction -- if positive, search forward, otherwise
            search backward

           *INT* num -- number of occurences to search for

           *INT* where -- if positive, move cursor after the occurence,
           otherwise move it before
           
           **OUTPUTS**
           
           *(INT, INT)* -- returns the range corresponding to the match,
           or None if none was found
           """

        success = None

        #
        # Change the \n in the regexp so they match any of the forms
        # recognised by the editor in this buffer
        #
        regexp = re.sub('\n', self.newline_regexp(), regexp)

        
        #
        # Find position of all matches
        #
        reobject = re.compile(regexp)

        text = self.get_text()
        best_match = None
        l = self.len()
        
        if direction > 0:
            trace('SourceBuff.search_for_match', 'searching forward')
            pos = self.cur_pos()
            trace('SourceBuff.search_for_match', 'Searching text "'+text[pos:]+'"')
            count = 0
            while pos < l:
                trace('SourceBuff.search_for_match', 
                   'searching from pos %d' % pos)
                a_match = reobject.search(text, pos)
                if not a_match:
                    trace('SourceBuff.search_for_match', 
                        'no more matches after cuor')
                    break
                count = count + 1
                if count >= num:
                    trace('SourceBuff.search_for_match', 'found the one')
# even if this is the same as the previous search, allow it unless we
# find a better one
                    best_match = a_match.span()
                    if not self.same_as_previous_search(regexp, direction,
                          where, a_match.span()):
                        trace('SourceBuff.search_for_match', 'not same')
                        break
                    trace('SourceBuff.search_for_match', 'same')
                trace('SourceBuff.search_for_match', 
                   'found match from %d to %d' % a_match.span())
#DCF: no overlapping matches
                pos = a_match.end()
#DCF:  ... but if we get a zero length match (e.g. '$' matching end of line), 
# then we will continue to get the same match unless we advance the position
                if a_match.start() == a_match.end():
                    trace('SourceBuff.search_for_match', 
                        'zero length match, better increment position anyway')
                    pos = pos + 1

        else:
            pos = 0
            matches = []
            trace('SourceBuff.search_for_match', 'searching backwards')
            # SN: changed to go only up to cur_pos, so we will stop if our final
            # match is just up to cur_pos (see comment on matches through
            # cur_pos below)
            # while pos < l:
            while pos < self.cur_pos():
                trace('SourceBuff.search_for_match', 
                    'searching from pos %d' % pos)
                trace('SourceBuff.search_for_match', 'Searching text "'+text[pos:]+'"')
                # SN: only search through cur_pos() -- this prevents a
                # final match which goes *through* cur_pos from being
                # found and blocking a match up to cur_pos.
                a_match = reobject.search(text[:self.cur_pos()], pos)
                if not a_match:
                          trace('SourceBuff.search_for_match', 
                                'no more matches for /'+regexp+'/ before cursor')
                          break                            
                matches.append(a_match.span())
                trace('SourceBuff.search_for_match', 
                   'found match from %d to %d' % a_match.span())
#DCF: no overlapping matches
                pos = a_match.end()
#DCF:  ... but if we get a zero length match (e.g. '$' matching end of line), 
# then we will continue to get the same match unless we advance the position
                if a_match.start() == a_match.end():
                    trace('SourceBuff.search_for_match', 
                        'zero length match, better increment position anyway')
                    pos = pos + 1
            if num <= len(matches):
                a_match = matches[-num]
                if not self.same_as_previous_search(regexp, direction,
                      where, a_match):
                    trace('SourceBuff.search_for_match', 'not same')
                    best_match = a_match
                elif num < len(matches):
                    best_match = matches[-(num + 1)]

        return best_match

    def closest_occurence_to_cursor(self, occurences, 
                                    ignore_overlapping_with_cursor=0,
                                    ignore_left_of_cursor=0,
                                    ignore_right_of_cursor=0,
                                    direction=None, 
                                    regexp=None, where=1
                                    ):
        
        """Determines which occurence of a search pattern (or a
        *Select Pseudocode* pattern) is closest to the current cursor
        location.

        If the closest occurence is the one that was previously found for the
        same search or *Select Pseudocode* operation, take next closest one.

        **INPUTS**

        *(INT, INT)* occurences -- List of occurences (start and end positions).
        Assumed that they are sorted in increasing order of their start
        position.

        *INT* direction -- If negative, only consider occurences that are before
        the cursor. If positive, only consider occurences that are past the
        cursor. If *None*, consider all occurences whether before or after cursor.

        *BOOL* ignore_overlapping_with_cursor-- If true, then ignore any 
        occurence that overlaps with the cursor.
        
        *BOOL* ignore_left_of_cursor-- If true, then ignore any 
        occurence that is directly left of the cursor.
           
        *BOOL* ignore_right_of_cursor-- If true, then ignore any 
        occurence that is directly right of the cursor.           
        

        *STR* regexp -- The regular expression used to generate the
         list of occurences.
         

        **OUTPUTS**
        
        *INT* closest_index -- Index in *occurences* of the closest
         occurence. If no such occurence, returns *None*"""
         
        trace('SourceBuff.closest_occurence_to_cursor', 
              'occurences=%s, direction=%s, regexp="%s", where=%s' % 
              (repr(occurences), direction, regexp, where))
                      
        closest_index = None
        
        #
        # Look in the list of occurences for the one closest to the cursor
        # in the right direction
        #
        shortest_distance = None
        for ii in range(len(occurences)):
        
            debug.trace('SourceBuff.closest_occurence_to_cursor', 
                        'ii=%s, cur_pos()=%s, occurences[ii]=%s' % (ii, self.cur_pos(), occurences[ii]))

            if self.ignore_occurence(occurences[ii], ignore_overlapping_with_cursor,
                                    ignore_left_of_cursor,
                                    ignore_right_of_cursor):
               debug.trace('SourceBuff.closest_occurence_to_cursor', 
                           'skipping occurences[ii]=%s' % repr(occurences[ii]))                                                    
               continue

            if direction == None:
                #
                # Don't care if closest occurence is before or after cursor
                #
                distance = self.region_distance(occurences[ii][0], occurences[ii][1], self.cur_pos(), self.cur_pos())
                if ((shortest_distance == None or distance < shortest_distance)
                    and not self.same_as_previous_search(regexp, direction,
                                                         where, occurences[ii])):
                    shortest_distance = distance
                    closest_index = ii
            elif direction < 0:
                #
                # Looking for closest occurence before cursor ...
                #
                if occurences[ii][0] > self.cur_pos():
                    #
                    # We have passed cursor.
                    #
                    break
                else:
                    #
                    # We haven't passed the cursor. So this is
                    # closest occurence before cursor yet.
                    #
                    if not self.same_as_previous_search(regexp, direction,
                                                   where, occurences[ii]):
                        closest_index = ii
            else:
                #
                # Looking for closest occurence after cursor ...
                #                
                if occurences[ii][1] >= self.cur_pos():
                    #
                    # ... and we have just passed cursor. So this
                    # is the closest occurence after cursor
                    #
                    if not self.same_as_previous_search(regexp, direction,
                                                   where, occurences[ii]):
                        closest_index = ii
                        break

        return closest_index

    def ignore_occurence(self, occurence, ignore_overlapping_with_cursor,
                         ignore_left_of_cursor, ignore_right_of_cursor):

        if (ignore_overlapping_with_cursor and 
            occurence[0] <= self.cur_pos() and
            occurence[1] >= self.cur_pos()):
            return 1
                        
        if (ignore_left_of_cursor and 
            occurence[1] == self.cur_pos()):
            return 1

        if (ignore_right_of_cursor and 
            occurence[0] == self.cur_pos()):
            return 1

        return 0

    def same_as_previous_search(self, regexp, direction, where, match,
        pos = None):
        
        """Determines whether a particular match found by *search_for* is the
        same as the one found by its last invocation.
        
        **INPUTS**
        
        *STR* regexp -- The regexp for current [search_for]. If
         *None*, then we are not currently doint a [search_for]
         operation.
        
        *INT* direction -- Direction of the search 
        
        *INT* where -- Put cursor at end or start of occurence
                
        *(INT, INT)* match -- Start and end position of the match
        

        **OUTPUTS**
        
        *BOOL* -- true if this is the same match as last invocation of
        *search_for*

        ..[search_for] file:///./SourceBuff.SourceBuff.html#search_for"""


        #
        # We consider this to be the same occurence as last search iif:
        #
        # a. Start/end position of both occurences are the same
        # b. Both occurences were found using same *regexp*
        # c. Both occurences were found using same *where*
        # d. Cursor hasn't moved since last search.
        #
        # Condition d. is because if the user moves the cursor to an other
        # location and redoes the same search again, he/she probably means
        # to do a new search starting from this new cursor location.
        #
        # Note that two occurences may be deemed identical even if they were
        # found using different *direction*. This is because if the user does
        # a search forward and then reverses the direction, we don't want
        # to go back to the occurence found in the forward direction (instead
        # we want to go directly to the one preceding).
        #
        # On the other hand, two occurences will be deemed identical only if
        # they were found using same *where* (condition c.). This is so you can
        # search for an occurence and then move the cursor to the beginning/end
        # of that occurence using a command like: "before that" or "after that"
        #
        answer = 0
        if self.last_search != None:
            last_regexp, last_direction, last_where, \
                last_match, last_alt_pos =  self.last_search
            trace('SourceBuff.same_as_previous_search', 
                'last reg = %s, where = %d, range = %s, pos = %s' % \
                (last_regexp, last_where, last_match, last_alt_pos))
            trace('SourceBuff.same_as_previous_search', 
                'reg = %s, where = %d, range = %s, pos = %s' % \
                (regexp, where, match, self.cur_pos()))
            if match == last_match and regexp == last_regexp and \
                    where == last_where:
                pos = last_alt_pos
                if pos is None:
                    pos = self.pos_extremity(last_match, last_where)
                if self.cur_pos() == pos:
                    answer = 1

        return answer
          
    def pos_extremity(self, range, where):
        """Returns the position of a given extremity of a range

        **INPUTS**

        *(INT, INT) range* -- Start and end position of the range

        *INT where* -- If positive, return position of end of
         range. Otherwise, return start position.

        **OUTPUTS**

        *INT* -- The approriate position
        """
        if where > 0:
            answer = range[1]
        else:
            answer = range[0]
        return answer

    def log_search(self, regexp, direction, where, match, alt_pos = None):
        """Logs the result of most recent search or selection operation, so
        that we know not to return the same match if the user repeats it
        
        **INPUTS**
        
        *STR* regexp -- Regular expreesion used for the search.
        
        *BOOL* direction -- If negative, then we were looking
         backwards. Forward if positive. If *None*, then we were doing
         a *Select Pseudocode* operation and we didn't care about
         direction.
        
        *INT* where -- If positive, then we wanted to put cursor after
         occurence. Before occurence if negative.
        
        *(INT, INT)* match -- Start and end position of the match that was
        used.

        *INT* alt_pos -- if not None, alternative position to use
        for comparisons, instead of match[where > 0]

        **OUTPUTS**
        
        *none* --
        """
        self.last_search = (regexp, direction, where, match, alt_pos)

    #
    # Callback methods. These are invoked by the external editor to notify
    # VoiceCode that certain events have taken place in the editor.
    #
    def delete_cbk(self, range):
        """External editor invokes that callback to notify VoiceCode
        of a deletion event.

        **INPUTS**
        
        (INT, INT) *range* -- Start and end pos of range to be deleted
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        self.on_change(range[0], range[1], "", 0)

    def insert_cbk(self, range, text):
        
        """External editor invokes that callback to notify VoiceCode
        of an insertion event.
    
        **INPUTS**
        
        (INT, INT) *range* -- Start and end position of text to be
        replaced by the insertion. If end of range is None, default to 
        the end of the buffer.

        STR *text* -- Text to be inserted

        **OUTPUTS**
        
        *none* -- 
        """
        
        trace('SourceBuff.insert_cbk', 'range=%s, text=\'%s\'' % (range, text))
        self.on_change(range[0], range[1], text, 0)

    def contents_cbk(self, text):
        
        """External editor invokes that callback to inform VoiceCode
        of the complete contents of the buffer.
        
        **INPUTS**
        
        STR *text* -- Up-to-date contents of the buffer

        **OUTPUTS**
        
        *none* -- 
        """
        
        trace('SourceBuff.insert_cbk', 'range=%s, text=\'%s\'' % (range, text))
        self.on_change(None, None, text, 0)

    def pos_selection_cbk(self, pos, selection, visible_range=None):
        """External editor invokes that callback to notify VoiceCode
        of a change in the current position or selection

        **INPUTS**
        
        INT *pos* -- Position the cursor was moved to.

        (INT, INT) *selection* -- Start and end position of selected text
        
        (INT, INT) *visible_range* -- Start and end position of the text 
        that is currently visible on the screen.
        
        **OUTPUTS**
        
        *none* -- 
        """
        pass

    def print_buff(self):
        """Prints lines around the cursor to STDOUT.

        This is mostly used when running regression test on an
        external editor (or the EdSim simulation editor).
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        *none* -- 
        """

        #
        # Figure out the text before/within/after the selection
        #
        selection_start, selection_end = self.get_selection()
        swapped = 0
        start_string = "<SEL_START>"
        end_string = "<SEL_END>"
        if selection_start > selection_end:
            selection_end, selection_start = selection_start, selection_end
            start_string, end_string = end_string, start_string
           
        debug.trace('SourceBuff.print_buff',
            'self.name()=%s, selection_start, selection_end = %d, %d' % \
            (self.name(), selection_start, selection_end))
        #
        # Figure out the first and last line to be printed
        #
        at_start = 0
        at_end = 0
        start = self.beginning_of_line(selection_start)
        if start == 0:
            at_start = 1
        else:
            for i in range(self.print_nlines):
                start = self.beginning_of_line(start - 1)
                if start <= 0:
                    start = 0
                    at_start = 1
                    break

# selection_end is actually the character after the end of the selection
        if selection_end == selection_start:
            end = self.end_of_line(selection_start)
        else:
            end = self.end_of_line(selection_end - 1)
        debug.trace('SourceBuff.print_buff',
            'end of current line, line # = %d %d' % \
            (end, self.line_num_of(end)))
        debug.trace('SourceBuff.print_buff',
            'char there is %s' % repr(self.get_text(end, end+1)))
        length = self.len()
        text = self.get_text()
        eol = re.compile('$|(%s)' % self.newline_regexp())
        if end == length:
            debug.trace('SourceBuff.print_buff',
                'this is end of buffer')
            at_end = 1
        else:
            for i in range(self.print_nlines):
                match = eol.search(text, end)
                check = max(match.end(), match.start() + 1)
# workaround for bizarre fact that match.end won't go beyond len(text) - 1
                debug.trace('SourceBuff.print_buff',
                    'next line is %d' % self.line_num_of(check))
                if check >= length:
                    end = length
                    at_end = 1
                    break
                end = self.end_of_line(check)
                debug.trace('SourceBuff.print_buff',
                    'its end is %d %d' % (end, self.line_num_of(end)))
                if end >= length:
                    end = length
                    at_end = 1
                    break
                debug.trace('SourceBuff.print_buff',
                    'char there is %s' % repr(text[end]))

        debug.trace('SourceBuff.print_buff',
            'selection_start, selection_end = %d, %d' % \
            (selection_start, selection_end))
        debug.trace('SourceBuff.print_buff',
            'start, end = %d, %d' % (start, end))
        before_content = self.get_text(start, selection_start)
        selection_content = self.get_text(selection_start, selection_end)
        after_content = self.get_text(selection_end, end)
        debug.trace('SourceBuff.print_buff',
            'before_content:\n%s\n' % before_content)
        debug.trace('SourceBuff.print_buff',
            'after_content:\n%s\n' % after_content)
        debug.trace('SourceBuff.print_buff',
            'selection_content:\n%s\n' % selection_content)
        
        printed = before_content
        if selection_content == '':
            printed = printed + '<CURSOR>'
        else:
            printed = printed + '<SEL_START>'
            printed = printed + selection_content
            printed = printed + '<SEL_END>'
        printed = printed + after_content


        from_line = self.line_num_of(start)
        lines_with_num = self.number_lines(printed, startnum =
            from_line)
        
        if at_start:
            sys.stdout.write("*** Start of source buffer ***\n")
        for number, line in lines_with_num:
            sys.stdout.write('%3i: %s\n' % (number, line))
        if at_end:
            sys.stdout.write("\n*** End of source buffer ***\n")
                
        return

    def lines_around_cursor(self):
        """Returns the line numbers of lines around cursor
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *(INT from_line, INT to_line)*

        *INT from_line* -- First line of the window.

        *INT to_line* -- Last line of the window.
        """

        curr_line = self.line_num_of(self.cur_pos())
        from_line = curr_line - self.print_nlines
        to_line = curr_line + self.print_nlines
        if from_line < 0:
            from_line = 0
        last_line = self.line_num_of(self.len())    
        if to_line > last_line:
            to_line = last_line    
        return from_line, to_line


    def __getitem__(self, key):
        """Get a character of the buffer using the buff[i] syntax.
        
        **INPUTS**
        
        *INT* key -- The index of the character to return
        
        **OUTPUTS**
        
        *CHAR* -- the character at position *key*
        """
        return self.contents()[key]

    def __setitem__(self, key, value):
        """Set a character of the buffer using the buff[i] syntax.
        
        **INPUTS**
        
        *INT* key -- The index of the character to return

        *STR* value -- The string to insert at position *key*

        **OUTPUTS**
        
        *none* -- 
        """
        self.insert(value, (key, key))

    def __getslice__(self, start, end):
        """Returns a slice of the buffer using the buff[start:end] syntax.
        
        **INPUTS**
        
        *INT* start, end -- The start and end indices of the slice

        **OUTPUTS**
        
        *STR* -- the slice from *start* to *end*
        """
        return self.contents()[start:end]

    def __getslice__(self, start, end, value):
        """Sets slice of the buffer using the buff[start:end] = value syntax.
        
        **INPUTS**
        
        *INT* start, end -- The start and end indices of the slice to be set

        *STR* value -- The string to be inserted in place of the slice.

        **OUTPUTS**
        
        """
        self.insert(value, (start,end))

    
class BackspaceMixIn(Object):
    """implements the backspace method by deletion.

    To use, derive a new class from this class and SourceBuff (or an
    existing subclass of SourceBuff).  Make sure that this class appears
    before SourceBuff (or its subclass) in the list of parent classes. 
    """
    def __init__(self, **args):
        self.deep_construct(BackspaceMixIn, {}, args)

    def backspace(self, n_times):
        """Delete a number of spaces before the cursor.
        
        **INPUTS**
        
        INT *n_times* -- number of characters to delete.
        
        **OUTPUTS**
        
        *none*
        """
        start, end = self.get_selection()
        if start == end:  # selection is empty
            start = start - n_times
        else:
            start = start - (n_times - 1)
        start = max(0, start)
        self.delete(range = (start, end))
        
class SourceBuffWithServices(
#    sb_mixins.WithStateService, 
    SourceBuff):
    """partial implementation of SourceBuff using sb_mixins to implement
    some methods using mixins

    Mixins included currently are:

    WithKbdService
    WithStateService*
    
    *Note: this is temporary, only until we get the diff-based state
    system implemented.  Then SourceBuffNonCached will use
    WithStateService, but most concrete subclasses of SourceBuffCached 
    will not.
    """
    
    def __init__(self, **attrs):
        self.deep_construct(SourceBuffWithServices,
                            {}, attrs
                            )
    def remove_other_references(self):
#        sb_mixins.WithStateService.remove_other_references(self)
        SourceBuff.remove_other_references(self)

