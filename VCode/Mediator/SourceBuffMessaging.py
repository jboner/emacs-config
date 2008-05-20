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
# (C)2001, National Research Council of Canada
#
##############################################################################

"""State information for an external source buffer connected to
VoiceCode via a messaging protocol."""

import messaging, SourceBuffCached
import AppTracker
import re
import sys
import SourceBuffWithDiffs
import sb_services
from debug import trace

class SourceBuffMessaging(SourceBuffWithDiffs.SourceBuffWithDiffs):
    
    """Class representing a source buffer connected to VoiceCode via a
    messaging protocol.

    This abstract class defines interface for manipulating buffer containing
    source code in some programming language.
    
    **INSTANCE ATTRIBUTES**

    *none* 

    Note however that the *app* attribute (defined in [SourceBuff])
    needs to be a subclass of [AppStateMessaging]

    **CLASS ATTRIBUTES**
    
    *none* --

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.py
    ..[AppStateMessaging] file:///./AppStateMessaging.AppStateMessaging.html"""
    
    def __init__(self, **attrs):
        self.init_attrs({})        
        self.deep_construct(SourceBuffMessaging,
                            {},
                            attrs, new_default = {'max_cookies': 32},
                            )

    def _file_name_from_app(self):
        """Gets from the external editor, the name of the file being
        displayed in this buffer.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        STR *name* -- 
        """
        self.app.talk_msgr.send_mess('file_name', {'buff_name': self.buff_name})
        response = self.app.talk_msgr.get_mess(expect=['file_name_resp'])
        value = messaging.messarg2str(response[1]['value'])
        if value == None:
            value = ""
           
        return value
        

    def _language_name_from_app(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        self.app.talk_msgr.send_mess('language_name',
            {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['language_name_resp'])
        return response[1]['value']

    def _get_pos_selection_from_app(self):
        """retrieves current position of cursor and the range of 
        current selection directly from the external application
        
        **INPUTS**
        
        *none*
        
        **OUTPUTS**
        
        *(INT, (INT, INT))* (pos, (start, end))
        
        pos is the offset into buffer of current cursor position
        start is the offset into the buffer of the start of the current
        selection.  end is the offset into the buffer of the character 
        following the selection (this matches Python's slice convention).
        """
        self.app.talk_msgr.send_mess('get_pos_selection',
            {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['get_pos_selection_resp'])
        value = response[1]['value']
        trace('SourceBuffMessaging._get_pos_selection_from_app',
            'value = %s' % value)
        pos = messaging.messarg2int(value['pos'])
        trace('SourceBuffMessaging._get_pos_selection_from_app',
            'pos = %d' % pos)
        selection = messaging.messarg2inttuple(value['selection'])
        trace('SourceBuffMessaging._get_pos_selection_from_app',
            'selection = %s' % repr(selection))
        return (pos, selection)

    def set_selection(self, range, cursor_at = 1):
        """sets range of current selection, in the external editor.

        Also sets the position to beginning or end of the selection.

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

        #
        # Set the selection and get updates from the editor
        #
        args = {'range': range, 'cursor_at': cursor_at,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('set_selection', args)
        response = self.app.talk_msgr.get_mess(expect=['set_selection_resp'])

        #
        # Apply the updates
        #
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0

    def _get_text_from_app(self, start = None, end = None):
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

        trace('SourceBuffMessaging._get_text_from_app', 'start=%s, end=%s' % (start, end))
        
        args = {'start': start, 'end': end,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('get_text', args)
        response = self.app.talk_msgr.get_mess(expect=['get_text_resp'])
        
        return response[1]['value']
        
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
        args = {'text': text, 'start': start, 'end': end,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('set_text', args)
        response = self.app.talk_msgr.get_mess(expect=['set_text_resp'])
        
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0

        

    def _get_visible_from_app(self):
        """ get start and end offsets of the currently visible region of
        the buffer.  End is the offset of the first character not
        visible (matching Python's slice convention)

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* (start, end)
        """
        self.app.talk_msgr.send_mess('get_visible',
            {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['get_visible_resp'])
        return messaging.messarg2inttuple(response[1]['value'])
        

    def make_position_visible(self):
        """scroll buffer (if necessary) so that the current position
        is visible

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """

        args = {'buff_name': self.name()}
        self.app.talk_msgr.send_mess('make_position_visible', args)
        response = self.app.talk_msgr.get_mess(expect=['make_position_visible_resp'])

#        self.app.update_response = 1
#        self.app.apply_upd_descr(response[1]['updates'])
#        self.app.update_response = 0
        
    def line_num_of(self, position = None):
        """
        Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.  (defaults to the current position)
        
        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        
        args = {'position': position, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('line_num_of', args)
        response = self.app.talk_msgr.get_mess(expect=['line_num_of_resp'])

        return messaging.messarg2int(response[1]['value'])

    def _len_from_app(self):
        """return length of buffer in characters.

        **INPUTS**

        *none*

        **OUTPUTS**

        *INT* length 
        """
        self.app.talk_msgr.send_mess('len', {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['len_resp'])

        return messaging.messarg2int(response[1]['value'])
        

    def _newline_conventions_from_app(self):
        
        """Returns a list of the forms of newline the editor can
        recognise for this buffer (read directly from editor).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        self.app.talk_msgr.send_mess('newline_conventions',
            {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['newline_conventions_resp'])
        return response[1]['value']

    def _pref_newline_convention_from_app(self):
        
        """Returns the form of newline that the editor prefers for
        this buffer (read directly from editor).
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        self.app.talk_msgr.send_mess('pref_newline_convention',
            {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['pref_newline_convention_resp'])
        return response[1]['value']

    def beginning_of_line(self, pos = None):
        """Returns the position of the beginning of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the beginning of line.
        

        **OUTPUTS**
        
        *INT* beg_pos -- Position of the beginning of the line
        """
        args = {'pos': pos, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('beginning_of_line', args)
        response = \
            self.app.talk_msgr.get_mess(expect=['beginning_of_line_resp'])

        return messaging.messarg2int(response[1]['value'])
        


    def end_of_line(self, pos = None):
        """Returns the position of the end of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the end of line.
        

        **OUTPUTS**
        
        *INT* end_pos -- Position of the end of the line
        """
        args = {'pos': pos, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('end_of_line', args)
        response = \
            self.app.talk_msgr.get_mess(expect=['end_of_line_resp'])

        return messaging.messarg2int(response[1]['value'])

    def move_relative_page(self, direction=1, num=1):
        """Moves up or down a certain number of pages
        
        **INPUTS**
        
        *INT* direction=1 -- If positive, page down. If negative, page up.
        
        *INT* num=1 -- Number of pages to move.
        

        **OUTPUTS**
        
        *none* -- 
        """
        trace('SourceBuffMessaging.move_relative_page', 'direction=%s, num=%s' % (direction, num))
        args = {'direction': direction, 'num': num,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('move_relative_page', args)
        response = self.app.talk_msgr.get_mess(expect=['move_relative_page_resp'])
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0

    def indent(self, range = None):
        
        """Automatically indent the code in a source buffer region. Indentation
        of each line is determined automatically based on the line's context.

        **INPUTS**

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """
# by default, assume that the remote editor does indentation.
# Subclasses for particular editors which use mediator-based indentation 
# can always override this choice.
        args = {'range': range, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('indent', args)
        response = self.app.talk_msgr.get_mess(expect=['indent_resp'])        

        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        
    def incr_indent_level(self, levels=1, range=None):
        
        """Increase the indentation of a region of code by a certain
        number of levels.
        
        **INPUTS**
        
        *INT* levels=1 -- Number of levels to indent by.
        
        *(INT, INT)* range=None -- Region of code to be indented 
        

        **OUTPUTS**
        
        *none* -- 
        """
# by default, assume that the remote editor does indentation.
# Subclasses for particular editors which use mediator-based indentation 
# can always override this choice.
        args = {'levels': levels, 'range': range, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('incr_indent_level', args)
        response = self.app.talk_msgr.get_mess(expect=['incr_indent_level_resp'])        
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        
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

# by default, assume that the remote editor does indentation.
# Subclasses for particular editors which use mediator-based indentation 
# can always override this choice.
        args = {'levels': levels, 'range': range, 'buff_name': self.name()}
        self.app.talk_msgr.send_mess('decr_indent_level', args)
        response = self.app.talk_msgr.get_mess(expect=['decr_indent_level_resp'])        
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        


    def insert(self, text, range = None):
        
        """Ask external editor to replace text in range with with text

        **INPUTS**

        *STR text* -- new text

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *AppTracker.TextBlock* -- a block representing the text
        inserted including any surrounding whitespace which was inserted,
        and the final range occupied by the text inserted
        (which is normally not the same as the range replaced), or
        None if the original text could not be matched to the text
        insertion reported by the editor.
        """

        trace('SourceBuffMessaging.insert', 'text=%s, range=%s, self.name()=%s' % (text, range, self.name()))
        
        args = {'text': text, 'range': range,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('insert', args)
        response = self.app.talk_msgr.get_mess(expect=['insert_resp'])        
        self.app.update_response = 1
        updates = self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        buff_tracker = AppTracker.BuffInsertionTracker(buff_name =
            self.name())
        tracker = AppTracker.SingleBuffTracker(buff_name =
            self.name(), tracked_buffer = buff_tracker)
        for update in updates:
            update.apply(tracker)
        block = buff_tracker.block_containing(self.cur_pos())
        inserted = block.text[:(self.cur_pos() - block.start())]
        loose = AppTracker.LooseMatch()
        s = r'\s*' +  loose.expr(text) + r'\s*$'
        found = re.search(s, inserted)
        if found:
            return AppTracker.TextBlock(found.group(),
                                        found.start() + block.start())
        return None
    
    def delete(self, range = None):
        """Delete text in a source buffer range.

        **INPUTS**

        *(INT, INT)* range -- code range to be deleted.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """
        #
        # Ask external editor to delete the region
        #
        args = {'range': range,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('delete', args)
        response = self.app.talk_msgr.get_mess(expect=['delete_resp'])

        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])        
        self.app.update_response = 0
        
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
        args = {'n_times': n_times,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('backspace', args)
        response = self.app.talk_msgr.get_mess(expect=['backspace_resp'])

        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])        
        self.app.update_response = 0
        
    def copy_selection(self):
        """Copy the selected text"""
        trace('SourceBuffMessaging.copy_selection', '** invoked on buffer %s' % self.name())
        self.app.talk_msgr.send_mess('copy_selection', {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['copy_selection_resp'])
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0            
        
    def cut_selection(self):
        """Cut the selected text"""
        self.app.talk_msgr.send_mess('cut_selection', {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['cut_selection_resp'])
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0        
        
    def paste(self):
        """Paste content of clipboard into current buffer"""
        trace('SourceBuffMessaging.paste', '** invoked')
        self.app.talk_msgr.send_mess('paste', {'buff_name': self.name()})
        response = self.app.talk_msgr.get_mess(expect=['paste_resp'])
        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0        
        

       
    def goto(self, pos):

        """Moves the cursor to position *INT pos* of source buffer
        (and make selection empty)
        """

        #
        # Ask external editor to delete the region
        #
        args = {'pos': pos,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('goto', args)
        response = self.app.talk_msgr.get_mess(expect=['goto_resp'])

        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        
    def goto_line(self, linenum, where=-1):
        """Go to a particular line in a buffer.

        *INT linenum* is the line number.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.
        """
        args = {'linenum': linenum, 'where': where,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('goto_line', args)
        response = self.app.talk_msgr.get_mess(expect=['goto_line_resp'])

        self.app.update_response = 1
        self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
      
class SourceBuffInsertIndentMess(SourceBuffMessaging):
    """subclass of SourceBuffMessaging which sends an insert_indent
    message to the external editor, instead of using the generic
    SourceBuff implementation of insert_indent in terms of insert and
    indent.
    
    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args):
        self.deep_construct(SourceBuffInsertIndentMess, {}, args)

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
        
        trace('SourceBuffInsertIndentMess.insert_indent', '** invoked')
        
# by default, assume that the remote editor does indentation.
# Subclasses for particular editors which use mediator-based indentation 
# can always override this choice.
        args = {'code_bef': code_bef, 'code_after': code_after, 'range': range,
            'buff_name': self.name()}
        self.app.talk_msgr.send_mess('insert_indent', args)
        response = self.app.talk_msgr.get_mess(expect=['insert_indent_resp'])        
        trace('SourceBuffInsertIndentMess.insert_indent',
            'updates = %s' % response[1]['updates'])
        self.app.update_response = 1
        updates = self.app.apply_upd_descr(response[1]['updates'])
        self.app.update_response = 0
        buff_tracker = AppTracker.BuffInsertionTracker(buff_name =
            self.name())
        tracker = AppTracker.SingleBuffTracker(buff_name =
            self.name(), tracked_buffer = buff_tracker)
        for update in updates:
            update.apply(tracker)
#            sys.stderr.write('after update, tracker shows\n')
#            sys.stderr.writelines(buff_tracker.show())
        block = buff_tracker.block_containing(self.cur_pos())
        if block:
            inserted = block.text[:(self.cur_pos() - block.start())]
            appended = block.text[(self.cur_pos() - block.start()):]
            loose = AppTracker.LooseMatch()
            appended_text = None
            inserted_text = None
            if code_bef:
                s = r'\s*' +  loose.expr(code_bef) + r'\s*$'
                found = re.search(s, inserted)
                if found:
                    inserted_text = AppTracker.TextBlock(found.group(),
                                                found.start() + block.start())
            if code_after:
                s = r'\s*' +  loose.expr(code_after) + r'\s*'
                found = re.match(s, appended)
                if found:
                    appended_text = AppTracker.TextBlock(found.group(),
                                                self.cur_pos())
            return inserted_text, appended_text
        elif code_bef or code_after:
#            sys.stderr.write('WARNING - BLOCK NOT FOUND\n')
#            sys.stderr.write('code_bef = %s, code_after = %s' %
#                (repr(code_bef), repr(code_after)))
#            sys.stderr.write('pos = %d\n' % self.cur_pos())
#            sys.stderr.write('blocks:\n')
#            sys.stderr.writelines(buff_tracker.show())
            return None, None


        
