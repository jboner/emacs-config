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

"""concrete implementation of CmdPromptWithHistory using wxPython
wxTextCtrl."""

import debug
from Object import Object
from CmdPrompt import *
from wxPython.wx import *

# workaround for minor bug in wxPython 2.3.2.1 (should be fixed in later
# versions, and the workaround will still work then)
def fix_x_y(value):
    if len(value) == 3:
        return value[1], value[2]
    return value

class wxCmdPromptWithHistory(CmdPromptWithHistory):
    """concrete implementation of CmdPromptWithHistory using a single
    line wxTextCtrl.  Up and down arrows move through the history,
    and enter enters the currently edited command.

    Note: to receive notification that a command has been entered, the
    command_callback argument must be supplied to CmdPrompt.

    **INSTANCE ATTRIBUTES**

    *wxTextCtrl* text -- underlying wxTextCtrl 

    *STR* stored -- storage for partially edited command line buffer
    when we move back through the command history.

    *(INT, INT)* stored_selection -- storage for current selection in 
    partially edited command line buffer when we move back through 
    the command history.

    **CLASS ATTRIBUTES**
    
    *none* --
    """
    def __init__(self, underlying, **args):
        """
        **INPUTS**

        *wxTextCtrl* underlying -- existing wxTextCtrl.  Must have
        wxTE_PROCESS_ENTER style, and be a single line control 
        (not wxTE_MULTILINE)
        """

        self.deep_construct(wxCmdPromptWithHistory,
            {"stored": "",
            "stored_selection": (0, 0),
            "text": underlying},
            args)

        EVT_TEXT_ENTER(self.text, self.text.GetId(), self._on_command_enter)
        EVT_CHAR(self.text, self._on_command_char)

    def _on_command_char(self, key_event):
        """internal command to handle wxWindows key events

        **INPUTS**

        *wxKeyEvent* key_event -- the wxKeyEvent which triggered this
        handler.

        **OUTPUTS**

        *none*
        """
    
        code = key_event.GetKeyCode()
        if code == WXK_UP:
            new_buffer = self.previous()
            if new_buffer != None:
                self.text.SetValue(new_buffer)
                self.text.SetInsertionPointEnd()
        elif code == WXK_DOWN:
            new_buffer = self.next()
            if new_buffer != None:
                self.text.SetValue(new_buffer)
                self.text.SetInsertionPointEnd()
        else:
# ignore other keys, allowing the default wxTextCtrl processing to proceed
            key_event.Skip()

    def _on_command_enter(self, event):
        """internal command to handle wxWindows 
        wxEVT_COMMAND_TEXT_ENTER events

        **INPUTS**

        *wxEvent* event -- the event which triggered this handler.

        **OUTPUTS**

        *none*
        """
        command = self.text.GetValue()
        self.text.SetValue("")
        self._on_command(command)

    def save_edited(self):
        """store partially edited new command.

        **INPUT**

        *none*

        **OUTPUT**

        *none*
        """
        self.stored = self.text.GetValue()
        self.stored_selection = self.text.GetSelection()

    def restore_edited(self):
        """restore previously saved partially edited new command.

        **INPUT**

        *none*

        **OUTPUT**

        *none*
        """
        self.text.SetValue(self.stored)
        self.text.SetSelection(self.stored_selection[0],
            self.stored_selection[1])

    def edited(self):
        """contents of saved command line past the last command in the
        stack, if it has previously been saved. 
        Unlike restore_edited, this method returns only the string
        contents in the saved command line.
    
        **INPUT**

        *none*

        **OUTPUTS**

        *STR* -- partial command previously stored with save_edited, 
        or None if there is none.
        """
        return self.stored

    def in_progress(self):
        """contents of command line currently being edited (not the one
        stored by save_edited)
        A concrete subclass of CmdPromptWithHistory must define this method.
      
        **INPUT**

        *none*

        **OUTPUTS**

        *STR* -- partial command currently being edited
        """
        return self.text.GetValue()


class wxCmdLog(CmdLog):
    """concrete implementation of CmdLog using a wxTextCtrl.  

    **INSTANCE ATTRIBUTES**

    *wxTextCtrl* log - underlying wxTextCtrl log window

    **CLASS ATTRIBUTES**
    
    *none* --
    """
    def __init__(self, underlying_text, **args):
        """
        **INPUTS**

        *wxTextCtrl* underlying_text - underlying wxTextCtrl log window.
        Should be READ_ONLY
        """
        self.deep_construct(wxCmdLog,
                            {'log':underlying_text}, 
                            args)
  
    def write(self, string):
        """log output/message.  Must be implemented by concrete subclass

        **INPUTS**

        *STR* string -- message string to be logged (unlike
        log_command, should include internal and trailing new-lines)

        **OUTPUTS**

        *none*
        """
        self.log.AppendText(string)
#        self.log.SetScrollPos(wxVERTICAL, self.log.GetScrollRange(wxVERTICAL), 1)
        p = self.log.GetLastPosition()
        width, height = self.log.GetClientSizeTuple()
        char_height = self.log.GetCharHeight()
        line_height = height/char_height
        x, y = fix_x_y(self.log.PositionToXY(p))
        y = y- line_height
        p = self.log.XYToPosition(x, y)
        self.log.ShowPosition(p)

#        self.log.ShowPosition(self.log.GetLastPosition())

    def flush(self):
        """flush any buffered output to the command log.
        Must be implemented by concrete subclass.
        This interface allows CmdLog to be used to capture
        standard output.

        **INPUTS**

        *STR* string -- message string to be logged (unlike
        log_command, should include internal and trailing new-lines)

        **OUTPUTS**

        *none*
        """
# I don't think that wxTextCtrl is buffered, and
# write already does ShowPosition
        pass

